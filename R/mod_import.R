# ─────────────────────────────────────────────
# Modul: Import (RADKLIM + Shape)
# ─────────────────────────────────────────────

importUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 6,
      h3("Datenimport (RADKLIM)"),
      fileInput(ns("raster"), "RADKLIM-Datei(en) (.nc)", multiple = TRUE, accept = ".nc"),
      dateRangeInput(
        ns("daterange"),
        "Zeitraum auswählen:",
        start = Sys.Date() - 2,
        end   = Sys.Date(),
        min   = as.Date("2001-01-01"),
        max   = Sys.Date(),
        format = "yyyy-mm-dd"
      ),
      radioButtons(
        ns("area_mode"),
        "Auswertungsgebiet wählen:",
        choices = c(
          "Shape (Polygon) einladen" = "shape",
          "Punkt in Karte setzen"    = "point",
          "Adresse eingeben"         = "address"
        ),
        selected = "shape"
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] === 'shape'", ns("area_mode")),
        fileInput(
          ns("mask_shape"),
          "Clip-Gebiet (Shapefile, mehrere Dateien)",
          multiple = TRUE,
          accept = c(".shp", ".dbf", ".shx", ".prj")
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] === 'address'", ns("area_mode")),
        textInput(
          ns("address"),
          "Adresse (Straße, Ort):",
          placeholder = "z. B. Am Hochschulcampus 1, 44801 Bochum"
        ),
        actionButton(
          ns("geocode_addr"),
          "Adresse suchen",
          icon = icon("search"),
          class = "btn-secondary"
        )
      ),
      
      actionButton(ns("load"), "Daten laden", icon = icon("play"), class = "btn-primary"),
      tags$hr(),
      verbatimTextOutput(ns("info"))
    ),
    column(
      width = 6,
      h3("Räumliche Vorschau"),
      helpText("Nach dem Laden wird die Niederschlagssumme halbtransparent über die OpenStreetMap-Karte gelegt."),
      leaflet::leafletOutput(ns("map_preview"), height = 560)
    )
  )
}

importServer <- function(input, output, session, shared) {
  ns <- session$ns

  # Statusmeldung zentral halten und EINMALIG rendern.
  # (Outputs nicht in jedem Handler neu zuweisen – das blieb in neueren
  #  Shiny-Versionen im "recalculating"-Zustand hängen.)
  info_msg <- reactiveVal("")
  output$info <- renderText(info_msg())

  # Basiskarte beim Start
  output$map_preview <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(lng = 10, lat = 51, zoom = 5)
  })
  
  # Zeitraum nach Upload der RADKLIM-Datei(en) bestimmen — über ALLE Dateien,
  # damit auch Monats-/Jahreswechsel (mehrere Dateien) den vollen Zeitraum ergeben.
  observeEvent(input$raster, {
    req(input$raster)
    info_msg("📄 RADKLIM-Datei(en) werden analysiert …")

    n_files    <- nrow(input$raster)
    times_list <- lapply(seq_len(n_files), function(i) {
      tryCatch(
        radklim_time_axis(input$raster$datapath[i], fname = input$raster$name[i]),
        error = function(e) e
      )
    })

    errs <- vapply(times_list, inherits, logical(1), "error")
    if (any(errs)) {
      info_msg(paste("❌ Fehler beim Einlesen:", times_list[[which(errs)[1]]]$message))
      return(NULL)
    }

    times <- do.call(c, times_list)
    times <- times[!is.na(times)]

    if (length(times) > 0) {
      minD <- as.Date(min(times))
      maxD <- as.Date(max(times))

      updateDateRangeInput(
        session, "daterange",
        start = minD,
        end   = maxD,
        min   = minD,
        max   = maxD
      )

      info_msg(sprintf(
        "✅ RADKLIM erkannt: %d Datei(en): %s\nGesamtzeitraum: %s – %s (%d Layer)",
        n_files,
        paste(basename(input$raster$name), collapse = ", "),
        format(minD), format(maxD),
        length(times)
      ))
    } else {
      info_msg("⚠️ Konnte keine Zeitinformation erkennen.")
    }
  })
  
  # Kartenklick im Punkt-Modus
  observeEvent(input$map_preview_click, {
    req(input$area_mode == "point")
    click <- input$map_preview_click
    req(!is.null(click$lat), !is.null(click$lng))
    
    pt_ll_sf <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
    pt_ll    <- sf::st_as_sf(pt_ll_sf)
    pt_25832 <- tryCatch(sf::st_transform(pt_ll, 25832), error = function(e) NULL)
    
    shared$pt_ll    <- pt_ll
    shared$pt_25832 <- pt_25832
    
    leaflet::leafletProxy(ns("map_preview"), session = session) |>
      leaflet::clearMarkers() |>
      leaflet::addMarkers(lng = click$lng, lat = click$lat)
  })
  
  # Adresse geokodieren
  observeEvent(input$geocode_addr, {
    req(input$area_mode == "address")
    addr <- trimws(input$address)
    
    if (!nzchar(addr)) {
      showNotification("Bitte zuerst eine Adresse eingeben.", type = "warning")
      return(NULL)
    }
    
    # tidygeocoder braucht ein Dataframe/Tibble
    addr_df <- tibble::tibble(address = addr)
    
    res <- tryCatch(
      tidygeocoder::geocode(
        addr_df,
        address      = address,
        method       = "osm",
        full_results = FALSE,
        limit        = 1
      ),
      error = function(e) {
        message("Geocoding-Fehler: ", e$message)
        NULL
      }
    )
    
    # Prüfen, ob etwas Sinnvolles zurückkam
    if (is.null(res) || nrow(res) == 0 ||
        !all(c("lat", "long") %in% names(res)) ||
        is.na(res$lat[1]) || is.na(res$long[1])) {
      showNotification("Adresse konnte nicht geokodiert werden.", type = "error")
      return(NULL)
    }
    
    lon <- res$long[1]
    lat <- res$lat[1]
    
    pt_ll_sf <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
    pt_ll    <- sf::st_as_sf(pt_ll_sf)
    pt_25832 <- tryCatch(sf::st_transform(pt_ll, 25832), error = function(e) NULL)
    
    shared$pt_ll    <- pt_ll
    shared$pt_25832 <- pt_25832
    
    leaflet::leafletProxy(ns("map_preview"), session = session) |>
      leaflet::clearMarkers() |>
      leaflet::addMarkers(lng = lon, lat = lat)
    
    showNotification("Adresse erfolgreich geokodiert.", type = "message")
  })
  
  # Daten laden
  observeEvent(input$load, {
    req(input$raster)
    
    tryCatch(
    withProgress(message = "📡 Importiere Daten …", value = 0, {
      incProgress(0.1, detail = "Lese RADKLIM-Datei(en) …")
      
      rad_paths <- input$raster$datapath
      rad_names <- input$raster$name
      r_list <- lapply(seq_along(rad_paths), function(i) {
        tryCatch(read_radklim_nc(rad_paths[i], fname = rad_names[i]), error = function(e) e)
      })
      if (any(vapply(r_list, inherits, logical(1), "error"))) {
        err <- r_list[[which(vapply(r_list, inherits, logical(1), "error"))[1]]]
        info_msg(paste("❌ Fehler RADKLIM:", err$message))
        return(NULL)
      }
      
      # Attribute aus den Einzel-Reads sichern, BEVOR c() sie verwirft
      times    <- do.call(c, lapply(r_list, attr, which = "hydrostorm_time"))
      products <- vapply(r_list, function(x) attr(x, "hydrostorm_product"), character(1))
      if (length(unique(products)) > 1) {
        info_msg("❌ Bitte nicht YW- und RW-Dateien mischen.")
        return(NULL)
      }
      product <- products[1]
      dt_min  <- attr(r_list[[1]], "hydrostorm_dt_min")
      r <- do.call(c, r_list)

      # Layer chronologisch sortieren — die Upload-Reihenfolge der Dateien ist
      # nicht garantiert (z. B. Monats-/Jahreswechsel: Januar vor Dezember gewählt)
      ord   <- order(times)
      r     <- r[[ord]]
      times <- times[ord]

      t_start <- as.POSIXct(input$daterange[1], tz = "UTC")
      t_end   <- as.POSIXct(input$daterange[2], tz = "UTC") + 86399
      sel <- which(times >= t_start & times <= t_end)
      if (length(sel) == 0) {
        info_msg("❌ Kein Layer im gewählten Zeitraum gefunden.")
        return(NULL)
      }
      r     <- r[[sel]]
      times <- times[sel]
      
      incProgress(0.4, detail = "Auswertungsgebiet vorbereiten …")
      
      vect_radklim <- NULL
      vect_25832   <- NULL
      
      # 1) Shape-Modus
      if (input$area_mode == "shape") {
        if (is.null(input$mask_shape)) {
          info_msg(
            "❌ Kein Shapefile geladen. Bitte im Modus 'Shape' die vier Dateien (.shp, .dbf, .shx, .prj) gemeinsam hochladen."
          )
          showNotification("Bitte zuerst ein Shapefile hochladen (.shp/.dbf/.shx/.prj).", type = "warning")
          return(NULL)
        }

        shp_files <- input$mask_shape
        shp_idx   <- grep("\\.shp$", shp_files$name, ignore.case = TRUE)
        if (length(shp_idx) != 1) {
          info_msg("❌ Es muss genau eine .shp-Datei ausgewählt sein.")
          return(NULL)
        }
        
        tmpdir <- tempfile("shape_")
        dir.create(tmpdir, recursive = TRUE)
        base <- tools::file_path_sans_ext(shp_files$name[shp_idx])
        exts <- c(".shp", ".dbf", ".shx", ".prj")
        
        for (ext in exts) {
          hit <- which(
            tools::file_path_sans_ext(shp_files$name) == base &
              grepl(ext, shp_files$name, ignore.case = TRUE)
          )
          if (length(hit) == 1) {
            file.copy(
              from = shp_files$datapath[hit],
              to   = file.path(tmpdir, paste0("shape", ext)),
              overwrite = TRUE
            )
          }
        }
        
        shp_path <- file.path(tmpdir, "shape.shp")
        if (!file.exists(shp_path)) {
          info_msg("❌ Konnte shape.shp nicht erzeugen – fehlen .shp/.dbf/.shx/.prj?")
          return(NULL)
        }
        
        vect_orig <- tryCatch(terra::vect(shp_path), error = function(e) e)
        if (inherits(vect_orig, "error")) {
          info_msg(paste("❌ Fehler beim Laden des Shapes:", vect_orig$message))
          return(NULL)
        }
        
        vect_radklim <- tryCatch(
          terra::project(vect_orig, terra::crs(r)),
          error = function(e) vect_orig
        )
        
        vect_25832 <- tryCatch(
          terra::project(vect_orig, "EPSG:25832"),
          error = function(e) NULL
        )
      }
      
      # 2) Punkt- oder Adress-Modus: aus Punkt ein kleines Polygon erzeugen
      if (input$area_mode %in% c("point", "address")) {
        if (is.null(shared$pt_ll)) {
          msg <- if (input$area_mode == "point") {
            "❌ Kein Punkt gesetzt. Bitte den Modus 'Punkt in Karte setzen' wählen und in die Karte klicken (es muss ein Marker erscheinen)."
          } else {
            "❌ Keine Adresse gefunden. Bitte eine Adresse eingeben und 'Adresse suchen' klicken."
          }
          info_msg(msg)
          showNotification(msg, type = "warning")
          return(NULL)
        }

        pt_r <- tryCatch(
          sf::st_transform(shared$pt_ll, terra::crs(r)),
          error = function(e) NULL
        )
        if (is.null(pt_r)) {
          info_msg("❌ Konnte Punkt nicht in Raster-Koordinatensystem transformieren.")
          return(NULL)
        }
        
        buf_r <- sf::st_buffer(pt_r, dist = 1000)  # 1 km Radius
        vect_radklim <- tryCatch(terra::vect(buf_r), error = function(e) NULL)
        if (is.null(vect_radklim)) {
          info_msg("❌ Konnte aus Punkt kein Auswerte-Polygon erzeugen.")
          return(NULL)
        }
        
        # Für KOSTRA reicht der Punkt in 25832
        vect_25832 <- tryCatch(
          terra::vect(sf::st_transform(shared$pt_ll, 25832)),
          error = function(e) NULL
        )
      }
      
      incProgress(0.6, detail = "Karte aktualisieren …")

      # Raster EINMAL auf das Gebiet (+5 km) zuschneiden und für alles Weitere
      # verwenden (Karten-Overlay UND Verarbeitung). Das volle Deutschland-
      # Raster über viele Tage/Monate sprengt auf Servern mit wenig RAM
      # (z. B. Posit Connect Cloud) den Speicher – der Zuschnitt reduziert die
      # Datenmenge um Größenordnungen.
      r_crop <- tryCatch(
        terra::crop(r, terra::ext(vect_radklim) + 5000),
        error = function(e) NULL
      )

      # Niederschlagssumme über den Zeitraum, nach EPSG:4326 projiziert –
      # für das halbtransparente Karten-Overlay.
      r_sum_ll <- if (!is.null(r_crop)) {
        tryCatch(
          terra::project(sum(r_crop, na.rm = TRUE), "EPSG:4326"),
          error = function(e) NULL
        )
      } else {
        NULL
      }

      proxy <- leaflet::leafletProxy(ns("map_preview"), session = session) |>
        leaflet::clearImages() |>
        leaflet::clearControls() |>
        leaflet::clearShapes() |>
        leaflet::clearMarkers()

      # Raster halbtransparent über OpenStreetMap legen
      if (!is.null(r_sum_ll)) {
        rng <- range(terra::values(r_sum_ll, mat = FALSE), na.rm = TRUE)
        if (all(is.finite(rng)) && diff(rng) > 0) {
          pal <- leaflet::colorNumeric("Blues", domain = rng, na.color = "transparent")
          proxy <- proxy |>
            leaflet::addRasterImage(r_sum_ll, colors = pal, opacity = 0.7, project = TRUE) |>
            leaflet::addLegend(
              pal = pal, values = rng,
              title = "Niederschlag<br>Summe [mm]", position = "bottomright"
            )
        }
      }

      # Auswertungsgebiet bzw. Punkt darüber zeichnen + auf das Gebiet zoomen
      bb <- NULL
      if (input$area_mode == "shape" && !is.null(vect_25832)) {
        shp_ll <- tryCatch(terra::project(vect_25832, "EPSG:4326"), error = function(e) NULL)
        if (!is.null(shp_ll)) {
          shp_sf <- sf::st_as_sf(shp_ll)
          proxy  <- proxy |>
            leaflet::addPolygons(data = shp_sf, weight = 2, color = "#004E7C", fill = FALSE)
          bb <- as.numeric(sf::st_bbox(shp_sf))
        }
      } else if (input$area_mode %in% c("point", "address") && !is.null(shared$pt_ll)) {
        coords <- sf::st_coordinates(shared$pt_ll)
        proxy  <- proxy |>
          leaflet::addMarkers(lng = coords[1, 1], lat = coords[1, 2])
      }

      if (!is.null(bb) && all(is.finite(bb))) {
        proxy |> leaflet::fitBounds(bb[1], bb[2], bb[3], bb[4])
      } else if (!is.null(shared$pt_ll)) {
        coords <- sf::st_coordinates(shared$pt_ll)
        proxy |> leaflet::setView(lng = coords[1, 1], lat = coords[1, 2], zoom = 12)
      }

      incProgress(0.8, detail = "Metadaten speichern …")

      # Ortsbezeichnung für die Plots bestimmen (Adresse, sonst Reverse-Geocoding)
      shared$location_label <- tryCatch(
        determine_location_label(
          area_mode  = input$area_mode,
          address    = input$address,
          pt_ll      = shared$pt_ll,
          vect_25832 = vect_25832
        ),
        error = function(e) NA_character_
      )

      # Zugeschnittenes Raster speichern (Fallback: Vollraster, falls Crop scheiterte)
      shared$rast         <- if (!is.null(r_crop)) r_crop else r
      shared$shape        <- vect_radklim
      shared$shape_25832  <- vect_25832
      shared$times        <- times
      shared$product      <- product
      shared$dt_min       <- dt_min
      shared$radklim_path <- rad_paths[1]
      shared$area_mode    <- input$area_mode
      
      info_msg(sprintf(
        "✅ RADKLIM geladen: %d Layer (%s – %s)\nAuswertungsmodus: %s",
        terra::nlyr(r),
        format(min(times)), format(max(times)),
        switch(
          input$area_mode,
          "shape"   = "Shape",
          "point"   = "Punkt",
          "address" = "Adresse"
        )
      ))
      
      incProgress(1, detail = "Fertig!")
    }),
    error = function(e) {
      msg <- paste("❌ Fehler beim Laden:", conditionMessage(e))
      info_msg(msg)
      showNotification(msg, type = "error", duration = 10)
      message(msg)
    })
  })
}
