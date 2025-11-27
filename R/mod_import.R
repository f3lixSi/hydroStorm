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
      verbatimTextOutput(ns("info")),
      plotOutput(ns("preview"), height = 280)
    ),
    column(
      width = 6,
      h3("Räumliche Vorschau"),
      leaflet::leafletOutput(ns("map_preview"), height = 500)
    )
  )
}

importServer <- function(input, output, session, shared) {
  ns <- session$ns
  
  # Basiskarte beim Start
  output$map_preview <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::setView(lng = 10, lat = 51, zoom = 5)
  })
  
  # Zeitraum nach Upload der RADKLIM-Datei(en) bestimmen
  observeEvent(input$raster, {
    req(input$raster)
    output$info <- renderText("📄 RADKLIM-Datei(en) werden analysiert …")
    
    first_path <- input$raster$datapath[1]
    r_tmp <- tryCatch(read_radklim_nc(first_path), error = function(e) e)
    if (inherits(r_tmp, "error")) {
      output$info <- renderText(paste("❌ Fehler beim Einlesen:", r_tmp$message))
      return(NULL)
    }
    
    times <- attr(r_tmp, "hydrostorm_time")
    if (!is.null(times) && length(times) > 1) {
      minD <- as.Date(min(times, na.rm = TRUE))
      maxD <- as.Date(max(times, na.rm = TRUE))
      
      updateDateRangeInput(
        session, "daterange",
        start = minD,
        end   = maxD,
        min   = minD,
        max   = maxD
      )
      
      output$info <- renderText(sprintf(
        "✅ RADKLIM erkannt: %s …\nZeitraum (erste Datei): %s – %s (%d Layer)",
        paste(basename(input$raster$name), collapse = ", "),
        format(minD), format(maxD),
        terra::nlyr(r_tmp)
      ))
    } else {
      output$info <- renderText("⚠️ Konnte keine Zeitinformation erkennen.")
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
    
    withProgress(message = "📡 Importiere Daten …", value = 0, {
      incProgress(0.1, detail = "Lese RADKLIM-Datei(en) …")
      
      rad_paths <- input$raster$datapath
      r_list <- lapply(rad_paths, function(p) {
        tryCatch(read_radklim_nc(p), error = function(e) e)
      })
      if (any(vapply(r_list, inherits, logical(1), "error"))) {
        err <- r_list[[which(vapply(r_list, inherits, logical(1), "error"))[1]]]
        output$info <- renderText(paste("❌ Fehler RADKLIM:", err$message))
        return(NULL)
      }
      
      r <- do.call(c, r_list)
      times   <- attr(r, "hydrostorm_time")
      product <- attr(r, "hydrostorm_product")
      dt_min  <- attr(r, "hydrostorm_dt_min")
      
      t_start <- as.POSIXct(input$daterange[1], tz = "UTC")
      t_end   <- as.POSIXct(input$daterange[2], tz = "UTC") + 86399
      sel <- which(times >= t_start & times <= t_end)
      if (length(sel) == 0) {
        output$info <- renderText("❌ Kein Layer im gewählten Zeitraum gefunden.")
        return(NULL)
      }
      r     <- r[[sel]]
      times <- times[sel]
      
      incProgress(0.4, detail = "Auswertungsgebiet vorbereiten …")
      
      vect_radklim <- NULL
      vect_25832   <- NULL
      
      # 1) Shape-Modus
      if (input$area_mode == "shape") {
        req(input$mask_shape)
        
        shp_files <- input$mask_shape
        shp_idx   <- grep("\\.shp$", shp_files$name, ignore.case = TRUE)
        if (length(shp_idx) != 1) {
          output$info <- renderText("❌ Es muss genau eine .shp-Datei ausgewählt sein.")
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
          output$info <- renderText("❌ Konnte shape.shp nicht erzeugen – fehlen .shp/.dbf/.shx/.prj?")
          return(NULL)
        }
        
        vect_orig <- tryCatch(terra::vect(shp_path), error = function(e) e)
        if (inherits(vect_orig, "error")) {
          output$info <- renderText(paste("❌ Fehler beim Laden des Shapes:", vect_orig$message))
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
        req(shared$pt_ll)
        
        pt_r <- tryCatch(
          sf::st_transform(shared$pt_ll, terra::crs(r)),
          error = function(e) NULL
        )
        if (is.null(pt_r)) {
          output$info <- renderText("❌ Konnte Punkt nicht in Raster-Koordinatensystem transformieren.")
          return(NULL)
        }
        
        buf_r <- sf::st_buffer(pt_r, dist = 1000)  # 1 km Radius
        vect_radklim <- tryCatch(terra::vect(buf_r), error = function(e) NULL)
        if (is.null(vect_radklim)) {
          output$info <- renderText("❌ Konnte aus Punkt kein Auswerte-Polygon erzeugen.")
          return(NULL)
        }
        
        # Für KOSTRA reicht der Punkt in 25832
        vect_25832 <- tryCatch(
          terra::vect(sf::st_transform(shared$pt_ll, 25832)),
          error = function(e) NULL
        )
      }
      
      incProgress(0.6, detail = "Leaflet-Vorschau aktualisieren …")
      
      proxy <- leaflet::leafletProxy(ns("map_preview"), session = session) |>
        leaflet::clearShapes() |>
        leaflet::clearMarkers()
      
      if (input$area_mode == "shape" && !is.null(vect_25832)) {
        shp_ll <- tryCatch(
          terra::project(vect_25832, "EPSG:4326"),
          error = function(e) NULL
        )
        if (!is.null(shp_ll)) {
          shp_sf <- sf::st_as_sf(shp_ll)
          proxy |>
            leaflet::addPolygons(
              data  = shp_sf,
              weight = 2,
              color  = "#004E7C",
              fill   = FALSE
            )
        }
      } else if (input$area_mode %in% c("point", "address") && !is.null(shared$pt_ll)) {
        coords <- sf::st_coordinates(shared$pt_ll)
        proxy |>
          leaflet::addMarkers(lng = coords[1], lat = coords[2])
      }
      
      incProgress(0.8, detail = "Metadaten speichern …")
      
      shared$rast         <- r
      shared$shape        <- vect_radklim
      shared$shape_25832  <- vect_25832
      shared$times        <- times
      shared$product      <- product
      shared$dt_min       <- dt_min
      shared$radklim_path <- rad_paths[1]
      
      output$info <- renderText(sprintf(
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
    })
  })
  
  # Raster-Vorschau
  output$preview <- renderPlot({
    req(shared$rast)
    r_plot <- shared$rast[[1]]
    p <- tidyterra::autoplot(r_plot) +
      theme_minimal() +
      ggtitle("RADKLIM (1. Layer, Original-KBS)") +
      theme(plot.title = element_text(face = "bold"))
    if (!is.null(shared$shape)) {
      p <- p +
        tidyterra::geom_spatvector(
          data = shared$shape,
          fill = NA,
          color = "red",
          linewidth = 1
        )
    }
    p
  })
}