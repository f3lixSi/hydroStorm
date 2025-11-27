# ─────────────────────────────────────────────
# Modul: Import (RADKLIM + Shape)
# ─────────────────────────────────────────────

importUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Datenimport (RADKLIM)"),
    fileInput(
      ns("raster"),
      "RADKLIM-Datei (.nc)",
      accept = ".nc",
      multiple = TRUE
    ),
    dateRangeInput(
      ns("daterange"),
      "Zeitraum auswählen:",
      start = Sys.Date() - 2,
      end   = Sys.Date(),
      min   = as.Date("2001-01-01"),
      max   = Sys.Date(),
      format = "yyyy-mm-dd"
    ),
    fileInput(
      ns("mask_shape"),
      "Clip-Gebiet (Shapefile – alle zugehörigen Dateien auswählen)",
      multiple = TRUE,
      accept = c(".shp", ".dbf", ".shx", ".prj")
    ),
    actionButton(
      ns("load"),
      "Daten laden",
      icon = icon("play"),
      class = "btn-primary"
    ),
    tags$hr(),
    verbatimTextOutput(ns("info")),
    plotOutput(ns("preview"), height = 280)
  )
}

importServer <- function(input, output, session, shared) {
  ns <- session$ns
  
  # 1) Nach Upload: Zeitraum automatisch erkennen
  observeEvent(input$raster, {
    req(input$raster)
    output$info <- renderText("📄 Datei wird analysiert …")
    
    r <- tryCatch(
      read_radklim_nc(
        path  = input$raster$datapath,
        fname = input$raster$name
      ),
      error = function(e) e
    )
    if (inherits(r, "error")) {
      output$info <- renderText(paste("❌ Fehler beim Einlesen:", r$message))
      return(NULL)
    }
    
    times <- attr(r, "hydrostorm_time")
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
        "✅ Datei erkannt: %s\nZeitraum: %s – %s (%d Layer)",
        basename(input$raster$name),
        format(minD), format(maxD),
        terra::nlyr(r)
      ))
    } else {
      output$info <- renderText("⚠️ Konnte keine Zeitinformation erkennen.")
    }
  })
  
  # 2) Nach Klick auf "Daten laden"
  observeEvent(input$load, {
    req(input$raster, input$mask_shape, input$daterange)
    
    withProgress(message = "📡 Importiere Daten …", value = 0, {
      incProgress(0.1, detail = "RADKLIM einlesen …")
      
      # RADKLIM mit Zeitfilter einlesen
      r <- tryCatch(
        read_radklim_nc(
          path    = input$raster$datapath,
          fname   = input$raster$name,
          t_start = input$daterange[1],
          t_end   = input$daterange[2]
        ),
        error = function(e) e
      )
      if (inherits(r, "error")) {
        output$info <- renderText(paste("❌ Fehler RADKLIM:", r$message))
        return(NULL)
      }
      
      times   <- attr(r, "hydrostorm_time")
      product <- attr(r, "hydrostorm_product")
      dt_min  <- attr(r, "hydrostorm_dt_min")
      
      incProgress(0.4, detail = "Shape-Dateien prüfen …")
      
      # Shape: mehrere Dateien (.shp, .dbf, .shx, .prj) hochgeladen
      shp_files <- input$mask_shape
      
      # .shp-Eintrag finden
      shp_idx <- grep("\\.shp$", shp_files$name, ignore.case = TRUE)
      if (length(shp_idx) != 1) {
        output$info <- renderText("❌ Es muss genau eine .shp-Datei ausgewählt sein.")
        return(NULL)
      }
      
      # temp-Verzeichnis für Shape
      tmpdir <- tempfile("shape_")
      dir.create(tmpdir, recursive = TRUE)
      
      # Basisname
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
      
      incProgress(0.6, detail = "Shape einlesen …")
      vect_orig <- tryCatch(terra::vect(shp_path), error = function(e) e)
      if (inherits(vect_orig, "error")) {
        output$info <- renderText(paste("❌ Fehler beim Laden des Shapes:", vect_orig$message))
        return(NULL)
      }
      
      # Shape in RADKLIM-KBS reprojizieren
      incProgress(0.75, detail = "Shape auf Raster-KBS reprojizieren …")
      vect_radklim <- vect_orig
      if (!identical(terra::crs(vect_radklim), terra::crs(r))) {
        vect_radklim <- terra::project(vect_radklim, terra::crs(r))
      }
      
      # Shape zusätzlich in EPSG:25832 für KOSTRA
      vect_25832 <- tryCatch(
        terra::project(vect_orig, "EPSG:25832"),
        error = function(e) NULL
      )
      
      # Shared speichern
      shared$rast          <- r
      shared$shape         <- vect_radklim
      shared$shape_25832   <- vect_25832
      shared$times         <- times
      shared$product       <- product
      shared$dt_min        <- dt_min
      shared$radklim_path  <- input$raster$datapath
      
      incProgress(0.9, detail = "Metadaten übernehmen …")
      
      flaeche_km2 <- tryCatch(
        terra::expanse(vect_orig) / 1e6,
        error = function(e) NA_real_
      )
      
      output$info <- renderText(sprintf(
        "✅ %s geladen: %d Layer (%s – %s)\nShape: %s, Fläche: %.2f km²",
        basename(input$raster$name),
        terra::nlyr(r),
        format(min(shared$times)), format(max(shared$times)),
        basename(shp_files$name[shp_idx]),
        flaeche_km2
      ))
      
      incProgress(1, detail = "Fertig!")
    })
  })
  
  # 3) Vorschauplot
  output$preview <- renderPlot({
    req(shared$rast)
    r_plot <- shared$rast[[1]]
    
    p <- tidyterra::autoplot(r_plot) +
      theme_minimal() +
      ggtitle("RADKLIM (1. Layer, Original-Projektion)") +
      theme(plot.title = element_text(face = "bold"))
    
    if (!is.null(shared$shape)) {
      p <- p +
        tidyterra::geom_spatvector(
          data   = shared$shape,
          fill   = NA,
          color  = "red",
          linewidth = 1
        )
    }
    p
  })
}