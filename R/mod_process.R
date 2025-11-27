# ─────────────────────────────────────────────
# Modul: Verarbeitung (Clippen, Aggregation)
# ─────────────────────────────────────────────

processUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("⚙️ Verarbeitung & Analyse"),
    helpText("Hier werden die eingelesenen Rasterdaten auf das Untersuchungsgebiet zugeschnitten und ausgewertet."),
    
    selectInput(
      ns("durations"),
      "Dauerstufen (in Minuten):",
      choices  = c(5,10,15,20,30,45,60,90,120,180,240,360,540,720,
                   1080,1440,2880,4320,5760,7200,8640,10080),
      selected = c(30,60,120,360,720,1440),
      multiple = TRUE
    ),
    
    radioButtons(
      ns("agg_fun"),
      "Flächenaggregation über Shape:",
      choices = c("Maximum" = "max", "Mittelwert" = "mean"),
      selected = "max",
      inline = TRUE
    ),
    
    actionButton(
      ns("run_proc"),
      "Analyse starten",
      icon = icon("play"),
      class = "btn-primary"
    ),
    
    tags$hr(),
    verbatimTextOutput(ns("log")),
    
    DTOutput(ns("preview_table"))
  )
}

processServer <- function(input, output, session, shared) {
  ns <- session$ns
  
  observeEvent(input$run_proc, {
    # Raster + Zeitinfos müssen da sein, Gebiet kommt als Shape ODER Punkt
    req(shared$rast, shared$times, shared$shape)
    
    withProgress(message = "Verarbeitung läuft …", value = 0, {
      
      ## 1) Auswertungsgeometrie in Raster-KBS vorbereiten --------------------
      incProgress(0.2, detail = "Auswertungsgebiet auf Raster-KBS bringen …")
      
      r_crs <- terra::crs(shared$rast)
      geom_radklim <- NULL
      
      if (!is.null(shared$shape)) {
        # Fall 1: Polygon-Maske vorhanden
        s_crs <- terra::crs(shared$shape)
        geom_radklim <- if (!identical(r_crs, s_crs)) {
          terra::project(shared$shape, r_crs)
        } else {
          shared$shape
        }
      } else if (!is.null(shared$pt_ll)) {
        # Fall 2: Punkt aus Leaflet (WGS84) vorhanden
        v_ll <- terra::vect(shared$pt_ll)      # shared$pt_ll: sf in EPSG:4326
        geom_radklim <- terra::project(v_ll, r_crs)
      } else {
        stop("Kein Auswertungsgebiet gefunden. Bitte Shape laden oder Punkt in der Karte setzen.")
      }
      
      ## 2) Flächenaggregat je Zeitschritt ------------------------------------
      incProgress(0.4, detail = "Flächenaggregation je Zeitschritt …")
      
      agg_fun_name <- if (identical(input$agg_fun, "mean")) "mean" else "max"
      vals <- terra::extract(
        shared$rast,
        geom_radklim,
        fun   = match.fun(agg_fun_name),
        na.rm = TRUE,
        ID    = FALSE
      )
      if (is.null(vals) || ncol(vals) == 0) {
        stop("Clip-Ergebnis ist leer (alles NA). Liegt das Shape bzw. der Punkt im RADKLIM-Gebiet?")
      }
      
      series_mm <- as.numeric(vals[1, ])
      times     <- shared$times
      
      if (length(series_mm) != length(times)) {
        stop("Längen von Zeitvektor und Wertvektor stimmen nicht überein.")
      }
      
      # --- Zeitbasis (dt_min) NUR aus Zeitvektor bestimmen ---
      #   -> YW:  5 min
      #   -> RW: 60 min
      times_posix <- if (inherits(times, "POSIXt")) {
        times
      } else {
        as.POSIXct(times, tz = "UTC")
      }
      
      if (length(times_posix) < 2) {
        stop("Zu wenige Zeitstempel, um die Zeitbasis zu bestimmen.")
      }
      
      dt_min <- as.integer(round(
        as.numeric(difftime(times_posix[2], times_posix[1], units = "mins"))
      ))
      
      # Fallback, falls irgendwas schiefgeht
      if (is.na(dt_min) || dt_min <= 0) {
        dt_min <- 60L
      }
      
      # Für spätere Nutzung einmal merken
      shared$dt_min <- dt_min
      
      ## 3) Dauerstufen bilden -------------------------------------------------
      incProgress(0.7, detail = "Dauerstufen aggregieren …")
      
      durations <- sort(unique(as.integer(input$durations)))
      DT <- data.table(
        Zeit     = as.POSIXct(times, tz = "UTC"),
        value_mm = series_mm
      )
      
      # Rolling sums (rechtsbündig) in mm
      for (d in durations) {
        w <- as.integer(round(d / dt_min))
        cname <- paste0("sum_", d, "min")
        
        if (is.na(w) || w < 1L || nrow(DT) < w) {
          DT[, (cname) := NA_real_]
        } else {
          DT[, (cname) := frollsum(value_mm, n = w, align = "right", na.rm = TRUE)]
        }
      }
      
      ## 4) Spaltennamen & Format ---------------------------------------------
      agg_label <- if (identical(input$agg_fun, "mean")) "Mittelwert" else "Max"
      
      nice_names <- names(DT)
      nice_names[1] <- "Zeit"
      nice_names[2] <- sprintf("%s [mm] (Flächenaggregat)", agg_label)
      nice_names <- gsub("^sum_(\\d+)min$", "D = \\1 min [mm]", nice_names)
      setnames(DT, names(DT), nice_names)
      
      # Zeit als String für Tabelle, numerisch runden
      DT[, Zeit := format(Zeit, "%Y-%m-%d %H:%M")]
      num_cols <- names(DT)[sapply(DT, is.numeric)]
      for (col in num_cols) {
        DT[, (col) := round(get(col), 2)]
      }
      
      # in shared speichern
      shared$result    <- DT
      shared$durations <- durations
      shared$agg_fun   <- input$agg_fun
      
      incProgress(1, detail = "Fertig.")
    })
    
    output$log <- renderText("✅ Analyse erfolgreich abgeschlossen.")
    
    output$preview_table <- renderDT({
      req(shared$result)
      datatable(
        shared$result,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = "400px",
          paging  = FALSE,
          dom     = "tip"
        ),
        escape = TRUE
      )
    })
  })
}