# ─────────────────────────────────────────────
# mod_plot.R — HydroStorm (Ergebnisse + KOSTRA)
# ─────────────────────────────────────────────

plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Ergebnisse"),
    
    # Steuer-Panel oben
    fluidRow(
      column(
        3,
        selectInput(
          ns("plot_dur"),
          "Dauerstufe (für Zeitreihe):",
          choices = c(5,10,15,20,30,45,60,90,120,180,240,360,540,
                      720,1080,1440,2880,4320,5760,7200,8640,10080),
          selected = 60
        )
      ),
      column(
        3,
        textInput(
          ns("kostra_index"),
          "KOSTRA-Kachelindex (optional):",
          placeholder = "z. B. 129105"
        )
      ),
      column(
        3,
        selectInput(
          ns("kostra_T"),
          "KOSTRA Jährlichkeit(en):",
          choices  = c(1, 2, 3, 5, 10, 20, 30, 50, 100),
          selected = 100,
          multiple = TRUE
        )
      ),
      column(
        3,
        actionButton(
          ns("compare_kostra"),
          "KOSTRA-Abgleich starten",
          icon  = icon("chart-line"),
          class = "btn-primary"
        )
      )
    ),
    
    tags$hr(),
    
    # Plot-/Tabellenbereich
    tabsetPanel(
      tabPanel(
        "Zeitreihe (HydroStorm)",
        plotOutput(ns("plot_ts"), height = 420)
      ),
      tabPanel(
        "Dauerstufenvergleich (HydroStorm vs. KOSTRA)",
        plotOutput(ns("plot_kostra"), height = 420)
      ),
      tabPanel(
        "KOSTRA-Tabelle",
        DT::DTOutput(ns("kostra_table"))
      )
    ),
    
    tags$hr(),
    
    # Downloads + Info
    downloadButton(ns("dl_csv"), "Download HydroStorm CSV", class = "btn-secondary"),
    tags$br(), tags$br(),
    downloadButton(ns("dl_kostra"), "Download KOSTRA CSV", class = "btn-secondary"),
    tags$hr(),
    verbatimTextOutput(ns("kostra_out"))
  )
}

plotServer <- function(input, output, session, shared) {
  ns <- session$ns
  
  # ---- Dauerstufen-Auswahl an berechnete Dauerstufen anpassen ----
  observeEvent(shared$result, {
    req(shared$durations)
    dur <- shared$durations
    updateSelectInput(
      session,
      "plot_dur",
      choices  = dur,
      selected = min(dur)
    )
  }, ignoreNULL = TRUE)
  
  # ---- Zeitreihenplot (HydroStorm) ----
  output$plot_ts <- renderPlot({
    req(shared$result, shared$durations)
    
    dur <- as.integer(input$plot_dur)
    req(!is.na(dur))
    
    cname <- paste0("D = ", dur, " min [mm]")
    req(cname %in% names(shared$result))
    
    ggplot(shared$result, aes(x = as.POSIXct(Zeit), y = .data[[cname]])) +
      geom_line(linewidth = 1.1, color = "#004E7C") +
      theme_bw(base_size = 12) +
      labs(
        x = "Zeit (UTC)",
        y = "Niederschlag [mm]",
        title = sprintf("HydroStorm – Dauerstufe %d min", dur),
        subtitle = sprintf(
          "Produkt %s | Zeitraum %s – %s",
          shared$product,
          format(min(as.POSIXct(shared$result$Zeit))),
          format(max(as.POSIXct(shared$result$Zeit)))
        )
      )
  })
  
  # ---- CSV-Export (HydroStorm) ----
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("hydrostorm_", Sys.Date(), "_", shared$product, ".csv")
    },
    content = function(file) {
      req(shared$result)
      readr::write_csv(shared$result, file)
    }
  )
  
  # ---- KOSTRA-Abruf + Zusammenfassung ----
  observeEvent(input$compare_kostra, {
    output$kostra_out <- renderText("⏳ KOSTRA-Abfrage läuft ...")
    
    tryCatch({
      # Entweder per Index oder über Koordinate aus shape_25832
      if (nzchar(input$kostra_index)) {
        kostra <- get_kostra_data(index = input$kostra_index)
      } else {
        # Shape oder Punkt als Koordinate für KOSTRA verwenden
        if (!is.null(shared$shape_25832)) {
          e <- terra::ext(shared$shape_25832)
          x <- (e[1] + e[2]) / 2
          y <- (e[3] + e[4]) / 2
        } else if (!is.null(shared$pt_25832)) {
          coords <- sf::st_coordinates(shared$pt_25832)
          x <- coords[1]
          y <- coords[2]
        } else {
          stop("Keine Geometrie für KOSTRA-Abfrage vorhanden. Bitte Shape laden oder Punkt setzen.")
        }
        kostra <- get_kostra_data(x = x, y = y)
      }
      
      shared$kostra <- kostra
      
      output$kostra_out <- renderPrint({
        cat("✅ KOSTRA-Daten erfolgreich geladen\n")
        cat(sprintf(
          "Dauerstufen: %d – %d min\n",
          min(kostra$dauer_min),
          max(kostra$dauer_min)
        ))
        cat("Typen:", paste(unique(kostra$typ), collapse = ", "), "\n")
        cat("Jährlichkeiten:", paste(unique(kostra$T_Jahre), collapse = ", "), "Jahre\n")
      })
    },
    error = function(e) {
      output$kostra_out <- renderText(
        paste("❌ Fehler bei KOSTRA-Abfrage:", e$message)
      )
    })
  })
  
  # ---- KOSTRA-Download ----
  output$dl_kostra <- downloadHandler(
    filename = function() paste0("kostra_", Sys.Date(), ".csv"),
    content = function(file) {
      req(shared$kostra)
      readr::write_csv(shared$kostra, file)
    }
  )
  
  # ---- Dauerstufen-Vergleichsplot HydroStorm vs. KOSTRA ----
  output$plot_kostra <- renderPlot({
    req(shared$result, shared$kostra)
    req(input$kostra_T)  # mind. eine Jährlichkeit ausgewählt
    
    hs <- shared$result
    k  <- shared$kostra
    
    # --- HydroStorm: nur berechnete Dauerstufen ---
    durations_hs <- shared$durations
    req(durations_hs)
    
    ds_cols <- paste0("D = ", durations_hs, " min [mm]")
    valid   <- ds_cols %in% names(hs)
    durations_hs <- durations_hs[valid]
    ds_cols      <- ds_cols[valid]
    req(length(durations_hs) > 0)
    
    maxvals <- sapply(
      ds_cols,
      function(col) max(hs[[col]], na.rm = TRUE)
    )
    
    df_hs <- data.frame(
      dauer_min = durations_hs,
      value     = maxvals,
      source    = "HydroStorm",
      stringsAsFactors = FALSE
    )
    
    # --- KOSTRA: alle Dauerstufen für ausgewählte Jährlichkeiten (nur HN) ---
    df_k <- subset(
      k,
      typ == "HN" & T_Jahre %in% input$kostra_T
    )
    req(nrow(df_k) > 0)
    
    # Eine Nachkommastelle
    df_k$value <- round(df_k$value, 1)
    
    # Legende: "KOSTRA T=xx a"
    df_k$source <- paste0("KOSTRA T=", df_k$T_Jahre, " a")
    
    # schön sortieren
    df_k <- df_k[order(df_k$dauer_min, df_k$T_Jahre), ]
    
    # --- Zusammenführen ---
    df_all <- rbind(
      df_hs[, c("dauer_min", "value", "source")],
      df_k[,  c("dauer_min", "value", "source")]
    )
    
    ggplot(df_all, aes(x = dauer_min, y = value, color = source)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      scale_x_log10(
        breaks = c(5,10,15,20,30,45,60,90,120,180,240,360,540,
                   720,1080,1440,2880,4320,5760,7200,8640,10080)
      ) +
      theme_bw(base_size = 12) +
      labs(
        x = "Dauerstufe [min] (log-Skala)",
        y = "Niederschlagshöhe [mm]",
        title = "HydroStorm vs. KOSTRA",
        color = "Datenquelle"
      )
  })
  
  # ---- Neue KOSTRA-Tabelle (Dauerstufen x Jährlichkeiten) ----
  output$kostra_table <- DT::renderDT({
    req(shared$kostra)
    
    k <- shared$kostra
    
    # Nur HN (Niederschlagshöhe)
    df <- subset(k, typ == "HN")
    req(nrow(df) > 0)
    
    dt <- data.table::as.data.table(df)
    
    # Auf eine Nachkommastelle runden
    dt[, value := round(value, 1)]
    
    # Wide-Format: Dauerstufe ~ T_Jahre
    wide <- data.table::dcast(
      dt,
      dauer_min ~ T_Jahre,
      value.var = "value"
    )
    
    # Nach Dauerstufe sortieren
    data.table::setorder(wide, dauer_min)
    
    # Spaltennamen setzen: erste Spalte = Dauer, Rest = T=xx a
    old_names <- names(wide)
    new_names <- old_names
    new_names[1] <- "Dauer [min]"
    if (length(old_names) > 1) {
      new_names[-1] <- paste0("T=", old_names[-1], " a")
    }
    data.table::setnames(wide, old_names, new_names)
    
    DT::datatable(
      wide,
      rownames = FALSE,
      options = list(
        scrollX   = TRUE,       # horizontal scrollbar
        scrollY   = "400px",    # vertikale Scrollhöhe
        paging    = FALSE,      # keine Seiten, nur scrollen
        dom       = "tip",
        ordering  = FALSE
      )
    )
  })
}