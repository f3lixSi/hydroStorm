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
    
    checkboxInput(
      ns("sri_color"),
      "HydroStorm-Punkte im Vergleichsplot nach Starkregenindex (SRI) einfärben",
      value = TRUE
    ),

    tags$hr(),

    # Plot-/Tabellenbereich
    tabsetPanel(
      tabPanel(
        "Zeitreihe (HydroStorm)",
        plotly::plotlyOutput(ns("plot_ts"), height = 420)
      ),
      tabPanel(
        "Dauerstufenvergleich (HydroStorm vs. KOSTRA)",
        plotly::plotlyOutput(ns("plot_kostra"), height = 420)
      ),
      tabPanel(
        "SRI-Einordnung",
        uiOutput(ns("sri_tile")),
        DT::DTOutput(ns("sri_table")),
        tags$hr(),
        helpText(
          "Einordnung nach dem Starkregenindex (SRI) gemäß Schmitt et al. (2018). ",
          "Der maßgebende SRI ist das Maximum über alle Dauerstufen. ",
          "Voraussetzung: KOSTRA-Abgleich wurde gestartet."
        )
      ),
      tabPanel(
        "KOSTRA-Tabelle",
        DT::DTOutput(ns("kostra_table"))
      )
    ),

    tags$hr(),

    # ---- Plot-Export ----------------------------------------------------------
    h4("Plot-Export"),
    helpText(
      "Der Bildschirm-Plot ist interaktiv (Hover, Zoom). Der Export nutzt die ",
      "Original-ggplot-Grafik in Druckqualität. Der Speicherort wird vom Browser ",
      "bestimmt – ggf. im Browser „Speicherort vor dem Download erfragen“ aktivieren."
    ),
    fluidRow(
      column(
        2,
        selectInput(
          ns("exp_format"), "Format",
          choices  = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg",
                       "JPEG" = "jpeg", "TIFF" = "tiff"),
          selected = "png"
        )
      ),
      column(2, numericInput(ns("exp_width"),  "Breite",  value = 15,  min = 1, step = 1)),
      column(2, numericInput(ns("exp_height"), "Höhe",   value = 9,   min = 1, step = 1)),
      column(
        2,
        selectInput(
          ns("exp_units"), "Einheit",
          choices  = c("cm" = "cm", "mm" = "mm", "Zoll" = "in", "Pixel" = "px"),
          selected = "cm"
        )
      ),
      column(2, numericInput(ns("exp_dpi"), "dpi", value = 600, min = 72, step = 10))
    ),
    fluidRow(
      column(
        4,
        downloadButton(ns("dl_plot_ts"), "Zeitreihe als Bild", class = "btn-secondary")
      ),
      column(
        4,
        downloadButton(ns("dl_plot_kostra"), "KOSTRA-Vergleich als Bild", class = "btn-secondary")
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

  # Ortsbezeichnung (aus Datenimport) sicher als Text
  loc_label <- reactive({
    l <- shared$location_label
    if (is.null(l) || length(l) == 0 || is.na(l)) "unbekannt" else as.character(l)
  })

  # Aktuelle Auswahl für den Bericht in shared spiegeln
  observe({
    shared$plot_dur  <- input$plot_dur
    shared$kostra_T  <- input$kostra_T
    shared$sri_color <- isTRUE(input$sri_color)
  })

  # ---- SRI-Einordnung je Dauerstufe (HydroStorm-Maxima vs. KOSTRA-HN) ----
  sri_data <- reactive({
    req(shared$result, shared$kostra, shared$durations)
    df <- hydro_sri_table(shared$result, shared$kostra, shared$durations)
    req(!is.null(df), nrow(df) > 0)
    df
  })

  # Farbige Gesamt-Kachel (maßgebender SRI)
  output$sri_tile <- renderUI({
    if (is.null(shared$result) || is.null(shared$kostra)) {
      return(div(
        style = "padding:10px;color:#666;",
        "Bitte zuerst Daten laden, die Analyse durchführen und den KOSTRA-Abgleich starten."
      ))
    }
    df <- sri_data()
    valid <- df[!is.na(df$sri) & df$sri > 0, , drop = FALSE]
    if (nrow(valid) == 0) {
      return(div(
        style = "padding:10px;color:#666;",
        "Kein Starkregen: alle ausgewerteten Dauerstufen liegen unter der 1-jährlichen Niederschlagshöhe."
      ))
    }
    imax <- which.max(valid$sri)
    sri  <- valid$sri[imax]
    bg   <- unname(SRI_COLORS[as.character(sri)])
    fg   <- unname(SRI_TEXTCOL[as.character(sri)])
    div(
      style = sprintf(
        "background:%s;color:%s;border-radius:12px;padding:16px 20px;margin-bottom:14px;",
        bg, fg
      ),
      div(style = "font-size:13px;opacity:.9;", "Maßgebender Starkregenindex"),
      div(style = "font-size:30px;font-weight:600;line-height:1.1;", sprintf("SRI %d", sri)),
      div(style = "font-size:15px;", sri_category(sri)),
      div(style = "font-size:13px;margin-top:4px;",
          sprintf("maßgebende Dauerstufe: %g min  ·  erreichte Jährlichkeit: %s",
                  valid$dauer_min[imax], valid$wiederkehr[imax]))
    )
  })

  # SRI-Tabelle je Dauerstufe (SRI-Spalte farbcodiert)
  output$sri_table <- DT::renderDT({
    req(shared$result, shared$kostra)
    df <- sri_data()
    disp <- data.frame(
      "Dauer [min]"            = df$dauer_min,
      "hN beobachtet [mm]"     = df$hN_obs,
      "erreichte Jährlichkeit" = df$wiederkehr,
      "SRI"                    = df$sri,
      "Kategorie"              = df$kategorie,
      check.names = FALSE
    )
    lv <- as.character(0:12)
    DT::datatable(
      disp, rownames = FALSE,
      options = list(paging = FALSE, dom = "t", ordering = FALSE, scrollX = TRUE)
    ) |>
      DT::formatStyle(
        "SRI",
        backgroundColor = DT::styleEqual(as.numeric(lv), unname(SRI_COLORS[lv])),
        color           = DT::styleEqual(as.numeric(lv), unname(SRI_TEXTCOL[lv])),
        fontWeight      = "bold"
      )
  })
  
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
  
  # ---- Hilfsfunktion: ggplot -> plotly (Untertitel in Titel falten) ----
  to_plotly <- function(p) {
    gg  <- plotly::ggplotly(p, tooltip = "text")

    # Legendennamen bereinigen: ggplotly erzeugt Tupel wie "(KOSTRA T=10 a,1)" / "(2,1)"
    for (i in seq_along(gg$x$data)) {
      nm <- gg$x$data[[i]]$name
      if (!is.null(nm) && length(nm) == 1 && is.character(nm)) {
        nm <- sub("^\\(", "", nm)
        nm <- sub(",\\s*[0-9]+\\)\\s*$", "", nm)
        nm <- sub("\\)\\s*$", "", nm)
        gg$x$data[[i]]$name <- nm
        if (!is.null(gg$x$data[[i]]$legendgroup)) {
          gg$x$data[[i]]$legendgroup <- nm
        }
      }
    }

    ttl <- p$labels$title
    sub <- p$labels$subtitle
    if (!is.null(sub)) sub <- gsub("\n", "<br>", sub)   # Zeilenumbrüche für plotly
    if (!is.null(ttl) || !is.null(sub)) {
      txt <- if (!is.null(sub)) {
        paste0(ttl, "<br><sup>", sub, "</sup>")
      } else {
        ttl
      }
      gg <- plotly::layout(gg, title = list(text = txt))
    }
    gg
  }

  # ---- Zeitreihenplot (HydroStorm) als reaktives ggplot ----
  ts_plot <- reactive({
    req(shared$result, shared$durations)
    dur <- as.integer(input$plot_dur)
    req(!is.na(dur))
    p <- hydro_ts_plot(shared$result, dur, shared$product, loc_label())
    req(!is.null(p))
    p
  })

  output$plot_ts <- plotly::renderPlotly({
    to_plotly(ts_plot())
  })
  
  # ---- CSV-Export (HydroStorm) ----
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("hydrostorm_", Sys.Date(), "_", shared$product, ".csv")
    },
    content = function(file) {
      req(shared$result)
      readr::write_csv(shared$result, file)
      showNotification(
        "✅ HydroStorm-CSV heruntergeladen (Speicherort siehe Browser-Downloads).",
        type = "message", duration = 5
      )
    }
  )

  # ---- Plot-Export via ggsave ----
  export_plot <- function(plot_reactive, base_name) {
    downloadHandler(
      filename = function() {
        paste0(base_name, "_", Sys.Date(), ".", input$exp_format)
      },
      content = function(file) {
        p <- plot_reactive()
        ok <- tryCatch({
          ggplot2::ggsave(
            filename = file,
            plot     = p,
            device   = input$exp_format,
            width    = input$exp_width,
            height   = input$exp_height,
            units    = input$exp_units,
            dpi      = input$exp_dpi
          )
          TRUE
        }, error = function(e) {
          showNotification(
            paste("❌ Export fehlgeschlagen:", e$message),
            type = "error", duration = 8
          )
          FALSE
        })
        if (isTRUE(ok)) {
          showNotification(
            sprintf(
              "✅ Plot heruntergeladen (%s, %g×%g %s, %g dpi).",
              toupper(input$exp_format),
              input$exp_width, input$exp_height, input$exp_units, input$exp_dpi
            ),
            type = "message", duration = 5
          )
        }
      }
    )
  }

  output$dl_plot_ts     <- export_plot(ts_plot,     "hydrostorm_zeitreihe")
  output$dl_plot_kostra <- export_plot(kostra_plot, "hydrostorm_kostra_vergleich")
  
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
      showNotification(
        "✅ KOSTRA-CSV heruntergeladen (Speicherort siehe Browser-Downloads).",
        type = "message", duration = 5
      )
    }
  )
  
  # ---- Dauerstufen-Vergleichsplot HydroStorm vs. KOSTRA als reaktives ggplot ----
  kostra_plot <- reactive({
    req(shared$result, shared$kostra, shared$durations, input$kostra_T)
    p <- hydro_kostra_plot(
      shared$result, shared$kostra, shared$durations,
      input$kostra_T, loc_label(), isTRUE(input$sri_color)
    )
    req(!is.null(p))
    p
  })

  output$plot_kostra <- plotly::renderPlotly({
    to_plotly(kostra_plot())
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