# ─────────────────────────────────────────────
# Modul: Bericht-Export (Word / PDF via R Markdown)
# ─────────────────────────────────────────────

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Bericht-Export"),
    helpText(
      "Erzeugt einen Bericht mit Titelkopf/Metadaten, Zeitreihen-Plot, ",
      "KOSTRA-Vergleich mit SRI (inkl. maßgebender SRI-Infobox) sowie ",
      "SRI- und KOSTRA-Tabellen. Grundlage sind die aktuell im Reiter ",
      "„Ergebnisse“ gewählten Einstellungen (Dauerstufe, Jährlichkeiten)."
    ),
    radioButtons(
      ns("report_format"),
      "Format:",
      choices  = c("Word (.docx)" = "word", "PDF" = "pdf"),
      selected = "word",
      inline   = TRUE
    ),
    helpText("Hinweis: Für PDF wird eine LaTeX-Installation (z. B. tinytex) benötigt."),
    verbatimTextOutput(ns("report_status")),
    downloadButton(ns("dl_report"), "Bericht herunterladen", class = "btn-primary")
  )
}

reportServer <- function(input, output, session, shared) {
  ns <- session$ns

  output$report_status <- renderText({
    if (is.null(shared$result)) {
      "Noch keine Analyse vorhanden. Bitte zuerst Daten laden und die Verarbeitung durchführen."
    } else if (is.null(shared$kostra)) {
      "Bereit – für die SRI-Einordnung im Bericht bitte zusätzlich den KOSTRA-Abgleich starten."
    } else {
      "Bereit: Bericht kann erzeugt werden."
    }
  })

  output$dl_report <- downloadHandler(
    filename = function() {
      ext <- if (identical(input$report_format, "pdf")) "pdf" else "docx"
      paste0("HydroStorm_Bericht_", Sys.Date(), ".", ext)
    },
    content = function(file) {
      req(shared$result)
      outfmt <- if (identical(input$report_format, "pdf")) "pdf_document" else "word_document"

      # Für PDF: LaTeX-Verfügbarkeit vorab prüfen und klar melden
      if (identical(outfmt, "pdf_document")) {
        has_latex <- nzchar(Sys.which("xelatex")) || nzchar(Sys.which("pdflatex")) ||
          (requireNamespace("tinytex", quietly = TRUE) && isTRUE(tinytex::is_tinytex()))
        if (!isTRUE(has_latex)) {
          showNotification(
            paste(
              "❌ PDF-Export nicht möglich: keine LaTeX-Installation gefunden.",
              "Bitte einmalig in R ausführen: install.packages('tinytex'); tinytex::install_tinytex(),",
              "danach R neu starten. Word funktioniert ohne LaTeX."
            ),
            type = "error", duration = 15
          )
          stop("Keine LaTeX-Installation gefunden (PDF-Export).")
        }
      }

      period <- if (!is.null(shared$times)) {
        paste(format(min(shared$times)), "–", format(max(shared$times)))
      } else "–"
      agg_lbl  <- if (identical(shared$agg_fun, "mean")) "Mittelwert" else "Maximum"
      area_lbl <- switch(
        shared$area_mode %||% "",
        "shape"   = "Shape (Polygon)",
        "point"   = "Punkt in Karte",
        "address" = "Adresse",
        "–"
      )
      loc <- shared$location_label
      loc <- if (!is.null(loc) && length(loc) == 1 && !is.na(loc)) as.character(loc) else "unbekannt"

      # Render-Umgebung: Daten hier, Bausteine (hydro_*) via globalenv
      renv <- new.env(parent = globalenv())
      renv$rep_result    <- shared$result
      renv$rep_kostra    <- shared$kostra
      renv$rep_durations <- shared$durations
      renv$rep_product   <- shared$product %||% ""
      renv$rep_location  <- loc
      renv$rep_plot_dur  <- as.integer(shared$plot_dur %||% min(shared$durations))
      renv$rep_kostra_T  <- shared$kostra_T %||% 100
      renv$rep_agg       <- agg_lbl
      renv$rep_area      <- area_lbl
      renv$rep_period    <- period
      logo_path          <- normalizePath(file.path("www", "logo_hydrostorm.png"), mustWork = FALSE)
      renv$rep_logo      <- if (file.exists(logo_path)) logo_path else NULL

      tmpl    <- normalizePath(file.path("report", "hydrostorm_report.Rmd"))
      tmp_rmd <- file.path(tempdir(), "hydrostorm_report.Rmd")
      file.copy(tmpl, tmp_rmd, overwrite = TRUE)
      # Logo neben die Vorlage kopieren (für die PDF-Kopfzeile via fancyhdr)
      if (!is.null(renv$rep_logo)) {
        file.copy(renv$rep_logo, file.path(tempdir(), "logo_hydrostorm.png"), overwrite = TRUE)
      }
      # Word-Referenzvorlage (Logo in Kopfzeile) mitkopieren
      ref_docx <- normalizePath(file.path("report", "reference.docx"), mustWork = FALSE)
      if (file.exists(ref_docx)) {
        file.copy(ref_docx, file.path(tempdir(), "reference.docx"), overwrite = TRUE)
      }

      withProgress(message = "📄 Bericht wird erzeugt …", value = 0.3, {
        out <- tryCatch(
          rmarkdown::render(
            tmp_rmd,
            output_format = outfmt,
            envir         = renv,
            output_dir    = tempdir(),
            quiet         = TRUE
          ),
          error = function(e) e
        )
        if (inherits(out, "error")) {
          showNotification(
            paste("❌ Bericht konnte nicht erzeugt werden:", conditionMessage(out)),
            type = "error", duration = 12
          )
          stop(out)
        }
        incProgress(0.9)
        file.copy(out, file, overwrite = TRUE)
      })

      showNotification(
        "✅ Bericht erstellt (Speicherort siehe Browser-Downloads).",
        type = "message", duration = 5
      )
    }
  )
}
