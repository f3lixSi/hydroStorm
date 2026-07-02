# ─────────────────────────────────────────────
# HydroStorm — radarbasierte Analyse von Starkregenereignissen
# Haupt-App
# ─────────────────────────────────────────────

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(terra)
library(tidyterra)
library(ggplot2)
library(plotly)
library(data.table)
library(readr)
library(jsonlite)
library(DT)
library(tidygeocoder)
library(rmarkdown)

# Module laden
source("R/mod_import.R")
source("R/mod_process.R")
source("R/mod_plot.R")
source("R/mod_report.R")
source("R/utils.R")

# Upload-Limit
options(shiny.maxRequestSize = 1000 * 1024^2)

# ---- Branding-Header ---------------------------------------------------------

app_header <- tags$div(
  class = "custom-navbar",
  img(src = "www/logo_hydrostorm.png", class = "branding-logo"),
  tags$div(
    class = "branding-text",
    tags$div("HydroStorm", class = "branding-title"),
    tags$div("Radarbasierte Analyse von Starkregenereignissen", class = "branding-subtitle")
  )
)

# ---- Info- / Hilfe-Tab ------------------------------------------------------

info_help_tab <- tabPanel(
  "ℹ️ Info & Hilfe",
  div(
    class = "p-4",
    
    h3("Über HydroStorm"),
    p(
      "HydroStorm ist ein Werkzeug zur Auswertung radarbasierter Niederschlagsdaten (RADKLIM ",
      "– Produkte YW und RW). Die App extrahiert Flächenkennwerte (Mittelwert oder Maximum) ",
      "über einem frei wählbaren Untersuchungsgebiet (Shapefile, Punkt oder Adresse), bildet daraus ",
      "Dauerstufen-Zeitreihen, stellt sie KOSTRA gegenüber, ordnet das Ereignis nach dem ",
      "Starkregenindex (SRI) ein und erzeugt auf Wunsch einen Bericht als Word oder PDF."
    ),
    
    tags$hr(),
    h3("Workflow – so gehst du vor"),
    tags$ol(
      tags$li(
        strong("Datenimport (Tab „📂 Datenimport“):"),
        tags$ul(
          tags$li("RADKLIM-NetCDF-Datei(en) (.nc) auswählen (z. B. YW_2017.002_YYYYMMDD.nc). Mehrere Tage können gemeinsam geladen werden."),
          tags$li("Zeitraum festlegen – wird nach Upload automatisch auf den verfügbaren Zeitraum gesetzt."),
          tags$li("Auswertungsgebiet wählen: Shapefile (.shp, .dbf, .shx, .prj gemeinsam), Punkt in die Karte klicken, oder Adresse eingeben und geokodieren."),
          tags$li("Nach „Daten laden“ legt die interaktive OpenStreetMap-Karte die Niederschlagssumme halbtransparent über das Gebiet.")
        )
      ),
      tags$li(
        strong("Verarbeitung & Analyse (Tab „⚙️ Verarbeitung“):"),
        tags$ul(
          tags$li("Dauerstufen (in Minuten) auswählen."),
          tags$li("Flächenkennwert wählen: Flächenmittelwert (Standard) oder Flächenmaximum je Zeitschritt."),
          tags$li("„Analyse starten“ – Ergebnis ist die Zeitreihe je Dauerstufe (rechtsbündige gleitende Summen).")
        )
      ),
      tags$li(
        strong("Ergebnisse (Tab „📊 Ergebnisse“):"),
        tags$ul(
          tags$li("Interaktive Zeitreihe je Dauerstufe (Hover/Zoom); Export als PNG/PDF/SVG."),
          tags$li("KOSTRA-Abgleich starten (über Kachelindex oder automatisch über Gebiets-/Punktkoordinate)."),
          tags$li("Dauerstufenvergleich HydroStorm vs. KOSTRA – optional mit SRI-Einfärbung der Ereignis-Punkte (per Checkbox)."),
          tags$li("Reiter „SRI-Einordnung“: maßgebender Starkregenindex als farbige Kachel und Tabelle je Dauerstufe."),
          tags$li("HydroStorm- und KOSTRA-Daten als CSV, Plots als Bild exportieren.")
        )
      ),
      tags$li(
        strong("Bericht (Tab „📄 Bericht“):"),
        tags$ul(
          tags$li("Bericht als Word oder PDF erzeugen: Titelkopf/Metadaten, Zeitreihe, KOSTRA-Vergleich mit SRI sowie SRI- und KOSTRA-Tabellen."),
          tags$li("Grundlage sind die im Reiter „Ergebnisse“ gewählten Einstellungen (Dauerstufe, Jährlichkeiten). Für die SRI-Einordnung muss der KOSTRA-Abgleich gestartet sein."),
          tags$li("PDF benötigt eine LaTeX-Installation (tinytex); Word funktioniert ohne.")
        )
      )
    ),
    
    tags$hr(),
    h3("Hinweise zu Eingangsdaten"),
    tags$ul(
      tags$li(
        strong("RADKLIM YW 2017.002: "),
        "5-min-Zeitschritt, Einheiten werden innerhalb der App auf Niederschlagshöhen [mm] ",
        "für den jeweiligen Zeitintervall umgerechnet."
      ),
      tags$li(
        strong("RADKLIM RW 2017.002: "),
        "Stundendaten; auch hier werden Einheiten einheitlich behandelt und als Summe in [mm] ",
        "für den Zeitschritt verwendet."
      ),
      tags$li(
        "Die Projektion der RADKLIM-Daten ist polare Stereografische Projektion. ",
        "Die Maske (Shape) wird intern automatisch auf diese Projektion reprojiziert."
      ),
      tags$li(
        "Das Auswertungsgebiet kann als Shapefile, als Punkt in der Karte oder als Adresse ",
        "vorgegeben werden; Punkt und Adresse werden intern zu einem 1-km-Puffer. Je Zeitschritt ",
        "wird daraus der Flächenmittelwert oder das Flächenmaximum gebildet."
      )
    ),
    
    tags$hr(),
    h3("Hinweise zu KOSTRA"),
    tags$ul(
      tags$li(
        "Der KOSTRA-Abgleich verwendet die offizielle KOSTRA-REST-API. ",
        "Hierfür ist ein gültiger API-Key implementiert."
      ),
      tags$li(
        "Die abgefragten KOSTRA-Werte beziehen sich auf die im KOSTRA-Datensatz definierten ",
        "Dauerstufen und Jährlichkeiten (z. B. 1, 2, 3, 5, 10, 20, 30, 50, 100 Jahre)."
      ),
      tags$li(
        "Für den räumlichen Bezug wird entweder ein manueller Kachelindex oder – falls nicht angegeben – ",
        "der Schwerpunkt des Untersuchungsgebiets (Shape) in EPSG:25832 verwendet."
      ),
      tags$li(
        "Bitte beachte, dass zwischen RADKLIM-Analysegebiet und KOSTRA-Kachel räumliche Abweichungen ",
        "bestehen können – die Gegenüberstellung dient einer orientierenden Plausibilisierung."
      )
    ),
    
    tags$hr(),
    h3("Starkregenindex (SRI)"),
    tags$ul(
      tags$li(
        "Die Einordnung folgt dem einheitlichen Verfahren nach Schmitt et al. (2018): je Dauerstufe ",
        "wird die beobachtete Niederschlagshöhe über KOSTRA einer Wiederkehrzeit zugeordnet und daraus ",
        "der Index 1–12 abgeleitet (SRI 1–2 Starkregen, 3–4 intensiv, 5–7 außergewöhnlich, 8–12 extrem)."
      ),
      tags$li(
        "Oberhalb der 100-jährlichen Höhe (SRI 8–12) erfolgt die Einordnung über den ",
        "Extrapolationsfaktor hN/hN(100a). Der maßgebende SRI des Ereignisses ist das Maximum ",
        "über alle ausgewerteten Dauerstufen."
      )
    ),

    tags$hr(),
    h3("Kontakt"),
    p(
      "Bei Fragen, Anmerkungen oder Feedback wende dich bitte an ",
      a(href = "mailto:felix.simon@hs-bochum.de", "Felix Simon"),
      "."
    ),
    
    tags$hr(),
    h3("Lizenz & Haftung"),
    p(
      "HydroStorm wird zu Forschungs- und Planungszwecken bereitgestellt. ",
      "Die Nutzung und Interpretation der Ergebnisse erfolgt in eigener Verantwortung. ",
      "Für Entscheidungen in der Planung, Genehmigung oder Gefahrenabwehr sind stets ",
      "zusätzliche Datengrundlagen (z. B. Pegelzeitreihen, lokale Starkregenanalysen, ",
      "hydraulische Modellierungen) heranzuziehen."
    ),
    p(
      "HydroStorm steht unter der ",
      strong("GNU General Public License, Version 3 (GPL-3.0)"),
      ". Details finden sich in der Datei ",
      code("LICENSE"),
      " im GitHub-Repository."
    ),
    p(
      "© 2026 Felix Simon, Hochschule Bochum."
    )
  )
)

# ---- UI ----------------------------------------------------------------------

ui <- tagList(
  app_header,
  navbarPage(
    id = "main_nav",
    title = NULL,
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      bg = "white",
      fg = "#004E7C",
      primary = "#004E7C",
      secondary = "#006DA4",
      success = "#004E7C",
      info = "#004E7C",
      base_font = bslib::font_google("Roboto"),
      heading_font = bslib::font_google("Roboto")
    ),
    header = tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabPanel("📂 Datenimport",   importUI("imp")),
    tabPanel("⚙️ Verarbeitung",  processUI("proc")),
    tabPanel("📊 Ergebnisse",    plotUI("plt")),
    tabPanel("📄 Bericht",       reportUI("rep")),
    info_help_tab
  )
)

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Shared reactive values
  shared <- reactiveValues(rast = NULL, shape = NULL, shape_25832 = NULL,
                           pt_ll = NULL, pt_radklim = NULL, pt_2583 = NULL,
                           result = NULL, kostra = NULL, dt_min = NULL,
                           product = NULL, radklim_path = NULL, times = NULL)
  
  # Module
  callModule(importServer,  "imp",  shared = shared)
  callModule(processServer, "proc", shared = shared)
  callModule(plotServer,    "plt",  shared = shared)
  callModule(reportServer,  "rep",  shared = shared)
  
  # About/Info-Button schaltet auf Info-Tab
  observeEvent(input$go_info, {
    updateTabsetPanel(session, "main_nav", selected = "ℹ️ Info & Hilfe")
  })
}

shiny::addResourcePath("www", "www")

shinyApp(ui, server)