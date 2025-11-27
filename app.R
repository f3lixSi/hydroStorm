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
library(data.table)
library(readr)
library(jsonlite)
library(DT)

# Module laden
source("R/mod_import.R")
source("R/mod_process.R")
source("R/mod_plot.R")
source("R/utils.R")

# Upload-Limit
options(shiny.maxRequestSize = 500 * 1024^2)

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
      "– Produkte YW und RW). Die App extrahiert Flächenkennwerte (Maximum oder Mittelwert) ",
      "über einem frei wählbaren Untersuchungsgebiet und bildet daraus Dauerstufen-Zeitreihen ",
      "mit anschließender Gegenüberstellung zu KOSTRA-Daten."
    ),
    
    tags$hr(),
    h3("Workflow – so gehst du vor"),
    tags$ol(
      tags$li(
        strong("Datenimport (Tab „📂 Datenimport“):"),
        tags$ul(
          tags$li("RADKLIM-NetCDF-Datei (.nc) auswählen (z. B. YW_2017.002_YYYYMMDD.nc)."),
          tags$li("Zeitraum festlegen – wird nach Upload automatisch auf den verfügbaren Zeitraum eingeschränkt."),
          tags$li("Untersuchungsgebiet als Shapefile hochladen (alle zugehörigen Dateien auswählen: .shp, .dbf, .shx, .prj).")
        )
      ),
      tags$li(
        strong("Verarbeitung & Analyse (Tab „⚙️ Verarbeitung“):"),
        tags$ul(
          tags$li("Dauerstufen (in Minuten) auswählen, die ausgewertet werden sollen."),
          tags$li("Option wählen, ob über der Fläche das Flächenmaximum oder der Flächenmittelwert gebildet wird."),
          tags$li("Auf „Analyse starten“ klicken – der Fortschrittsbalken zeigt den Status der Berechnung."),
          tags$li("Die Ergebnis-Tabelle zeigt die Zeitreihe der Flächenkennwerte sowie die aggregierten Dauerstufen.")
        )
      ),
      tags$li(
        strong("Ergebnisse (Tab „📊 Ergebnisse“):"),
        tags$ul(
          tags$li("Zeitreihe der Originalwerte oder einer ausgewählten Dauerstufe plotten."),
          tags$li("KOSTRA-Abgleich starten (über Kachelindex oder Koordinate aus der Maske)."),
          tags$li("Dauerstufenvergleich HydroStorm vs. KOSTRA für frei wählbare Jährlichkeiten darstellen."),
          tags$li("HydroStorm- und KOSTRA-Daten als CSV exportieren.")
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
        "Die Auswertung erfolgt aktuell immer auf Basis des gewählten Gebiets (Polygon) – ",
        "entweder als Flächenmaximum oder als Flächenmittelwert je Zeitschritt."
      )
    ),
    
    tags$hr(),
    h3("Hinweise zu KOSTRA"),
    tags$ul(
      tags$li(
        "Der KOSTRA-Abgleich verwendet die offizielle KOSTRA-REST-API. ",
        "Hierfür wird ein gültiger API-Key (Umgebungsvariable ", code("KOSTRA_KEY"), ") benötigt."
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
      "© 2025 Felix Simon, Hochschule Bochum."
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
    info_help_tab 
  )
)

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Shared reactive values
  shared <- reactiveValues(rast = NULL, shape = NULL, shape_25832 = NULL,
                           result = NULL, kostra = NULL, dt_min = NULL,
                           product = NULL, radklim_path = NULL)
  
  # Module
  callModule(importServer,  "imp",  shared = shared)
  callModule(processServer, "proc", shared = shared)
  callModule(plotServer,    "plt",  shared = shared)
  
  # About/Info-Button schaltet auf Info-Tab
  observeEvent(input$go_info, {
    updateTabsetPanel(session, "main_nav", selected = "ℹ️ Info & Hilfe")
  })
}

shiny::addResourcePath("www", "www")

shinyApp(ui, server)