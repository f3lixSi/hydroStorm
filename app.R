# ─────────────────────────────────────────────
# HydroStorm – Starkregen-Analyse-App
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

# Module & Utils laden
source("R/mod_import.R")
source("R/mod_process.R")
source("R/mod_plot.R")
source("R/utils.R")

# Upload-Limit (500 MB)
options(shiny.maxRequestSize = 500 * 1024^2)

# Branding-Header
app_header <- tags$div(
  class = "custom-navbar",
  img(src = "www/logo_hydrostorm.png", class = "branding-logo"),
  tags$div(
    class = "branding-text",
    tags$div("HydroStorm", class = "branding-title"),
    tags$div("Radarbasierte Analyse von Starkregenereignissen", class = "branding-subtitle")
  )
)

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
    tabPanel("📂 Datenimport", importUI("imp")),
    tabPanel("⚙️ Verarbeitung", processUI("proc")),
    tabPanel("📊 Ergebnisse", plotUI("plt"))
  )
)

server <- function(input, output, session) {
  
  # Gemeinsamer Speicher für Module
  shared <- reactiveValues(
    rast         = NULL,  # RADKLIM SpatRaster (stereo)
    shape        = NULL,  # Shape im RADKLIM-KBS
    shape_25832  = NULL,  # Shape in EPSG:25832 (für KOSTRA)
    times        = NULL,  # Zeitvektor (POSIXct)
    product      = NULL,  # "YW2017.002" oder "RW2017.002"
    dt_min       = NULL,  # Zeitschritt in Minuten
    radklim_path = NULL,  # Pfad zur hochgeladenen RADKLIM-Datei
    agg_fun      = "max", # "max" oder "mean"
    result       = NULL,  # Aggregierte Zeitreihe (data.table)
    durations    = NULL,  # tatsächlich berechnete Dauerstufen
    kostra       = NULL   # KOSTRA-Daten (data.table)
  )
  
  callModule(importServer,  "imp",  shared = shared)
  callModule(processServer, "proc", shared = shared)
  callModule(plotServer,    "plt",  shared = shared)
}

shiny::addResourcePath("www", "www")

shinyApp(ui, server)