# ─────────────────────────────────────────────
# utils.R — RADKLIM & KOSTRA
# ─────────────────────────────────────────────

library(terra)
library(httr)
library(jsonlite)
library(data.table)

# ---- RADKLIM: Zeitachse bestimmen (ohne Rasterwerte einzulesen) ----
# Schnelle Analyse nach dem Upload (Datumsbereich über ALLE Dateien) und
# Baustein für read_radklim_nc(). Fallback über den Dateinamen unterstützt
# Tagesdateien (…YYYYMMDD.nc) und Monatsdateien (…YYYYMM.nc).
radklim_time_axis <- function(path, fname = NULL, r = NULL) {
  ref_name <- if (!is.null(fname)) basename(fname) else basename(path)
  if (is.null(r)) r <- terra::rast(path)

  tvals <- tryCatch(terra::time(r), error = function(e) NULL)
  if (!is.null(tvals) && !all(is.na(tvals))) {
    return(as.POSIXct(tvals, tz = "UTC"))
  }

  step_s <- if (grepl("YW", ref_name, ignore.case = TRUE)) 5 * 60 else 60 * 60
  dstr   <- gsub("\\D", "", ref_name)

  t0 <- NULL
  if (nchar(dstr) >= 8) {
    t0 <- as.POSIXct(substr(dstr, nchar(dstr) - 7, nchar(dstr)),
                     format = "%Y%m%d", tz = "UTC")
    # Plausibilität: Monatsdateien (…YYYYMM) können im 8-Zeichen-Fenster
    # scheinbar gültige, aber absurde Jahre ergeben -> verwerfen
    if (!is.na(t0) && as.integer(format(t0, "%Y")) < 1900) t0 <- NA
  }
  if ((is.null(t0) || is.na(t0)) && nchar(dstr) >= 6) {
    # Monatsdatei: YYYYMM -> Monatsanfang
    t0 <- as.POSIXct(paste0(substr(dstr, nchar(dstr) - 5, nchar(dstr)), "01"),
                     format = "%Y%m%d", tz = "UTC")
  }

  if (!is.null(t0) && !is.na(t0)) {
    t0 + (seq_len(terra::nlyr(r)) - 1L) * step_s
  } else {
    rep(as.POSIXct(NA), terra::nlyr(r))
  }
}

# ---- RADKLIM: netCDF einlesen ----
read_radklim_nc <- function(path, t_start = NULL, t_end = NULL, fname = NULL) {
  # WICHTIG: Dateiname für Produkt-Erkennung
  ref_name <- if (!is.null(fname)) basename(fname) else basename(path)
  message("Lese RADKLIM-Datei: ", ref_name)
  
  r <- terra::rast(path)
  
  # CRS & Extent wie in deinem Skript
  terra::crs(r) <- "+proj=stere +lat_0=90.0 +lon_0=10.0 +lat_ts=60.0 +a=6370040 +b=6370040 +units=m"
  terra::ext(r) <- c(-443462, 456538, -4758645, -3658645)
  
  # Zeitachse (NetCDF-Zeit, sonst Fallback über Dateinamen – Tages-/Monatsdatei)
  tvals <- radklim_time_axis(path, fname = ref_name, r = r)
  if (all(is.na(tvals))) tvals <- seq_len(terra::nlyr(r))
  
  # Metadaten (für _FillValue etc.)
  md <- tryCatch(terra::describe(r[[1]]), error = function(e) NULL)
  if (is.null(md)) md <- list()
  
  safe_get <- function(x, key) {
    if (is.null(x)) return(NA_real_)
    if (is.list(x) && !is.data.frame(x) && key %in% names(x)) {
      return(suppressWarnings(as.numeric(x[[key]])))
    }
    if (is.data.frame(x) && key %in% rownames(x)) {
      val <- x[key, , drop = TRUE]
      return(suppressWarnings(as.numeric(val)))
    }
    NA_real_
  }
  
  fv <- safe_get(md, "_FillValue")
  mv <- safe_get(md, "missing_value")

  # Nodata über Metadaten-Flag statt Werte-Ersetzung:
  # r[r == fv] <- NA würde das KOMPLETTE Raster (alle Layer, ganz Deutschland)
  # im Speicher/Temp materialisieren – auf Servern mit wenig RAM (z. B. Posit
  # Connect Cloud) stürzt der R-Prozess dabei ab ("disconnected from server").
  # NAflag() setzt nur ein Nodata-Flag; GDAL wendet _FillValue i. d. R.
  # ohnehin automatisch an.
  na_val <- if (!is.na(fv) && is.finite(fv)) {
    fv
  } else if (!is.na(mv) && is.finite(mv)) {
    mv
  } else {
    NA_real_
  }
  if (!is.na(na_val)) {
    tryCatch(terra::NAflag(r) <- na_val, error = function(e) NULL)
  }
  
  # ref_name ist ein Vektor, wenn mehrere Dateien gewählt wurden
  is_yw <- grepl("YW", ref_name, ignore.case = TRUE)
  
  # Sicherheit: nicht YW und RW mischen
  if (any(is_yw) && any(!is_yw)) {
    stop("Bitte nicht YW- und RW-Dateien mischen.")
  }
  
  prod   <- if (any(is_yw)) "YW2017.002" else "RW2017.002"
  dt_min <- if (prod == "YW2017.002") 5L else 60L
  
  # Zeitraumfilter (ganze Tage)
  if (!is.null(t_start) && !is.null(t_end)) {
    if (inherits(t_start, "Date")) {
      t_start <- as.POSIXct(t_start, tz = "UTC")
    } else {
      t_start <- as.POSIXct(t_start, tz = "UTC")
    }
    if (inherits(t_end, "Date")) {
      t_end <- as.POSIXct(t_end, tz = "UTC") + (24 * 3600 - dt_min * 60)
    } else {
      t_end <- as.POSIXct(t_end, tz = "UTC")
    }
    
    keep <- which(tvals >= t_start & tvals <= t_end)
    if (length(keep) == 0) stop("Keine Layer im gewählten Zeitraum gefunden.")
    r     <- r[[keep]]
    tvals <- tvals[keep]
  }
  
  attr(r, "hydrostorm_time")    <- tvals
  attr(r, "hydrostorm_product") <- prod
  attr(r, "hydrostorm_dt_min")  <- dt_min
  
  r
}

# ---- Lange Nominatim-Adresse auf "Straße, Stadt" kürzen ----
short_location <- function(addr) {
  parts <- trimws(unlist(strsplit(addr, ",")))
  parts <- parts[nzchar(parts)]
  if (length(parts) <= 2) return(paste(parts, collapse = ", "))

  # Hausnummer am Anfang entfernen (z. B. "54" oder "54a")
  if (grepl("^[0-9]+[a-zA-Z]?$", parts[1])) parts <- parts[-1]
  # PLZ entfernen
  parts <- parts[!grepl("^[0-9]{3,}$", parts)]
  # Land (letztes Element) entfernen
  if (length(parts) >= 1) parts <- parts[-length(parts)]
  # Bundesländer entfernen
  bl <- c("Nordrhein-Westfalen", "Bayern", "Baden-Württemberg", "Niedersachsen",
          "Hessen", "Rheinland-Pfalz", "Sachsen", "Thüringen", "Brandenburg",
          "Sachsen-Anhalt", "Mecklenburg-Vorpommern", "Schleswig-Holstein",
          "Saarland", "Berlin", "Hamburg", "Bremen")
  parts <- parts[!parts %in% bl]

  if (length(parts) == 0)  return(addr)
  if (length(parts) == 1)  return(parts[1])
  # Straße (erstes) + Stadt (letztes verbleibendes)
  paste0(parts[1], ", ", parts[length(parts)])
}

# ---- Ortsbezeichnung bestimmen (für Plot-Titel/Caption) ----
# Reihenfolge: eingegebene Adresse -> Reverse-Geocoding des Schwerpunkts -> Koordinaten
determine_location_label <- function(area_mode, address = NULL,
                                     pt_ll = NULL, vect_25832 = NULL) {

  # 1) Adresse direkt übernehmen
  if (identical(area_mode, "address") && !is.null(address) && nzchar(trimws(address))) {
    return(trimws(address))
  }

  # 2) Schwerpunkt (Lon/Lat) bestimmen
  lon <- NA_real_; lat <- NA_real_
  if (!is.null(pt_ll)) {
    cc  <- sf::st_coordinates(pt_ll)
    lon <- cc[1, 1]; lat <- cc[1, 2]
  } else if (!is.null(vect_25832)) {
    cll <- tryCatch(terra::project(vect_25832, "EPSG:4326"), error = function(e) NULL)
    if (!is.null(cll)) {
      cen <- tryCatch(terra::crds(terra::centroids(cll)), error = function(e) NULL)
      if (!is.null(cen)) { lon <- cen[1, 1]; lat <- cen[1, 2] }
    }
  }
  if (is.na(lon) || is.na(lat)) return(NA_character_)

  # 3) Reverse-Geocoding (OSM/Nominatim)
  rev <- tryCatch(
    tidygeocoder::reverse_geocode(
      tibble::tibble(lat_ = lat, long_ = lon),
      lat = lat_, long = long_, method = "osm", full_results = FALSE
    ),
    error = function(e) NULL
  )

  if (!is.null(rev) && "address" %in% names(rev) &&
      !is.na(rev$address[1]) && nzchar(rev$address[1])) {
    return(short_location(rev$address[1]))
  }

  sprintf("%.4f, %.4f (Lon/Lat)", lon, lat)
}

# ---- KOSTRA: API-Abruf ----
get_kostra_data <- function(index = NULL, x = NULL, y = NULL, epsg = 25832,
                            key = Sys.getenv("KOSTRA_KEY")) {
  
  if (!is.null(index)) {
    url <- sprintf("https://dva3.de/kostra-rest/get_values_by_index/?index=%s", index)
  } else if (!is.null(x) && !is.null(y)) {
    url <- sprintf(
      "https://dva3.de/kostra-rest/get_values_by_coordinate/?x=%f&y=%f&epsg=%d",
      x, y, epsg
    )
  } else {
    stop("Bitte entweder 'index' oder ('x','y') angeben.")
  }
  
  resp <- httr::GET(
    url,
    httr::add_headers(
      "accept"  = "application/json",
      "api-key" = key
    )
  )
  
  if (httr::status_code(resp) != 200L) {
    stop("KOSTRA API Fehler: ",
         httr::content(resp, "text", encoding = "UTF-8"))
  }
  
  js <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  
  # JSON → DataFrame
  rows <- lapply(names(js), function(dur) {
    block <- js[[dur]]  # Liste: Code → Wert
    data.frame(
      dauer_min = as.numeric(dur),
      code      = names(block),
      value     = as.numeric(unlist(block)),
      stringsAsFactors = FALSE
    )
  })
  
  df <- data.table::rbindlist(rows)
  
  # Typ + Jährlichkeit + Einheit
  df[, typ := substr(code, 1, 2)]                # HN / RN / UC
  df[, T_Jahre := as.numeric(sub(".*_(\\d+)A$", "\\1", code))]
  df[, einheit := fifelse(
    typ == "HN", "mm",
    fifelse(typ == "RN", "l/(s·ha)", "±%")
  )]
  df[, code := NULL]
  setcolorder(df, c("dauer_min", "typ", "T_Jahre", "value", "einheit"))
  df[]
}

# ---- Starkregenindex (SRI) nach Schmitt et al. 2018 ----
# Farbskala 1..12 grün -> gelb -> rot -> violett, 0 = kein Starkregen
SRI_COLORS <- c(
  "0"  = "#cccccc",
  "1"  = "#ACE438", "2"  = "#C8FEA0", "3"  = "#E3FF5D", "4"  = "#FDF63A",
  "5"  = "#FEA229", "6"  = "#FF6122", "7"  = "#FF3E1F", "8"  = "#F63743",
  "9"  = "#FF3666", "10" = "#F82E82", "11" = "#EB2EBE", "12" = "#BF26D9"
)
# Textfarbe (weiß auf den kräftigen Feldern ab SRI 6)
SRI_TEXTCOL <- c(
  "0"  = "#1b1b1b",
  "1"  = "#1b1b1b", "2"  = "#1b1b1b", "3"  = "#1b1b1b", "4"  = "#1b1b1b",
  "5"  = "#1b1b1b", "6"  = "#ffffff", "7"  = "#ffffff", "8"  = "#ffffff",
  "9"  = "#ffffff", "10" = "#ffffff", "11" = "#ffffff", "12" = "#ffffff"
)

sri_category <- function(sri) {
  if (length(sri) == 0 || is.na(sri))      return(NA_character_)
  if (sri == 0)      "kein Starkregen (< 1 a)"
  else if (sri <= 2) "Starkregen"
  else if (sri <= 4) "intensiver Starkregen"
  else if (sri <= 7) "außergewöhnlicher Starkregen"
  else               "extremer Starkregen"
}

# Wiederkehrzeit -> SRI 1..7
sri_from_T <- function(T) {
  if (is.na(T)) return(NA_real_)
  if (T >= 100) 7
  else if (T >= 50) 6
  else if (T >= 30) 5
  else if (T >= 20) 4
  else if (T >= 10) 3
  else if (T >= 3)  2
  else              1
}

# Extrapolationsfaktor (hN/hN_100a) -> SRI 7..12
sri_from_factor <- function(f) {
  if (is.na(f)) return(NA_real_)
  if (f >= 2.80) 12
  else if (f >= 2.20) 11
  else if (f >= 1.60) 10
  else if (f >= 1.40) 9
  else if (f >= 1.20) 8
  else 7
}

# SRI für eine Dauerstufe bestimmen.
# hN_obs : beobachtete Niederschlagshöhe [mm]
# hn_by_T: benannter Vektor KOSTRA-HN, Namen = Jährlichkeit (z. B. "1","2",...,"100")
compute_sri <- function(hN_obs, hn_by_T) {
  if (is.na(hN_obs) || length(hn_by_T) == 0) {
    return(list(sri = NA_real_, t_label = NA_character_))
  }
  Ts <- sort(as.numeric(names(hn_by_T)))
  hn <- as.numeric(hn_by_T[as.character(Ts)])
  hn100 <- if ("100" %in% names(hn_by_T)) as.numeric(hn_by_T["100"]) else max(hn, na.rm = TRUE)

  if (hN_obs < hn[1]) {
    return(list(sri = 0, t_label = "< 1 a"))
  }
  if (!is.na(hn100) && hN_obs >= hn100) {
    f <- hN_obs / hn100
    return(list(sri = sri_from_factor(f), t_label = sprintf("> 100 a (Faktor %.2f)", f)))
  }
  reached <- max(Ts[hn <= hN_obs], na.rm = TRUE)
  list(sri = sri_from_T(reached), t_label = sprintf("%g a", reached))
}

# ---- Wiederverwendbare Plot-/Tabellen-Bausteine (App + Bericht) ----

# SRI je Dauerstufe (HydroStorm-Maxima vs. KOSTRA-HN) -> data.frame oder NULL
hydro_sri_table <- function(result, kostra, durations) {
  if (is.null(result) || is.null(kostra) || is.null(durations)) return(NULL)
  k <- data.table::as.data.table(kostra)
  k <- k[typ == "HN"]
  if (nrow(k) == 0) return(NULL)

  rows <- lapply(durations, function(d) {
    col <- paste0("D = ", d, " min [mm]")
    if (!(col %in% names(result))) return(NULL)
    hN_obs <- suppressWarnings(max(result[[col]], na.rm = TRUE))
    if (!is.finite(hN_obs)) return(NULL)
    kd <- k[dauer_min == d]
    if (nrow(kd) == 0) return(NULL)
    hn_by_T <- stats::setNames(as.numeric(kd$value), as.character(kd$T_Jahre))
    res <- compute_sri(hN_obs, hn_by_T)
    data.frame(
      dauer_min  = d,
      hN_obs     = round(hN_obs, 1),
      wiederkehr = res$t_label,
      sri        = res$sri,
      kategorie  = sri_category(res$sri),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df
}

# Zeitreihenplot (HydroStorm) -> ggplot oder NULL
hydro_ts_plot <- function(result, dur, product = "", location = "unbekannt") {
  if (is.null(result)) return(NULL)
  dur   <- as.integer(dur)
  cname <- paste0("D = ", dur, " min [mm]")
  if (!(cname %in% names(result))) return(NULL)

  df <- data.frame(
    Zeit  = as.POSIXct(result$Zeit),
    value = as.numeric(result[[cname]])
  )
  ggplot2::ggplot(df, ggplot2::aes(
    x = Zeit, y = value, group = 1,
    text = paste0("Zeit: ", format(Zeit, "%Y-%m-%d %H:%M"),
                  "<br>Niederschlag: ", round(value, 2), " mm")
  )) +
    ggplot2::geom_line(linewidth = 0.6, color = "#004E7C") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9)
    ) +
    ggplot2::labs(
      x = "Zeit (UTC)", y = "Niederschlag [mm]",
      title = sprintf("HydroStorm – Dauerstufe %d min", dur),
      subtitle = paste(strwrap(sprintf(
        "Ort: %s | Produkt %s | Zeitraum %s – %s",
        location, product, format(min(df$Zeit)), format(max(df$Zeit))
      ), width = 70), collapse = "\n")
    )
}

# Dauerstufen-Vergleichsplot HydroStorm vs. KOSTRA (optional SRI-Einfärbung) -> ggplot oder NULL
hydro_kostra_plot <- function(result, kostra, durations, kostra_T,
                              location = "unbekannt", use_sri = TRUE) {
  if (is.null(result) || is.null(kostra) || is.null(durations) || length(kostra_T) == 0) {
    return(NULL)
  }

  ds_cols      <- paste0("D = ", durations, " min [mm]")
  keep         <- ds_cols %in% names(result)
  durations_hs <- durations[keep]
  ds_cols      <- ds_cols[keep]
  if (length(durations_hs) == 0) return(NULL)

  maxvals <- sapply(ds_cols, function(col) max(result[[col]], na.rm = TRUE))
  df_hs <- data.frame(
    dauer_min = durations_hs,
    value     = round(maxvals, 1),
    source    = "HydroStorm",
    stringsAsFactors = FALSE
  )

  df_k <- subset(kostra, typ == "HN" & T_Jahre %in% kostra_T)
  if (nrow(df_k) == 0) return(NULL)
  df_k$value  <- round(df_k$value, 1)
  df_k$source <- paste0("KOSTRA T=", df_k$T_Jahre, " a")
  df_k <- df_k[order(df_k$dauer_min, df_k$T_Jahre), ]

  breaks_x <- c(5, 10, 30, 60, 120, 360, 720, 1440, 2880, 5760, 10080)
  base_theme <- ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      axis.text.x   = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title    = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9)
    )
  sub <- paste(strwrap(sprintf("Ort: %s", location), width = 70), collapse = "\n")

  sri_df <- if (isTRUE(use_sri)) hydro_sri_table(result, kostra, durations) else NULL

  if (isTRUE(use_sri) && !is.null(sri_df)) {
    df_hs$sri   <- sri_df$sri[match(df_hs$dauer_min, sri_df$dauer_min)]
    df_hs$sri_f <- factor(df_hs$sri, levels = 0:12)

    ggplot2::ggplot() +
      ggplot2::geom_line(
        data = df_k,
        ggplot2::aes(x = dauer_min, y = value, color = source, group = source,
                     text = paste0(source, "<br>Dauerstufe: ", dauer_min,
                                   " min<br>Niederschlagshöhe: ", round(value, 1), " mm")),
        linewidth = 0.6
      ) +
      ggplot2::geom_line(
        data = df_hs, ggplot2::aes(x = dauer_min, y = value),
        color = "grey30", linewidth = 0.6
      ) +
      ggplot2::geom_point(
        data = df_hs,
        ggplot2::aes(x = dauer_min, y = value, fill = sri_f,
                     text = paste0("HydroStorm<br>Dauerstufe: ", dauer_min,
                                   " min<br>Niederschlagshöhe: ", round(value, 1),
                                   " mm<br>SRI ", sri)),
        shape = 21, size = 2.8, color = "grey20", stroke = 0.5
      ) +
      ggplot2::scale_fill_manual(values = SRI_COLORS, drop = TRUE, name = "SRI") +
      ggplot2::scale_x_log10(breaks = breaks_x) +
      base_theme +
      ggplot2::labs(
        x = "Dauerstufe [min] (log-Skala)", y = "Niederschlagshöhe [mm]",
        title = "HydroStorm vs. KOSTRA (Punkte = SRI)", subtitle = sub, color = "KOSTRA"
      )
  } else {
    df_all <- rbind(
      df_hs[, c("dauer_min", "value", "source")],
      df_k[,  c("dauer_min", "value", "source")]
    )
    ggplot2::ggplot(df_all, ggplot2::aes(
      x = dauer_min, y = value, color = source, group = source,
      text = paste0(source, "<br>Dauerstufe: ", dauer_min,
                    " min<br>Niederschlagshöhe: ", round(value, 1), " mm")
    )) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::geom_point(size = 1.2) +
      ggplot2::scale_x_log10(breaks = breaks_x) +
      base_theme +
      ggplot2::labs(
        x = "Dauerstufe [min] (log-Skala)", y = "Niederschlagshöhe [mm]",
        title = "HydroStorm vs. KOSTRA", subtitle = sub, color = "Datenquelle"
      )
  }
}