# ─────────────────────────────────────────────
# utils.R — RADKLIM & KOSTRA
# ─────────────────────────────────────────────

library(terra)
library(httr)
library(jsonlite)
library(data.table)

# ---- RADKLIM: netCDF einlesen ----
# ---- RADKLIM: netCDF einlesen ----
read_radklim_nc <- function(path, t_start = NULL, t_end = NULL, fname = NULL) {
  # WICHTIG: Dateiname für Produkt-Erkennung
  ref_name <- if (!is.null(fname)) basename(fname) else basename(path)
  message("Lese RADKLIM-Datei: ", ref_name)
  
  r <- terra::rast(path)
  
  # CRS & Extent wie in deinem Skript
  terra::crs(r) <- "+proj=stere +lat_0=90.0 +lon_0=10.0 +lat_ts=60.0 +a=6370040 +b=6370040 +units=m"
  terra::ext(r) <- c(-443462, 456538, -4758645, -3658645)
  
  # Zeitachse
  tvals <- tryCatch(terra::time(r), error = function(e) NULL)
  if (is.null(tvals) || all(is.na(tvals))) {
    dstr <- gsub("\\D", "", ref_name)
    if (nchar(dstr) >= 8) {
      day <- as.Date(substr(dstr, nchar(dstr) - 7, nchar(dstr)), "%Y%m%d")
      tvals <- seq.POSIXt(day, day + 86340, by = "5 min")
    } else {
      tvals <- seq_len(terra::nlyr(r))
    }
  }
  
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
  
  # Nodata ersetzen
  if (!is.na(fv) && is.finite(fv)) r[r == fv] <- NA
  if (!is.na(mv) && is.finite(mv)) r[r == mv] <- NA
  
  # Produkt & Zeitschritt **auf Basis des echten Dateinamens**
  prod   <- if (grepl("YW", ref_name, ignore.case = TRUE)) "YW2017.002" else "RW2017.002"
  dt_min <- if (grepl("YW", prod)) 5L else 60L
  
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