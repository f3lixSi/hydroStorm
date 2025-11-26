library(terra)
library(data.table)

# Deine "Referenz-Pipeline" (vereinfacht auf eine Datei)
radklim_ref_series <- function(nc_path, shp_path, t_start, t_end) {
  r <- terra::rast(nc_path)
  terra::crs(r)  <- "+proj=stere +lat_0=90.0 +lon_0=10.0 +lat_ts=60.0 +a=6370040 +b=6370040 +units=m"
  terra::ext(r)  <- c(-443462,456538,-4758645,-3658645)
  
  shp   <- terra::vect(shp_path)          # Shape in EPSG:25832 o.ä.
  shp_s <- terra::project(shp, terra::crs(r))  # wie in deinem Skript
  
  tvals <- terra::time(r)
  t_start <- as.POSIXct(t_start, tz = "UTC")
  t_end   <- as.POSIXct(t_end,   tz = "UTC")
  keep <- which(tvals >= t_start & tvals <= t_end)
  r_sub  <- r[[keep]]
  t_sub  <- tvals[keep]
  
  vals <- terra::extract(r_sub, shp_s, fun = max, na.rm = TRUE, ID = FALSE)
  max_mm <- as.numeric(vals[1, ])
  
  data.table(
    datetime = as.POSIXct(t_sub, tz = "UTC"),
    max_mm   = max_mm
  )
}

# HydroStorm-Logik in "nackt"
radklim_app_style <- function(nc_path, shp_path, t_start, t_end) {
  # Hier benutzen wir genau deine aktuelle read_radklim_nc()
  r <- read_radklim_nc(
    path   = nc_path,
    fname  = basename(nc_path),
    t_start = t_start,
    t_end   = t_end
  )
  
  shp   <- terra::vect(shp_path)
  shp_s <- terra::project(shp, terra::crs(r))
  
  tvals <- attr(r, "hydrostorm_time")
  vals  <- terra::extract(r, shp_s, fun = max, na.rm = TRUE, ID = FALSE)
  max_mm <- as.numeric(vals[1, ])
  
  data.table(
    datetime = as.POSIXct(tvals, tz = "UTC"),
    max_mm   = max_mm
  )
}

# Ober-Funktion: vergleicht beide Serien 1:1
compare_radklim_series <- function(nc_path, shp_path, t_start, t_end) {
  cat("Lese Referenz-Pipeline …\n")
  ref <- radklim_ref_series(nc_path, shp_path, t_start, t_end)
  
  cat("Lese App-Pipeline …\n")
  app <- radklim_app_style(nc_path, shp_path, t_start, t_end)
  
  # Sicherstellen, dass die Zeitachsen gleich sind
  if (!all.equal(ref$datetime, app$datetime)) {
    cat("⚠️ Datetime-Vektoren unterscheiden sich!\n")
    print(head(data.table(ref = ref$datetime, app = app$datetime), 20))
  } else {
    cat("✅ Datetime-Vektoren sind identisch.\n")
  }
  
  # Differenzen der Maxima
  diff <- ref$max_mm - app$max_mm
  cat("Erste 10 Werte (ref vs. app vs. diff):\n")
  print(head(data.table(datetime = ref$datetime,
                        ref = ref$max_mm,
                        app = app$max_mm,
                        diff = diff), 20))
  
  cat("\nZusammenfassung der Differenzen:\n")
  print(summary(diff))
  
  idx <- which(abs(diff) > 1e-6)
  if (length(idx) == 0) {
    cat("\n🎯 Serien sind numerisch identisch.\n")
  } else {
    cat("\n❗ Es gibt ", length(idx), " Zeitpunkte mit Abweichungen > 1e-6.\n", sep = "")
    cat("Beispiele:\n")
    print(head(data.table(datetime = ref$datetime[idx],
                          ref = ref$max_mm[idx],
                          app = app$max_mm[idx],
                          diff = diff[idx]), 20))
  }
  
  invisible(list(ref = ref, app = app, diff = diff))
}