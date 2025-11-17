# R/modules/mod_utils.R
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# Precip column finder
find_precip_col <- function(cols){
  cands <- c("rain","precip","prec","prcp","rf")
  hit <- cands[cands %in% cols]
  if (length(hit)) hit[1] else NULL
}

# Shinylive/Chromium download workaround (strip native download attr)
downloadButton_sl <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}
downloadLink_sl <- function(...) {
  tag <- shiny::downloadLink(...)
  tag$attribs$download <- NULL
  tag
}

# Nearest-to-noon record per local day (optionally per station id)
# data.table version (no dplyr/rlang)
nearest_noon_per_day <- function(df, dt_col = "datetime", hour_col = "hour",
                                 tz = "UTC", id_col = NULL){
  stopifnot(dt_col %in% names(df), hour_col %in% names(df))
  
  # Work on a copy to avoid by-reference side effects on reactive data
  DT <- data.table::as.data.table(data.table::copy(df))
  
  # Local civil date and absolute distance to noon
  DT[, date_local := as.Date(get(dt_col), tz = tz)]
  DT[, dist := abs(get(hour_col) - 12)]
  
  # Select the closest-to-noon row per group
  if (!is.null(id_col) && id_col %in% names(DT)) {
    data.table::setorder(DT, dist)
    ans <- DT[, .SD[1L], by = .(grp = get(id_col), date_local)]
    data.table::setnames(ans, "grp", id_col)
  } else {
    data.table::setorder(DT, dist)
    ans <- DT[, .SD[1L], by = .(date_local)]
  }
  
  # Drop helper column(s) and return as data.table (data.frame-compatible)
  ans[, dist := NULL]
  ans[]
}

# Time zone helpers
parse_z_to_hours <- function(z_txt){
  z_txt <- as.character(z_txt)
  z_txt <- z_txt[nzchar(z_txt)]
  if (!length(z_txt)) return(NA_real_)
  sgn <- ifelse(substr(z_txt, 1, 1) == "-", -1, 1)
  hh <- suppressWarnings(as.integer(substr(z_txt, 2, 3)))
  mm <- suppressWarnings(as.integer(substr(z_txt, 4, 5)))
  sgn * (hh + (mm/60))
}
tz_standard_offset_hours <- function(tz, probe_date = "2025-01-15 12:00:00"){
  probe <- as.POSIXct(probe_date, tz = tz)
  parse_z_to_hours(format(probe, "%z"))
}
tz_modal_offset_hours <- function(datetimes){
  z_txt <- format(datetimes, "%z")
  z_txt <- z_txt[nzchar(z_txt)]
  if (!length(z_txt)) stop("Could not infer modal offset: empty %z values.")
  z_mode <- names(which.max(table(z_txt)))
  list(offset = parse_z_to_hours(z_mode), z_mode = z_mode)
}
