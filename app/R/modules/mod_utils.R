# R/modules/mod_utils.R
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# Simple GoC theme for ggplot (GCDS colours/fonts)
theme_goc <- function(base_size = 12, base_family = "Noto Sans") {
  primary <- "#26374A" # GCDS primary background
  border  <- "#7D828B" # GCDS border default
  textcol <- "#333333" # GCDS text primary
  stripbg <- "#F1F2F3" # GCDS light background
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", colour = textcol),
      plot.subtitle= ggplot2::element_text(colour = textcol),
      plot.caption = ggplot2::element_text(colour = textcol),
      axis.title   = ggplot2::element_text(face = "bold", colour = textcol),
      axis.text    = ggplot2::element_text(colour = textcol),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = border, linewidth = 0.25),
      strip.text   = ggplot2::element_text(face = "bold", colour = textcol, family = "Lato"),
      strip.background = ggplot2::element_rect(fill = stripbg, colour = border, linewidth = 0.5)
    )
}

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
nearest_noon_per_day <- function(df, dt_col = "datetime", hour_col = "hour",
                                 tz = "UTC", id_col = NULL){
  stopifnot(dt_col %in% names(df), hour_col %in% names(df))
  df$date_local <- as.Date(df[[dt_col]], tz = tz)
  
  # If an id column is provided and exists, do it per (id, date_local)
  if (!is.null(id_col) && id_col %in% names(df)) {
    id_sym <- rlang::sym(id_col)
    dplyr::group_by(df, !!id_sym, .data$date_local) |>
      dplyr::slice_min(abs(.data[[hour_col]] - 12), with_ties = FALSE) |>
      dplyr::ungroup()
  } else {
    dplyr::group_by(df, .data$date_local) |>
      dplyr::slice_min(abs(.data[[hour_col]] - 12), with_ties = FALSE) |>
      dplyr::ungroup()
  }
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
