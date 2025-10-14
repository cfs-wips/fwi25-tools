# R/modules/mod_filter.R
# Safely parse a vector into Date (no external packages)
safe_to_date <- function(x, tz = "UTC", origin = "1970-01-01") {
  # Early exits for common classes
  if (inherits(x, "Date"))    return(x)
  if (inherits(x, "POSIXt"))  return(as.Date(x, tz = tz))
  
  # Coerce factors to character
  if (is.factor(x)) x <- as.character(x)
  
  # Numeric handling: seconds/ms epochs or days-since-origin
  if (is.numeric(x)) {
    ax <- abs(x[is.finite(x)])
    if (!length(ax)) return(as.Date(rep(NA, length(x))))
    # Heuristic: milliseconds if typical magnitude > 1e11; seconds if > 1e9; else days
    m <- stats::median(ax, na.rm = TRUE)
    if (m > 1e11) {
      return(as.Date(as.POSIXct(x / 1000, origin = origin, tz = tz)))
    } else if (m > 1e9) {
      return(as.Date(as.POSIXct(x, origin = origin, tz = tz)))
    } else {
      # Assume "days since origin"
      return(as.Date(x, origin = origin))
    }
  }
  
  # Character handling: try POSIX first (most flexible), then Date
  if (is.character(x)) {
    # Try common POSIX formats
    try_posix <- suppressWarnings(
      as.POSIXct(
        x, tz = tz,
        tryFormats = c(
          "%Y-%m-%d %H:%M:%OS",
          "%Y/%m/%d %H:%M:%OS",
          "%Y-%m-%dT%H:%M:%OS",     # ISO 8601 (without Z)
          "%Y-%m-%dT%H:%M:%OSZ",    # ISO 8601 (with Z)
          "%Y-%m-%d %H:%M",
          "%Y/%m/%d %H:%M"
        )
      )
    )
    d <- as.Date(try_posix, tz = tz)
    
    # Fill remaining NAs via Date-only formats
    na_idx <- which(is.na(d))
    if (length(na_idx)) {
      d2 <- suppressWarnings(
        as.Date(
          x[na_idx],
          tryFormats = c(
            "%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%m/%d/%Y",
            "%Y%m%d",   "%Y-%j"     # %j = day of year
          )
        )
      )
      d[na_idx] <- d2
    }
    return(d)
  }
  
  # Fallback
  as.Date(rep(NA, length(x)))
}

# Build dates from year/month/day columns
build_date_from_ymd <- function(y, m, d) {
  yi <- suppressWarnings(as.integer(y))
  mi <- suppressWarnings(as.integer(m))
  di <- suppressWarnings(as.integer(d))
  suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yi, mi, di)))
}

# Main helper: get min/max date from a df
minmax_date <- function(df,
                        dt_col = NULL,     # name of datetime column (optional)
                        ycol = NULL, mcol = NULL, dcol = NULL,  # optional y/m/d
                        tz = "UTC", origin = "1970-01-01") {
  # 1) Try a provided/guessed datetime column
  d <- NULL
  if (!is.null(dt_col) && nzchar(dt_col) && dt_col %in% names(df)) {
    d <- safe_to_date(df[[dt_col]], tz = tz, origin = origin)
  }
  
  # 2) If that failed or not provided, try separate year/month/day if present
  if (is.null(d) || all(is.na(d))) {
    if (!is.null(ycol) && !is.null(mcol) && !is.null(dcol) &&
        ycol %in% names(df) && mcol %in% names(df) && dcol %in% names(df)) {
      d <- build_date_from_ymd(df[[ycol]], df[[mcol]], df[[dcol]])
    }
  }
  
  # 3) As a last resort, try to auto-detect a plausible date-like column
  if (is.null(d) || all(is.na(d))) {
    # simple heuristic: columns whose names mention date/time or end with _dt/_ts
    candidates <- grep("date|time|dt|_dt$|_ts$|timestamp", names(df), ignore.case = TRUE, value = TRUE)
    for (cand in candidates) {
      d_try <- safe_to_date(df[[cand]], tz = tz, origin = origin)
      if (!all(is.na(d_try))) { d <- d_try; break }
    }
  }
  
  if (is.null(d) || all(is.na(d))) {
    return(list(min = as.Date(NA), max = as.Date(NA)))
  }
  
  list(
    min = suppressWarnings(min(d, na.rm = TRUE)),
    max = suppressWarnings(max(d, na.rm = TRUE))
  )
}

mod_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(id = ns("lbl_filter"), uiOutput(ns("lbl_filter"))),
    
    div(
      role = "group",
      `aria-labelledby` = ns("lbl_start_date"),
      uiOutput(ns("lbl_start_date")),
      dateInput(ns("start_date"), label = NULL, value = NA)
    ),
    
    tags$p(
      id = ns("help_drop_rows"),
      class = "help-block",
      textOutput(ns("txt_drop_rows_help"))
    )
  )
}

mod_filter_server <- function(id, tr, raw_file, mapping) {
  moduleServer(id, function(input, output, session) {
    
    output$lbl_filter <- renderUI({
      label_with_help(tr("filter"), tr("drop_rows_help"))
    })
    
    output$lbl_start_date <- renderUI({
      tags$label(
        `for` = session$ns("start_date"),
        label_with_help(tr("drop_rows_prior"), tr("tt_start_date"))
      )
    })
    
    output$txt_drop_rows_help <- renderText(tr("drop_rows_help"))
    
    # Robust column finder
    find_col <- function(cols, keywords) {
      rx <- paste0("^(", paste(keywords, collapse = "|"), ")$")
      m  <- cols[grepl(rx, cols, ignore.case = TRUE)]
      if (length(m) > 0) m[1] else ""
    }
    
    # Prefill start_date from first row if possible
    observeEvent(raw_file(), {
      df <- raw_file()
      req(!is.null(df), NROW(df) > 0)
      
      cols <- names(df)
      
      # Prefer your known datetime candidates; add a couple more common aliases
      dt_col <- find_col(cols, c("datetime", "timestamp", "obs_time", "date"))
      
      # y/m/d fallbacks (used by minmax_date if dt_col is missing or unparsable)
      ycol <- find_col(cols, c("year", "yr"))
      mcol <- find_col(cols, c("month", "mon"))
      dcol <- find_col(cols, c("day", "dy"))
      
      rng <- minmax_date(
        df,
        dt_col = if (nzchar(dt_col)) dt_col else NULL,
        ycol   = if (nzchar(ycol))   ycol   else NULL,
        mcol   = if (nzchar(mcol))   mcol   else NULL,
        dcol   = if (nzchar(dcol))   dcol   else NULL,
        tz = "UTC"
      )
      
      # Update only if we successfully found a min date
      if (!is.na(rng$min)) {
        updateDateInput(
          session, "start_date",
          value = rng$min,   # your first guess is the true min
          min   = rng$min,   # optional but improves UX
          max   = rng$max
        )
      } else {
        # Optional: clear or leave as-is. Uncomment to clear.
        # updateDateInput(session, "start_date", value = NULL)
      }
    })
    
    return(list(start_date = reactive(input$start_date)))
  })
}