# R/modules/mod_filter.R
# Filter module that provides a robust start_date (true Date or NULL),
# eliminating the "Couldn't coerce ... to yyyy-mm-dd" warning in Shiny.

# --- Utilities ---------------------------------------------------------------

# Safely coerce a vector to Date using base R only.
safe_to_date <- function(x, tz = "UTC", origin = "1970-01-01") {
  # Already a Date?
  if (inherits(x, "Date")) {
    return(x)
  }
  # POSIXt -> Date in tz
  if (inherits(x, "POSIXt")) {
    return(as.Date(x, tz = tz))
  }
  # Factor -> character
  if (is.factor(x)) x <- as.character(x)

  # Numeric: could be seconds/ms since epoch, or days since origin
  if (is.numeric(x)) {
    ax <- abs(x[is.finite(x)])
    if (!length(ax)) {
      return(as.Date(rep(NA, length(x))))
    }
    m <- stats::median(ax, na.rm = TRUE)
    if (m > 1e11) {
      return(as.Date(as.POSIXct(x / 1000, origin = origin, tz = tz)))
    } else if (m > 1e9) {
      return(as.Date(as.POSIXct(x, origin = origin, tz = tz)))
    } else {
      return(as.Date(x, origin = origin))
    }
  }

  # Character: try POSIX first (more forgiving), then Date formats
  if (is.character(x)) {
    try_posix <- suppressWarnings(as.POSIXct(
      x,
      tz = tz,
      tryFormats = c(
        "%Y-%m-%d %H:%M:%OS",
        "%Y/%m/%d %H:%M:%OS",
        "%Y-%m-%dT%H:%M:%OS",
        "%Y-%m-%dT%H:%M:%OSZ",
        "%Y-%m-%d %H:%M",
        "%Y/%m/%d %H:%M"
      )
    ))
    d <- as.Date(try_posix, tz = tz)
    na_idx <- which(is.na(d))
    if (length(na_idx)) {
      d2 <- suppressWarnings(as.Date(
        x[na_idx],
        tryFormats = c(
          "%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%m/%d/%Y",
          "%Y%m%d", "%Y-%j" # day-of-year
        )
      ))
      d[na_idx] <- d2
    }
    return(d)
  }

  as.Date(rep(NA, length(x)))
}

# Build Date from y/m/d vectors safely
build_date_from_ymd <- function(y, m, d) {
  yi <- suppressWarnings(as.integer(y))
  mi <- suppressWarnings(as.integer(m))
  di <- suppressWarnings(as.integer(d))
  suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yi, mi, di)))
}

# Derive min/max date available in a data.frame using either a datetime column,
# separate y/m/d, or auto-detected "date/time" candidates.
minmax_date <- function(df,
                        dt_col = NULL, # name of datetime column (optional)
                        ycol = NULL, mcol = NULL, dcol = NULL, # optional y/m/d
                        tz = "UTC", origin = "1970-01-01") {
  d <- NULL

  # 1) datetime column if provided & present
  if (!is.null(dt_col) && nzchar(dt_col) && (dt_col %in% names(df))) {
    d <- safe_to_date(df[[dt_col]], tz = tz, origin = origin)
  }

  # 2) y/m/d fallback if datetime unavailable or unparsable
  if (is.null(d) || all(is.na(d))) {
    if (!is.null(ycol) && !is.null(mcol) && !is.null(dcol) &&
      ycol %in% names(df) && mcol %in% names(df) && dcol %in% names(df)) {
      d <- build_date_from_ymd(df[[ycol]], df[[mcol]], df[[dcol]])
    }
  }

  # 3) last resort: look for likely date-like columns
  if (is.null(d) || all(is.na(d))) {
    candidates <- grep(
      pattern = "date|time|dt|_dt$|_ts$|timestamp",
      x = names(df),
      ignore.case = TRUE,
      value = TRUE
    )
    for (cand in candidates) {
      d_try <- safe_to_date(df[[cand]], tz = tz, origin = origin)
      if (!all(is.na(d_try))) {
        d <- d_try
        break
      }
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

# Simple label+help wrapper (expects a function label_with_help() in your app)
# Fallback to plain text if not available.
label_with_help_safe <- function(label_text, help_text = NULL) {
  if (exists("label_with_help", mode = "function")) {
    label_with_help(label_text, help_text)
  } else {
    if (is.null(help_text) || !nzchar(help_text)) label_text else paste0(label_text, " — ", help_text)
  }
}

# --- UI ----------------------------------------------------------------------

mod_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5(id = ns("lbl_filter"), uiOutput(ns("lbl_filter"))),
    div(
      role = "group",
      `aria-labelledby` = ns("lbl_start_date"),
      uiOutput(ns("lbl_start_date")),
      # IMPORTANT: use value = NULL (not NA/"") to avoid warnings on init/reset
      dateInput(ns("start_date"), label = NULL, value = NULL, width = "240px")
    ),
    tags$p(
      id = ns("help_drop_rows"),
      class = "help-block",
      textOutput(ns("txt_drop_rows_help"))
    )
  )
}

# --- Server ------------------------------------------------------------------

# tr   : i18n translation function (string -> localized string)
# tz   : timezone module list providing tz$tz_use()
# raw_file : reactive data.frame (CSV load)
# mapping  : mapping module list providing column names (col_datetime, col_year, etc.)
mod_filter_server <- function(id, tr, tz, raw_file, mapping) {
  moduleServer(id, function(input, output, session) {
    # --- i18n text outputs ---
    output$lbl_filter <- renderUI({
      label_with_help_safe(tr("filter"), tr("drop_rows_help"))
    })

    output$lbl_start_date <- renderUI({
      tags$label(
        `for` = session$ns("start_date"),
        label_with_help_safe(tr("drop_rows_prior"), tr("tt_start_date"))
      )
    })

    output$txt_drop_rows_help <- renderText(tr("drop_rows_help"))

    # Helper to find a column by keyword(s), case-insensitive
    find_col <- function(cols, keywords) {
      rx <- paste0("^(", paste(keywords, collapse = "|"), ")$")
      m <- cols[grepl(rx, cols, ignore.case = TRUE)]
      if (length(m) > 0) m[1] else ""
    }

    # Prefill start_date from the minimum date we can infer
    observeEvent(raw_file(),
      {
        df <- raw_file()
        req(!is.null(df), NROW(df) > 0)

        cols <- names(df)
        # Common candidates for datetime
        dt_col <- find_col(cols, c("datetime", "timestamp", "obs_time", "date"))
        ycol <- find_col(cols, c("year", "yr"))
        mcol <- find_col(cols, c("month", "mon"))
        dcol <- find_col(cols, c("day", "dy"))

        tz_use <- if (!is.null(tz) && is.reactive(tz$tz_use)) tz$tz_use() else "UTC"
        rng <- minmax_date(
          df,
          dt_col = if (nzchar(dt_col)) dt_col else NULL,
          ycol   = if (nzchar(ycol)) ycol else NULL,
          mcol   = if (nzchar(mcol)) mcol else NULL,
          dcol   = if (nzchar(dcol)) dcol else NULL,
          tz     = tz_use
        )

        # Update only when we have a valid min date; otherwise, leave as NULL
        if (!is.na(rng$min)) {
          updateDateInput(
            session, "start_date",
            value = rng$min, # set the first available date
            min = rng$min, # optional UX bounds
            max = rng$max
          )
        } else {
          # Clear (sets to NULL; avoids "" which triggers the yyyy-mm-dd warning)
          updateDateInput(session, "start_date", value = NULL)
        }
      },
      ignoreInit = FALSE
    )

    # Public reactive: always return a proper Date or NULL
    # (Never return a length-0 character or "")
    start_date_clean <- reactive({
      val <- input$start_date
      if (is.null(val)) {
        return(NULL)
      }
      # Shiny dateInput usually returns Date already; these guards add resilience.
      if (inherits(val, "Date")) {
        return(val)
      }
      if (inherits(val, "POSIXt")) {
        return(as.Date(val, tz = if (!is.null(tz) && is.reactive(tz$tz_use)) tz$tz_use() else "UTC"))
      }
      if (is.character(val)) {
        if (!nzchar(val)) {
          return(NULL)
        }
        return(as.Date(val)) # expects yyyy-mm-dd
      }
      # Fallback
      suppressWarnings(as.Date(val))
    })

    return(list(
      start_date = start_date_clean
    ))
  })
}
