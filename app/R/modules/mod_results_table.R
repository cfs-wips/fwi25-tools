# R/modules/mod_results_table.R
# -----------------------------------------------------------------------------
# FWI25 Results (hourly) — Results Table Module
# -----------------------------------------------------------------------------

mod_results_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Ensure columns adjust after layout changes
    tags$script(HTML(
      "
      $(function(){
        var adjust = function(){
          try{ $.fn.dataTable.tables({visible:true, api:true}).columns.adjust(); }catch(e){}
        };
        $(document).on('shown.bs.tab shown.bs.collapse', function(){ setTimeout(adjust, 0); });
        $(window).on('resize.dt', adjust);
      });
      "
    )),
    tags$section(
      class = "gc-card",
      tags$div(role = "region", `aria-label` = "FWI25 results table", uiOutput(ns("title"))),
      div(
        class = "gc-card__content",
        div(
          class = "gc-placeholder", `aria-live` = "polite", `aria-busy` = "true",
          div(class = "gc-placeholder__text", uiOutput(ns("hint")))
        ),
        div(
          class = "gc-spin-wrap",
          DT::DTOutput(ns("tbl"), width = "100%"),
          div(
            class = "gc-spin-overlay",
            div(class = "gc-spinner", `aria-hidden` = "true"),
            span(class = "sr-only", "Loading…")
          )
        )
      )
    )
  )
}

#' @param tr translator function
#' @param dt_i18n function providing DT language list
#' @param results reactive data.frame/tibble with typical cols:
#'   id (optional), datetime, timestamp, tz (Olson), timezone (std offset hrs)
#' @param tz_reactive reactive Olson tz fallback (e.g., "America/Edmonton")
#' @param ignore_dst_reactive reactive policy: TRUE => fixed standard offset,
#'   FALSE => civil (DST-aware). Can also be character ("standard"/"civil") or numeric (1/0).
mod_results_table_server <- function(id, tr, dt_i18n, results, tz_reactive,
                                     ignore_dst_reactive = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    # ---- i18n helpers

    i18n_or <- function(key, default) {
      val <- tryCatch(tr(key), error = function(e) NULL)
      if (is.null(val)) {
        return(default)
      }
      val_chr <- as.character(val)
      if (!length(val_chr) || !nzchar(val_chr)) {
        return(default)
      }
      # treat '??key??' as missing and use the default
      if (grepl("^\\?\\?.*\\?\\?$", val_chr)) {
        return(default)
      }
      val_chr
    }


    output$title <- renderUI({
      h4(i18n_or("fwi25_results_title", "FWI25 results"))
    })
    outputOptions(output, "title", suspendWhenHidden = FALSE)
    output$hint <- renderUI({
      i18n_or("hint_run_to_compute", "Click Run to compute results.")
    })
    outputOptions(output, "hint", suspendWhenHidden = FALSE)

    # ---- Policy normalizer & helpers (unchanged core behavior)
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

    normalize_policy <- function(val) {
      tolower_chr <- function(x) {
        x <- as.character(x)
        x[is.na(x)] <- ""
        tolower(trimws(x))
      }
      if (is.logical(val)) {
        return(isTRUE(val))
      }
      if (is.numeric(val)) {
        return(isTRUE(val == 1))
      }
      if (is.character(val)) {
        v <- tolower_chr(val)
        if (v %in% c("standard", "ignore", "ignore_dst", "fixed", "std", "no_dst", "offset_fixed")) {
          return(TRUE)
        }
        if (v %in% c("civil", "dst", "from_data", "data", "olson", "local", "use_dst")) {
          return(FALSE)
        }
      }
      if (isTRUE(getOption("fwi.debug_times", FALSE))) {
        message(
          "FWI25: normalize_policy() fell back to civil for value: ",
          paste0(capture.output(str(val)), collapse = " ")
        )
      }
      FALSE
    }

    has_explicit_offset <- function(x_chr) {
      grepl("(Z|[+\\-]\\d{2}:?\\d{2})$", x_chr %||% "", ignore.case = TRUE)
    }

    parse_mixed_to_utc <- function(x, olson_vec) {
      x <- as.character(x)
      stopifnot(length(x) == length(olson_vec))
      out <- as.POSIXct(rep(NA, length(x)), tz = "UTC")
      ok <- nzchar(x) & !is.na(x)
      if (!any(ok)) {
        return(out)
      }
      xo <- x[ok]
      ols <- olson_vec[ok]
      haso <- has_explicit_offset(xo)
      if (any(haso)) {
        out[ok][haso] <- suppressWarnings(lubridate::ymd_hms(xo[haso], tz = "UTC", quiet = TRUE))
      }
      if (any(!haso)) {
        loc <- suppressWarnings(lubridate::ymd_hms(xo[!haso], tz = ols[!haso], quiet = TRUE))
        out[ok][!haso] <- lubridate::with_tz(loc, "UTC")
      }
      out
    }

    parse_timestamp_to_utc <- function(x) {
      x <- as.character(x)
      out <- as.POSIXct(rep(NA, length(x)), tz = "UTC")
      ok <- nzchar(x) & !is.na(x)
      if (!any(ok)) {
        return(out)
      }
      xo <- x[ok]
      haso <- has_explicit_offset(xo)
      if (any(haso)) {
        out[ok][haso] <- suppressWarnings(lubridate::ymd_hms(xo[haso], tz = "UTC", quiet = TRUE))
      }
      if (any(!haso)) {
        out[ok][!haso] <- suppressWarnings(lubridate::ymd_hms(xo[!haso], tz = "UTC", quiet = TRUE))
      }
      out
    }

    std_off_hours <- function(tz) {
      if (exists("tz_standard_offset_hours", mode = "function")) {
        tz_standard_offset_hours(tz) # if available
      } else {
        ref <- as.POSIXct("2000-01-15 12:00:00", tz = tz)
        z <- format(ref, "%z")
        sgn <- ifelse(substr(z, 1, 1) == "-", -1, 1)
        hh <- suppressWarnings(as.integer(substr(z, 2, 3)))
        mm <- suppressWarnings(as.integer(substr(z, 4, 5)))
        sgn * (hh + mm / 60)
      }
    }

    fixed_tz_from_offset <- function(offset_hours) {
      ifelse(is.finite(offset_hours),
        sprintf("Etc/GMT%+d", -as.integer(offset_hours)),
        NA_character_
      )
    }

    format_iso_with_offset_per_row <- function(x, tz_vec) {
      out <- character(length(x))
      ok <- !is.na(x)
      out[!ok] <- NA_character_
      if (any(ok)) {
        out[ok] <- mapply(function(xx, tz) {
          if (is.na(xx) || is.na(tz) || !nzchar(tz)) {
            return(NA_character_)
          }
          s <- format(lubridate::with_tz(xx, tz), "%Y-%m-%dT%H:%M:%S%z")
          sub("([+\\-]\\d{2})(\\d{2})$", "\\1:\\2", s)
        }, x[ok], tz_vec[ok], USE.NAMES = FALSE)
      }
      out
    }

    hdec_to_hms <- function(hdec) {
      h <- floor(hdec)
      m <- floor((hdec - h) * 60)
      s <- round((hdec - h - m / 60) * 3600)
      h <- pmax(0, pmin(23, h))
      m <- pmax(0, pmin(59, m))
      s <- pmax(0, pmin(59, s))
      sprintf("%02d:%02d:%02d", h, m, s)
    }

    make_utc_from_date_hdec_LST <- function(date_vec, hdec, std_offset_hours) {
      out <- rep(as.POSIXct(NA, tz = "UTC"), length(date_vec))
      ok <- is.finite(hdec) & !is.na(date_vec) & is.finite(std_offset_hours)
      if (!any(ok)) {
        return(out)
      }
      time_str <- hdec_to_hms(hdec[ok])
      date_str <- format(as.Date(date_vec[ok]), "%Y-%m-%d")
      out[ok] <- as.POSIXct(paste0(date_str, " ", time_str), tz = "UTC") - std_offset_hours[ok] * 3600
      out
    }

    add_debug_cols <- function(df, policy, display_tz_vec, std_off, olson_vec,
                               dt_raw, dt_utc, ts_raw, ts_utc) {
      if (!isTRUE(getOption("fwi.debug_times", FALSE))) {
        return(df)
      }
      cbind(
        df,
        `.dbg_policy` = ifelse(policy, "standard", "civil"),
        `.dbg_display_tz_used` = display_tz_vec,
        `.dbg_std_offset_hours` = std_off,
        `.dbg_olson_resolved` = olson_vec,
        `.dbg_datetime_raw` = if (length(dt_raw)) dt_raw else NA,
        `.dbg_datetime_has_offs` = if (length(dt_raw)) grepl("(Z|[+\\-]\\d{2}:?\\d{2})$", as.character(dt_raw)) else NA,
        `.dbg_datetime_parsed_utc` = if (length(dt_utc)) format(dt_utc, "%Y-%m-%dT%H:%M:%SZ") else NA,
        `.dbg_timestamp_raw` = if (length(ts_raw)) ts_raw else NA,
        `.dbg_timestamp_has_offs` = if (length(ts_raw)) grepl("(Z|[+\\-]\\d{2}:?\\d{2})$", as.character(ts_raw)) else NA,
        `.dbg_timestamp_parsed_utc` = if (length(ts_utc)) format(ts_utc, "%Y-%m-%dT%H:%M:%SZ") else NA
      )
    }

    # ---- Data prep & table build ----
    table_data <- reactive({
      df <- results()
      req(df) # <- gates until Run HFWI (eventReactive upstream)
      df <- as.data.frame(df)
      if (!("id" %in% names(df))) df$id <- seq_len(nrow(df))

      # Resolve per-row Olson tz (civil) and STANDARD offsets
      olson_fallback <- tz_reactive()
      if ("tz" %in% names(df) && is.character(df$tz) && any(nzchar(df$tz))) {
        olson_vec <- ifelse(nzchar(df$tz), df$tz, olson_fallback)
      } else {
        olson_vec <- rep(olson_fallback, nrow(df))
      }
      std_off <- if ("timezone" %in% names(df) && is.numeric(df$timezone)) {
        df$timezone
      } else {
        m <- setNames(vapply(unique(olson_vec), std_off_hours, numeric(1)), unique(olson_vec))
        unname(m[olson_vec])
      }

      # Parse datetime/timestamp to UTC
      dt_raw <- if ("datetime" %in% names(df)) df$datetime else NULL
      ts_raw <- if ("timestamp" %in% names(df)) df$timestamp else NULL
      if (is.character(dt_raw)) df$datetime <- parse_mixed_to_utc(dt_raw, olson_vec)
      if (is.character(ts_raw)) df$timestamp <- parse_timestamp_to_utc(ts_raw) # key behavior
      # Establish LST date anchor for numeric sunrise/sunset (prefer datetime)
      date_LST <- NULL
      if ("datetime" %in% names(df) && inherits(df$datetime, c("POSIXct", "POSIXt"))) {
        dt_LST <- df$datetime + std_off * 3600
        date_LST <- as.Date(dt_LST)
      } else if ("date" %in% names(df) && inherits(df$date, "Date")) {
        date_LST <- df$date
      }
      if ("sunrise" %in% names(df) && is.numeric(df$sunrise) && !is.null(date_LST)) {
        df$sunrise_utc <- make_utc_from_date_hdec_LST(date_LST, df$sunrise, std_off)
      }
      if ("sunset" %in% names(df) && is.numeric(df$sunset) && !is.null(date_LST)) {
        df$sunset_utc <- make_utc_from_date_hdec_LST(date_LST, df$sunset, std_off)
      }
      if ("sunrise" %in% names(df) && is.character(df$sunrise)) {
        df$sunrise_utc <- parse_timestamp_to_utc(df$sunrise) # UTC string in your pipeline
      }
      if ("sunset" %in% names(df) && is.character(df$sunset)) {
        df$sunset_utc <- parse_timestamp_to_utc(df$sunset)
      }

      # Choose display tz per row
      ignore_dst <- normalize_policy(ignore_dst_reactive())
      display_tz_vec <- if (ignore_dst) fixed_tz_from_offset(std_off) else olson_vec
      # Build ISO strings with offsets for visible time columns
      datetime_iso <- if ("datetime" %in% names(df) && inherits(df$datetime, c("POSIXct", "POSIXt"))) {
        format_iso_with_offset_per_row(df$datetime, display_tz_vec)
      } else {
        rep(NA_character_, nrow(df))
      }

      timestamp_iso <- if ("timestamp" %in% names(df) && inherits(df$timestamp, c("POSIXct", "POSIXt"))) {
        format_iso_with_offset_per_row(df$timestamp, display_tz_vec)
      } else {
        rep(NA_character_, nrow(df))
      }

      sunrise_iso <- if ("sunrise_utc" %in% names(df) && inherits(df$sunrise_utc, c("POSIXct", "POSIXt"))) {
        format_iso_with_offset_per_row(df$sunrise_utc, display_tz_vec)
      } else {
        rep(NA_character_, nrow(df))
      }

      sunset_iso <- if ("sunset_utc" %in% names(df) && inherits(df$sunset_utc, c("POSIXct", "POSIXt"))) {
        format_iso_with_offset_per_row(df$sunset_utc, display_tz_vec)
      } else {
        rep(NA_character_, nrow(df))
      }

      out <- data.frame(
        id = df$id,
        datetime = datetime_iso,
        timestamp = timestamp_iso,
        sunrise_local = sunrise_iso,
        sunset_local = sunset_iso,
        stringsAsFactors = FALSE
      )
      drop_helpers <- c("sunrise_utc", "sunset_utc", "date")
      passthrough <- setdiff(names(df), c(names(out), drop_helpers))
      out <- cbind(out, df[passthrough])

      # Append optional debug columns
      out <- add_debug_cols(
        out,
        policy = ignore_dst, display_tz_vec = display_tz_vec, std_off = std_off,
        olson_vec = olson_vec, dt_raw = dt_raw,
        dt_utc = if ("datetime" %in% names(df) && inherits(df$datetime, "POSIXt")) df$datetime else NA,
        ts_raw = ts_raw,
        ts_utc = if ("timestamp" %in% names(df) && inherits(df$timestamp, "POSIXt")) df$timestamp else NA
      )
      out
    }) |> bindCache(results(), tz_reactive(), ignore_dst_reactive())

    # ---- Render DT ----
    output$tbl <- DT::renderDT(
      {
        df <- table_data()
        # DataTables callback ensures column widths re-sync after init/draw/resize.
        cb <- DT::JS("
        var tbl = table;          // DataTables API instance
        function adjust(){ try { tbl.columns.adjust(); } catch(e){} }
        setTimeout(adjust, 0);    // after init
        tbl.on('draw.dt', adjust);
        $(window).on('resize.dt', adjust);
      ")

        DT::datatable(
          df,
          rownames = FALSE,
          fillContainer = TRUE,
          escape = TRUE,
          filter = "top",
          class = "display nowrap compact hover stripe gc-dt",
          extensions = c("Buttons", "Scroller"),
          options = list(
            language = dt_i18n(),
            autoWidth = TRUE,
            scrollX = TRUE,
            deferRender = TRUE,
            scroller = TRUE,
            scrollY = 300,
            scrollCollapse = TRUE,
            pageLength = 25,
            lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
            dom = "Bfrtip",
            buttons = list(
              list(extend = "copy", text = tr("dt_btn_copy")),
              list(extend = "csv", text = tr("dt_btn_csv"), filename = "hFWI"),
              list(extend = "excel", text = tr("dt_btn_excel"), filename = "hFWI")
            ),
            initComplete = DT::JS("function(){ this.api().columns.adjust(); }")
          ),
          callback = cb
        )
      },
      server = TRUE
    )
    outputOptions(output, "tbl", suspendWhenHidden = FALSE)
  })
}
