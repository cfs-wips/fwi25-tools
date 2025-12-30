# R/modules/mod_fwi87_table.R
# -----------------------------------------------------------------------------
# FWI87 (daily) table with DST policy normalization, robust parsing, and
# correct "Ignore DST" behavior: 12:00 LOCAL STANDARD time displayed with
# standard offset, e.g., 20070510T12:00:00-05:00 for Toronto in summer.
# -----------------------------------------------------------------------------


# mod_fwi87_table_ui.R
mod_fwi87_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(HTML("
      $(function(){
        var adjust = function(){
          try{ $.fn.dataTable.tables({visible:true, api:true}).columns.adjust(); }catch(e){}
        };
        $(document).on('shown.bs.tab shown.bs.collapse', function(){ setTimeout(adjust, 0); });
        $(window).on('resize.dt', adjust);
      });
    ")),
    tags$section(
      class = "gc-card fwi87-tab",
      tags$div(role = "region", `aria-label` = "FWI87 results table", uiOutput(ns("title"))),
      div(
        class = "gc-card__content",
        div(
          class = "gc-placeholder", `aria-live` = "polite", `aria-busy` = "true",
          div(class = "gc-placeholder__text", uiOutput(ns("hint")))
        ),
        div(
          class = "gc-spin-wrap",
          # ⬇️ fill vertically to card height
          DT::DTOutput(ns("tbl"), width = "100%", height = "100%", fill = TRUE),
          div(
            class = "gc-spin-overlay",
            div(class = "gc-spinner", `aria-hidden` = "true"),
            span(class = "visually-hidden", "Loading…")
          )
        )
      )
    )
  )
}


#' @param tr translator function
#' @param dt_i18n function returning DT language list
#' @param df87 reactive data.frame/tibble (daily results)
#' @param tz_reactive reactive( character(1) Olson tz fallback, e.g., "America/Toronto")
#' @param ignore_dst_reactive reactive policy:
#'        TRUE => fixed standard offset (ignore DST), FALSE => civil (DST-aware).
#'        Can also be character ("standard"/"civil") or numeric (1/0).
mod_fwi87_table_server <- function(id, tr, dt_i18n, df87, tz_reactive,
                                   ignore_dst_reactive = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    # ---- i18n

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
      h5(i18n_or("fwi87_results_title", "FWI87 daily results"))
    })
    outputOptions(output, "title", suspendWhenHidden = FALSE)
    output$hint <- renderUI({
      i18n_or("hint_run_to_compute", "Click Run to compute results.")
    })
    outputOptions(output, "hint", suspendWhenHidden = FALSE)

    # ---- Policy normalizer
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
        message("FWI87: normalize_policy() fell back to civil for value: ", paste0(capture.output(str(val)), collapse = " "))
      }
      FALSE
    }

    # ---- Time helpers
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
    std_off_hours <- function(tz) {
      if (exists("tz_standard_offset_hours", mode = "function")) {
        tz_standard_offset_hours(tz) # from your mod_utils.R
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
    # ISO BASIC per-row formatter: "YYYYMMDDTHH:MM:SS±hh:mm"
    format_iso_basic_with_offset_per_row <- function(x, tz_vec) {
      out <- character(length(x))
      ok <- !is.na(x)
      out[!ok] <- NA_character_
      if (any(ok)) {
        out[ok] <- mapply(function(xx, tz) {
          if (is.na(xx) || is.na(tz) || !nzchar(tz)) {
            return(NA_character_)
          }
          s <- format(lubridate::with_tz(xx, tz), "%Y%m%dT%H:%M:%S%z")
          sub("([+\\-]\\d{2})(\\d{2})$", "\\1:\\2", s)
        }, x[ok], tz_vec[ok], USE.NAMES = FALSE)
      }
      out
    }
    # Build a noon LOCAL STANDARD instant from a Date:
    #   UTC = local_noon_LST - standard_offset
    local_standard_noon_utc <- function(date_vec, std_off) {
      out <- rep(as.POSIXct(NA, tz = "UTC"), length(date_vec))
      ok <- !is.na(date_vec) & is.finite(std_off)
      if (!any(ok)) {
        return(out)
      }
      date_str <- format(as.Date(date_vec[ok]), "%Y-%m-%d")
      out[ok] <- as.POSIXct(paste0(date_str, " 12:00:00"), tz = "UTC") - std_off[ok] * 3600
      out
    }

    add_debug_cols <- function(df, policy, display_tz_vec, std_off, olson_vec,
                               dt_raw, civil_instant_utc, std_noon_utc) {
      if (!isTRUE(getOption("fwi.debug_times", FALSE))) {
        return(df)
      }
      cbind(
        df,
        `.dbg_policy`              = ifelse(policy, "standard", "civil"),
        `.dbg_display_tz_used`     = display_tz_vec,
        `.dbg_std_offset_hours`    = std_off,
        `.dbg_olson_resolved`      = olson_vec,
        `.dbg_datetime_raw`        = if (length(dt_raw)) dt_raw else NA,
        `.dbg_civil_noon_utc`      = if (length(civil_instant_utc)) format(civil_instant_utc, "%Y-%m-%dT%H:%M:%SZ") else NA,
        `.dbg_std_noon_utc`        = if (length(std_noon_utc)) format(std_noon_utc, "%Y-%m-%dT%H:%M:%SZ") else NA
      )
    }

    # ---- Build the display table
    table_data <- reactive({
      policy_ignore <- normalize_policy(ignore_dst_reactive()) # depend on the toggle
      d <- df87()
      req(!is.null(d), nrow(d) > 0)
      d <- as.data.frame(d)
      n <- nrow(d)

      # Per-row Olson TZ (civil) with fallback
      olson_fallback <- tz_reactive()
      if ("tz" %in% names(d) && is.character(d$tz) && any(nzchar(d$tz))) {
        olson_vec <- ifelse(nzchar(d$tz), d$tz, olson_fallback)
      } else {
        olson_vec <- rep(olson_fallback, n)
      }

      # STANDARD offset (hours)
      std_off <- if ("timezone" %in% names(d) && is.numeric(d$timezone)) {
        d$timezone
      } else {
        m <- setNames(vapply(unique(olson_vec), std_off_hours, numeric(1)), unique(olson_vec))
        unname(m[olson_vec])
      }

      # Prepare the two instants we may display for the 'datetime' column:
      # A) CIVIL-NOON INSTANT (engine's df$datetime, which should be POSIXct in Olson TZ)
      civil_instant_utc <- NULL
      dt_raw <- if ("datetime" %in% names(d)) d$datetime else NULL

      # Normalize to POSIXct UTC instant
      if ("datetime" %in% names(d) && inherits(d$datetime, "POSIXlt")) d$datetime <- as.POSIXct(d$datetime)
      if ("datetime" %in% names(d) && inherits(d$datetime, "POSIXct")) {
        # Store its UTC for debug/export
        civil_instant_utc <- lubridate::with_tz(d$datetime, "UTC")
      } else if ("date" %in% names(d) && inherits(d$date, "Date")) {
        # Fallback: treat the 'date' as the civil day (we'll still compute std noon below)
        d$datetime <- as.POSIXct(paste0(format(d$date, "%Y-%m-%d"), " 12:00:00"), tz = olson_fallback)
        civil_instant_utc <- lubridate::with_tz(d$datetime, "UTC")
      }

      # Row date anchor (civil date from df$datetime in Olson TZ)
      date_civil <- if ("datetime" %in% names(d) && inherits(d$datetime, "POSIXct")) {
        as.Date(lubridate::with_tz(d$datetime, olson_vec))
      } else if ("date" %in% names(d) && inherits(d$date, "Date")) {
        d$date
      } else {
        rep(as.Date(NA), n)
      }

      # B) STANDARD-NOON INSTANT to display when policy_ignore == TRUE
      std_noon_utc <- local_standard_noon_utc(date_civil, std_off)

      # Choose DISPLAY TZ vector per policy
      display_tz_vec <- if (policy_ignore) fixed_tz_from_offset(std_off) else olson_vec

      # ---- Compose the 'datetime' DISPLAY COLUMN
      datetime_display_instant <- if (policy_ignore) {
        # Ignore DST -> display *local STANDARD noon* instant
        std_noon_utc
      } else {
        # Civil -> display the engine's civil noon instant
        civil_instant_utc
      }

      # Format as ISO BASIC with offset
      datetime_iso <- format_iso_basic_with_offset_per_row(datetime_display_instant, display_tz_vec)

      # Build output: put the formatted 'datetime' back into the data frame
      out <- d
      out$datetime <- datetime_iso

      # Append debug columns if requested
      out <- add_debug_cols(
        out,
        policy = policy_ignore,
        display_tz_vec = display_tz_vec,
        std_off = std_off,
        olson_vec = olson_vec,
        dt_raw = dt_raw,
        civil_instant_utc = civil_instant_utc,
        std_noon_utc = std_noon_utc
      )

      out
    })

    # ---- Render DT
    output$tbl <- DT::renderDT(
      {
        d <- table_data()
        cb <- DT::JS("
        var tbl = table;          // DataTables API instance
        function adjust(){ try { tbl.columns.adjust(); } catch(e){} }
        setTimeout(adjust, 0);    // after init
        tbl.on('draw.dt', adjust);
        $(window).on('resize.dt', adjust);
      ")
        DT::datatable(
          d,
          rownames = FALSE,
          escape = TRUE,
          fillContainer = TRUE,
          filter = "top",
          class = "display nowrap compact hover stripe gc-dt datatable", # add "datatable"
          extensions = c("Buttons"),
          options = list(
            language = dt_i18n(),
            autoWidth = FALSE, # <- match mod_inputs
            deferRender = TRUE, # <- match mod_inputs
            scrollX = TRUE, # <- match mod_inputs
            scrollY = "38vh", # <- match mod_inputs
            dom = "Brtp",
            buttons = list(
              list(extend = "copy", text = tr("dt_btn_copy")),
              list(extend = "csv", text = tr("dt_btn_csv"), filename = "dailyFWI"),
              list(extend = "excel", text = tr("dt_btn_excel"), filename = "dailyFWI")
            ),
            # drop columnDefs width rules for ISO datetime/sunrise/sunset
            initComplete = DT::JS("function(){ this.api().columns.adjust(); }")
          ),
          callback = cb
        )
      },
      server = FALSE
    )
    outputOptions(output, "tbl", suspendWhenHidden = FALSE)
  })
}
