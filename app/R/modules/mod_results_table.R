# R/modules/mod_results_table.R
# -----------------------------------------------------------------------------
# FWI25 Results (hourly) -- Results Table Module
#
# - Keeps sunrise/sunset decimals from fwi25 in the table.
# - Displays datetime, timestamp, sunrise_local, sunset_local according to an
#   "Ignore DST" toggle:
#       * ignore DST = TRUE  -> fixed standard offset (e.g., -05:00) via tz_standard_offset_hours()
#       * ignore DST = FALSE -> civil time in Olson TZ from 'tz' column (e.g., -04:00 in summer)
# - Uses tz (Olson) per row; falls back to tz_reactive() if tz column missing/blank.
# - Converts sunrise/sunset decimals (LST) to UTC instants using STANDARD offset,
#   then renders as ISO strings with offsets per the DST policy.
# -----------------------------------------------------------------------------

mod_results_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Ensure columns adjust after layout changes
    tags$script(HTML(
      "
      $(function(){
        var adjust = function(){
          try{ $.fn.dataTable.tables({visible:true, api:true}).columns.adjust(); }catch(e){}
        };
        $(document).on('shown.bs.tab shown.bs.collapse', function(){
          setTimeout(adjust, 0);
        });
        $(window).on('resize.dt', adjust);
      });
      "
    )),
    tags$section(class = "gc-card",
                 tags$div(role = 'region', `aria-label` = "FWI25 results table", uiOutput(ns("title"))),
                 div(class = "gc-card__content",
                     div(class = "gc-placeholder", `aria-live` = "polite", `aria-busy` = "true",
                         div(class = "gc-placeholder__text", uiOutput(ns("hint")))
                     ),
                     shinycssloaders::withSpinner(
                       DT::DTOutput(ns("tbl"), width = "100%"),
                       type = 8, color = "#26374A"
                     )
                 )
    )
  )
}

# Required args:
#   - tr:                   translator function
#   - dt_i18n:              function providing DT language list
#   - results:              reactive data.frame/tibble with columns like:
#                            id (optional), datetime, timestamp, tz (Olson), timezone (std offset hrs),
#                            date (Date, optional), sunrise (decimal or ISO Z), sunset (decimal or ISO Z),
#                            plus all other FWI variables
#   - tz_reactive:          reactive Olson tz string fallback (e.g., "America/Toronto")
#   - ignore_dst_reactive:  reactive(bool); TRUE => show fixed standard offset, FALSE => show civil (DST-aware)
mod_results_table_server <- function(id, tr, dt_i18n, results, tz_reactive, ignore_dst_reactive = reactive(TRUE)){
  moduleServer(id, function(input, output, session){
    
    # --- i18n helpers ---------------------------------------------------------
    i18n_or <- function(key, default){
      val <- tryCatch(tr(key), error = function(e) NULL)
      if (is.null(val)) return(default)
      val_chr <- as.character(val)
      if (length(val_chr) == 0 || !nzchar(val_chr)) default else val_chr
    }
    
    output$title <- renderUI({ h4(i18n_or("fwi25_results_title", "FWI25 results")) })
    outputOptions(output, "title", suspendWhenHidden = FALSE)
    output$hint  <- renderUI({ i18n_or("hint_run_to_compute", "Click Run to compute results.") })
    outputOptions(output, "hint", suspendWhenHidden = FALSE)
    
    # --- Helpers (lean; reuse mod_utils where possible) -----------------------
    
    # Use app's utilities if available; else small local fallbacks.
    std_off_hours <- function(tz){
      if (exists("tz_standard_offset_hours", mode = "function")) {
        tz_standard_offset_hours(tz)  # from mod_utils.R
      } else {
        # Fallback: mid-winter probe (no DST)
        ref <- as.POSIXct("2000-01-15 12:00:00", tz = tz)
        z   <- format(ref, "%z")
        sgn <- ifelse(substr(z,1,1)=="-", -1, 1)
        hh  <- suppressWarnings(as.integer(substr(z,2,3)))
        mm  <- suppressWarnings(as.integer(substr(z,4,5)))
        sgn * (hh + (mm/60))
      }
    } # [1](https://041gc-my.sharepoint.com/personal/justin_beckers_nrcan-rncan_gc_ca/Documents/Microsoft%20Copilot%20Chat%20Files/mod_utils.txt)
    
    modal_off_hours <- function(datetimes){
      if (exists("tz_modal_offset_hours", mode = "function")) {
        tz_modal_offset_hours(datetimes)  # returns list(offset, z_mode)
      } else {
        z_txt  <- format(datetimes, "%z")
        z_txt  <- z_txt[nzchar(z_txt)]
        if (!length(z_txt)) return(list(offset = NA_real_, z_mode = NA_character_))
        z_mode <- names(which.max(table(z_txt)))
        sgn <- ifelse(substr(z_mode,1,1)=="-", -1, 1)
        hh  <- suppressWarnings(as.integer(substr(z_mode,2,3)))
        mm  <- suppressWarnings(as.integer(substr(z_mode,4,5)))
        list(offset = sgn * (hh + (mm/60)), z_mode = z_mode)
      }
    } # [1](https://041gc-my.sharepoint.com/personal/justin_beckers_nrcan-rncan_gc_ca/Documents/Microsoft%20Copilot%20Chat%20Files/mod_utils.txt)
    
    parse_iso_utc <- function(x){
      suppressWarnings(lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE))
    }
    
    # Decimal hour (e.g., 4.7333) -> "HH:MM:SS"
    hdec_to_hms <- function(hdec){
      h <- floor(hdec)
      m <- floor((hdec - h) * 60)
      s <- round((hdec - h - m/60) * 3600)
      h <- pmax(0, pmin(23, h)); m <- pmax(0, pmin(59, m)); s <- pmax(0, pmin(59, s))
      sprintf("%02d:%02d:%02d", h, m, s)
    }
    
    # Convert local *standard* time (decimal) to a UTC instant using STANDARD offset.
    # local_LST = UTC + std_offset  =>  UTC = local_LST - std_offset
    make_utc_from_date_hdec_LST <- function(date_vec, hdec, std_offset_hours){
      out <- rep(as.POSIXct(NA, tz = "UTC"), length(date_vec))
      ok  <- is.finite(hdec) & !is.na(date_vec) & is.finite(std_offset_hours)
      if (!any(ok)) return(out)
      time_str <- hdec_to_hms(hdec[ok])
      date_str <- format(as.Date(date_vec[ok]), "%Y-%m-%d")
      out[ok] <- as.POSIXct(paste0(date_str, " ", time_str), tz = "UTC") - std_offset_hours[ok] * 3600
      out
    }
    
    # Fixed-offset tz string (Etc/GMT has reversed sign; -5 -> "Etc/GMT+5")
    fixed_tz_from_offset <- function(offset_hours){
      ifelse(is.finite(offset_hours), sprintf("Etc/GMT%+d", -as.integer(offset_hours)), NA_character_)
    }
    
    # Format to ISO with offset, given a per-row tz vector (Olson or fixed)
    format_iso_with_offset_per_row <- function(x, tz_vec){
      out <- character(length(x))
      ok  <- !is.na(x)
      out[!ok] <- NA_character_
      if (any(ok)){
        out[ok] <- mapply(function(xx, tz){
          if (is.na(xx) || is.na(tz) || !nzchar(tz)) return(NA_character_)
          s <- format(lubridate::with_tz(xx, tz), "%Y-%m-%dT%H:%M:%S%z")
          sub("([+\\-]\\d{2})(\\d{2})$", "\\1:\\2", s)
        }, x[ok], tz_vec[ok], USE.NAMES = FALSE)
      }
      out
    }
    
    # --- Data prep & table build ---------------------------------------------
    table_data <- reactive({
      df <- results(); req(df)
      df <- as.data.frame(df)
      
      # Ensure we have an id for the table
      if (!("id" %in% names(df))) df$id <- seq_len(nrow(df))
      
      # Resolve per-row Olson tz (civil) and STANDARD offsets
      olson_fallback <- tz_reactive()
      if ("tz" %in% names(df) && is.character(df$tz) && any(nzchar(df$tz))){
        olson_vec <- ifelse(nzchar(df$tz), df$tz, olson_fallback)
      } else {
        olson_vec <- rep(olson_fallback, nrow(df))
      }
      
      # Prefer a supplied numeric STANDARD offset column if present; otherwise derive
      if ("timezone" %in% names(df) && is.numeric(df$timezone)) {
        std_off <- df$timezone
      } else {
        # derive per-row (vectorized by unique tz)
        uniq_tz <- unique(olson_vec)
        map_off <- setNames(
          vapply(uniq_tz, std_off_hours, numeric(1)),
          uniq_tz
        )
        std_off <- unname(map_off[olson_vec])
      } # Uses tz_standard_offset_hours() from mod_utils when available. [1](https://041gc-my.sharepoint.com/personal/justin_beckers_nrcan-rncan_gc_ca/Documents/Microsoft%20Copilot%20Chat%20Files/mod_utils.txt)
      # Parse datetime/timestamp → POSIXct UTC if character (ISO Z or offset)
      if ("datetime" %in% names(df) && is.character(df$datetime)){
        dt <- parse_iso_utc(df$datetime)
        if (all(is.na(dt))){
          dt <- suppressWarnings(lubridate::parse_date_time(
            df$datetime,
            orders = c("Y-m-d H:M:S","Y-m-d H:M","Ymd HMS","Ymd HM","Y-m-dTH:M:S","Y-m-dTH:M"),
            tz = "UTC"
          ))
        }
        df$datetime <- dt
      }
      if ("timestamp" %in% names(df) && is.character(df$timestamp)){
        df$timestamp <- parse_iso_utc(df$timestamp)
      }
      
      # Establish LST date anchor for numeric sunrise/sunset (prefer datetime)
      date_LST <- NULL
      if ("datetime" %in% names(df) && inherits(df$datetime, c("POSIXct","POSIXt"))){
        # Convert UTC -> LST by arithmetic using STANDARD offset (no DST) to pick the LST day
        dt_LST <- df$datetime + std_off * 3600
        date_LST <- as.Date(dt_LST)
      } else if ("date" %in% names(df) && inherits(df$date, "Date")){
        date_LST <- df$date
      } else if (all(c("year","month","day") %in% names(df))) {
        date_LST <- as.Date(sprintf("%04d-%02d-%02d", df$year, df$month, df$day))
      }
      
      # Sunrise/Sunset as UTC instants
      # Case A: numeric LST decimals -> construct UTC via STANDARD offset
      if ("sunrise" %in% names(df) && is.numeric(df$sunrise) && !is.null(date_LST)){
        df$sunrise_utc <- make_utc_from_date_hdec_LST(date_LST, df$sunrise, std_off)
      }
      if ("sunset" %in% names(df) && is.numeric(df$sunset) && !is.null(date_LST)){
        df$sunset_utc  <- make_utc_from_date_hdec_LST(date_LST, df$sunset,  std_off)
      }
      # Case B: ISO8601 strings (UTC or offset) -> parse as UTC *instants*
      if ("sunrise" %in% names(df) && is.character(df$sunrise)){
        df$sunrise_utc <- parse_iso_utc(df$sunrise)
      }
      if ("sunset" %in% names(df) && is.character(df$sunset)){
        df$sunset_utc  <- parse_iso_utc(df$sunset)
      }
      
      # Choose display tz per row
      ignore_dst <- isTRUE(ignore_dst_reactive())
      display_tz_vec <- if (ignore_dst) {
        # Fixed STANDARD offset for display (e.g., -05:00 in summer)
        fixed_tz_from_offset(std_off)
      } else {
        # Civil (DST-aware) Olson tz from data/fallback
        olson_vec
      }
      
      # Build ISO strings with offsets for the four visible time columns
      datetime_iso  <- if ("datetime" %in% names(df)  && inherits(df$datetime,  c("POSIXct","POSIXt"))) {
        format_iso_with_offset_per_row(df$datetime,  display_tz_vec)
      } else rep(NA_character_, nrow(df))
      timestamp_iso <- if ("timestamp" %in% names(df) && inherits(df$timestamp, c("POSIXct","POSIXt"))) {
        format_iso_with_offset_per_row(df$datetime, display_tz_vec)
      } else rep(NA_character_, nrow(df))
      sunrise_iso   <- if ("sunrise_utc" %in% names(df) && inherits(df$sunrise_utc, c("POSIXct","POSIXt"))) {
        format_iso_with_offset_per_row(df$sunrise_utc, display_tz_vec)
      } else rep(NA_character_, nrow(df))
      sunset_iso    <- if ("sunset_utc"  %in% names(df) && inherits(df$sunset_utc,  c("POSIXct","POSIXt"))) {
        format_iso_with_offset_per_row(df$sunset_utc,  display_tz_vec)
      } else rep(NA_character_, nrow(df))
      
      # --- Final minimal table ------------------------------------------------
      # 1) First, only the requested visible columns:
      out <- data.frame(
        id            = df$id,
        datetime      = datetime_iso,
        timestamp     = timestamp_iso,
        sunrise_local = sunrise_iso,
        sunset_local  = sunset_iso,
        stringsAsFactors = FALSE
      )
      
      # 2) Then, append ALL remaining inputs/outputs from results(),
      #    but drop purely internal helper columns used for construction.
      drop_helpers <- c(
        "sunrise_utc","sunset_utc",   # construction helpers
        "date"                        # optional anchor; you still keep year/month/day if present
      )
      
      passthrough <- setdiff(names(df), c(names(out), drop_helpers))
      # NOTE: We do NOT drop sunrise/sunset decimals—they pass through here.
      
      out <- cbind(out, df[passthrough])
      
      out
    })
    
    # --- Render DT -----------------------------------------------------------
    output$tbl <- DT::renderDT({
      df <- table_data()
      DT::datatable(
        df,
        rownames   = FALSE,
        escape     = TRUE,
        filter     = "top",
        class      = "display nowrap compact hover stripe gc-dt",
        extensions = c("Buttons", "Scroller"),
        options = list(
          language    = dt_i18n(),
          autoWidth   = TRUE,
          scrollX     = TRUE,
          deferRender = TRUE,
          scroller    = TRUE,
          pageLength  = 25,
          lengthMenu  = list(c(10,25,50,100,-1), c('10','25','50','100','All')),
          scrollY     = 300,
          dom         = "Bfrtip",
          buttons     = list(
            list(extend = "copy",  text = tr("dt_btn_copy")),
            list(extend = "csv",   text = tr("dt_btn_csv"),   filename = "hFWI"),
            list(extend = "excel", text = tr("dt_btn_excel"), filename = "hFWI")
          )
        )
      )
    }, server = TRUE, fillContainer = TRUE)
    outputOptions(output, "tbl", suspendWhenHidden = FALSE)
  })
}