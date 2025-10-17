# R/modules/mod_results_table.R
# ------------------------------------------------------------------------------
# FWI25 Results (hourly)
# - No data shown before Run (placeholder only)
# - After Run: show hFWI results in the data's TZ
# - Convert sunrise/sunset (decimal hours, already LOCAL) -> POSIXct in data TZ
# - Alignment fix: safe global script + micro-delay after tab shown
# ------------------------------------------------------------------------------

mod_results_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Global-safe adjust; include a tiny timeout so it runs after layout settles
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
                         div(class = "gc-placeholder__text", uiOutput(ns("hint")))),
                     shinycssloaders::withSpinner(
                       DT::DTOutput(ns("tbl"), width = "100%"),
                       type = 8, color = "#26374A"
                     )
                 )
    )
  )
}

# Required args: results (reactive data.frame), tz_reactive (reactive string TZ)
mod_results_table_server <- function(id, tr, dt_i18n, results, tz_reactive){
  moduleServer(id, function(input, output, session){
    
    # --- i18n helpers -----------------------------------------------------------
    i18n_or <- function(key, default){
      val <- tryCatch(tr(key), error = function(e) NULL)
      if (is.null(val)) return(default)
      val_chr <- as.character(val)
      if (length(val_chr) == 0 || !nzchar(val_chr)) default else val_chr
    }
    
    output$title <- renderUI({ h4(i18n_or("fwi25_results_title", "FWI25 results")) })
    outputOptions(output, "title", suspendWhenHidden = FALSE)
    
    output$hint <- renderUI({ i18n_or("hint_run_to_compute", "Click Run to compute results.") })
    outputOptions(output, "hint", suspendWhenHidden = FALSE)
    
    # --- Helpers ----------------------------------------------------------------
    to_display_tz <- function(df, tz){
      if (is.null(df) || !nrow(df)) return(df)
      is_posix <- vapply(df, function(x) inherits(x, c("POSIXt","POSIXct","POSIXlt")), logical(1))
      if (any(is_posix)){
        for (nm in names(df)[is_posix]){
          df[[nm]] <- lubridate::with_tz(df[[nm]], tzone = tz)
        }
      }
      df
    }
    
    # Decimal hour (e.g., 4.25) is already LOCAL time from fwi25
    hdec_to_hms <- function(hdec){
      h <- floor(hdec); m <- floor((hdec - h) * 60); s <- round((hdec - h - m/60) * 3600)
      h <- pmax(0, pmin(23, h)); m <- pmax(0, pmin(59, m)); s <- pmax(0, pmin(59, s))
      sprintf("%02d:%02d:%02d", h, m, s)
    }
    
    # Build POSIXct from Date + decimal hour in given TZ (no re-interpretation)
    make_dt_from_date_and_hdec <- function(date_vec, hdec, tz){
      if (is.null(hdec)) return(rep(as.POSIXct(NA), length(date_vec)))
      out <- rep(as.POSIXct(NA), length(date_vec))
      ok <- is.finite(hdec); if (!any(ok)) return(out)
      time_str <- hdec_to_hms(hdec[ok])
      date_str <- format(as.Date(date_vec[ok]), "%Y-%m-%d")
      out[ok] <- as.POSIXct(paste0(date_str, " ", time_str), tz = tz)
      out
    }
    
    # --- Data: only show AFTER Run ---------------------------------------------
    table_data <- reactive({
      tz_use <- tz_reactive()
      df <- results()
      req(df)
      df <- as.data.frame(df)
      
      # Determine per-row date to anchor sunrise/sunset (prefer Date column)
      date_col <- NULL
      if ("date" %in% names(df) && inherits(df$date, "Date")) {
        date_col <- df$date
      } else if ("datetime" %in% names(df)) {
        if (is.character(df$datetime)) {
          dt <- suppressWarnings(lubridate::ymd_hms(df$datetime, quiet = TRUE, tz = "UTC"))
          if (all(is.na(dt))) {
            dt <- suppressWarnings(lubridate::parse_date_time(
              df$datetime,
              orders = c("Y-m-d H:M:S","Y-m-d H:M","Ymd HMS","Ymd HM","Y-m-dTH:M:S","Y-m-dTH:M"),
              tz = "UTC"
            ))
          }
          df$datetime <- dt
        }
        # Use data TZ for the Date extraction
        date_col <- as.Date(lubridate::with_tz(df$datetime, tz = tz_use))
      }
      
      # Convert sunrise/sunset decimal hours (already local) into POSIXct at tz_use
      if ("sunrise" %in% names(df) && is.numeric(df$sunrise) && !is.null(date_col)) {
        df$sunrise <- make_dt_from_date_and_hdec(date_col, df$sunrise, tz_use)
      }
      if ("sunset" %in% names(df) && is.numeric(df$sunset) && !is.null(date_col)) {
        df$sunset <- make_dt_from_date_and_hdec(date_col, df$sunset, tz_use)
      }
      
      # Render all POSIX columns (including datetime) in display TZ
      df <- to_display_tz(df, tz_use)
      df
    })
    
    # --- Render DT --------------------------------------------------------------
    output$tbl <- DT::renderDT({
      df <- table_data()
      DT::datatable(
        df,
        rownames = FALSE,
        escape   = TRUE,
        filter   = "top",
        class    = "display nowrap compact hover stripe gc-dt",
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
          buttons = list(
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