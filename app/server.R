library(shiny)
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(DT)
library(ggplot2)


# ---- Load NG-CFFDRS R code fetched at build time ----
source("ng/util.r", local = TRUE)
source("ng/make_inputs.r", local = TRUE)
source("ng/NG_FWI.r", local = TRUE)
source("ng/old_cffdrs.r", local = TRUE)



server <- function(input, output, session) {
  # PATCH: Helpers
  # Safe defaulting
  `%||%` <- function(a, b)
    if (is.null(a) || is.na(a))
      b
  else
    a
  
  #Default Column Finder
  find_col <- function(cols, keywords) {
    match <- cols[grepl(paste0("^", keywords, "$", collapse = "|"), cols, ignore.case = TRUE)]
    if (length(match) > 0) match[1] else ""
  }
  
  # Correctly parse "+HHMM"/"-HHMM" into decimal hours (+0930 -> +9.5, +0545 -> +5.75)
  parse_z_to_hours <- function(z_txt) {
    z_txt <- as.character(z_txt)
    z_txt <- z_txt[nzchar(z_txt)]
    if (!length(z_txt))
      return(NA_real_)
    sgn <- ifelse(substr(z_txt, 1, 1) == "-", -1, 1)
    hh  <- suppressWarnings(as.integer(substr(z_txt, 2, 3)))
    mm  <- suppressWarnings(as.integer(substr(z_txt, 4, 5)))
    sgn * (hh + (mm %||% 0) / 60)
  }
  
  tz_standard_offset_hours <- function(tz, probe_date = "2025-01-15 12:00:00") {
    probe <- as.POSIXct(probe_date, tz = tz)
    parse_z_to_hours(format(probe, "%z"))
  }
  
  tz_modal_offset_hours <- function(datetimes) {
    z_txt <- format(datetimes, "%z")
    z_txt <- z_txt[nzchar(z_txt)]
    if (!length(z_txt))
      stop("Could not infer modal offset: empty %z values.")
    z_mode <- names(which.max(table(z_txt)))
    list(offset = parse_z_to_hours(z_mode), z_mode = z_mode)
  }
  # Helper to (re)populate the choices safely
  populate_plot_choices <- function() {
    df <- data_for_plot()                     # req() is inside data_for_plot()
    dt_col <- attr(df, "dt_col")
    
    # Numeric columns, excluding the datetime
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    choices  <- setdiff(num_cols, dt_col)
    
    # If nothing numeric, leave control empty
    if (length(choices) == 0L) {
      updateSelectizeInput(session, "plot_y_multi",
                           choices = character(0), selected = character(0))
      return(invisible(NULL))
    }
    
    # Keep previous selection if still valid; otherwise pick up to 3 defaults
    
    prev <- isolate(input$plot_y_multi)
    if (is.null(prev)) prev <- character(0)
    still_valid <- intersect(prev, choices)
    
    default_sel <- if (length(still_valid)) still_valid else head(choices, min(3L, length(choices)))
    
    # Client-side update (more reliable when tabs are lazy-rendered)
    updateSelectizeInput(
      session, "plot_y_multi",
      choices  = choices,
      selected = default_sel
    )
  }
  
  # Recompute choices:
  #  - when the underlying data changes
  #  - when the dataset selector changes
  #  - when the Plot tab becomes visible (lazy UI)
  observeEvent(list(data_for_plot(), input$plot_dataset), {
    populate_plot_choices()
  }, ignoreInit = FALSE)
  
  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "Plot")) populate_plot_choices()
  }, ignoreInit = FALSE)
  
  

  
  data_for_plot <- reactive({
    # You must have pressed Run at least once, because both sources depend on it
    req(shaped_input())  # creates inputs (wx)
    req(input$plot_dataset)
    
    # 1) Choose the source
    df <- if (identical(input$plot_dataset, "inputs")) {
      as.data.frame(shaped_input()$inputs)    # your wx tibble
    } else {
      as.data.frame(run_model())              # your hFWI results table
    }
    
    validate(need(nrow(df) > 0, "Selected dataset has no rows."))
    
    # 2) Detect or construct a datetime column
    # Prefer an existing datetime-like column
    dt_candidates <- names(df)[grepl("datetime|timestamp|date|time", names(df), ignore.case = TRUE)]
    dt_col <- character(0)
    
    # Keep only POSIXt/POSIXct/Date if present among candidates
    if (length(dt_candidates)) {
      typed <- dt_candidates[sapply(df[dt_candidates], function(x) inherits(x, c("POSIXt","Date")) || is.character(x))]
      dt_col <- if (length(typed)) typed[1] else dt_candidates[1]
    }
    
    # If none found, try to build from year/month/day/hour (common in your pipeline)
    if (!length(dt_col) && all(c("year","month","day","hour") %in% names(df))) {
      tz_use <- shaped_input()$tz
      if (is.null(tz_use) || !nzchar(tz_use)) tz_use <- "UTC"
      df$datetime <- lubridate::make_datetime(
        year  = as.integer(df$year),
        month = as.integer(df$month),
        day   = as.integer(df$day),
        hour  = as.integer(df$hour),
        tz    = tz_use
      )
      dt_col <- "datetime"
    }
    
    validate(need(length(dt_col) == 1, "Couldn't find or construct a datetime/timestamp column."))
    
    # 3) Order by time and tag which column we chose
    ord <- try(order(df[[dt_col]]), silent = TRUE)
    if (!inherits(ord, "try-error")) df <- df[ord, , drop = FALSE]
    attr(df, "dt_col") <- dt_col
    df
  })
  
  # Populate Y variable choices once data_for_plot() is ready ----------------
  observeEvent(data_for_plot(), {
    df <- data_for_plot()
    dt_col <- attr(df, "dt_col")
    
    # Only numeric columns, excluding the datetime column
    num_cols <- names(df)[sapply(df, is.numeric)]
    choices  <- setdiff(num_cols, dt_col)
    
    validate(need(length(choices) > 0, "No numeric columns available to plot."))
    
    # Preselect the first few as a convenience
    default_sel <- head(choices, min(3L, length(choices)))
    
    updateSelectizeInput(
      session, "plot_y_multi",
      choices = choices,
      selected = if (!is.null(input$plot_y_multi) && length(intersect(input$plot_y_multi, choices)) > 0)
        intersect(input$plot_y_multi, choices)
      else
        default_sel,
      server = TRUE
    )
  }, ignoreInit = FALSE)
  
  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "Plot")) {
      # Only try when inputs/results exist (after Run)
      if (!is.null(shaped_input())) populate_plot_choices()
    }
  }, ignoreInit = FALSE)
  
  
  output$plot_ts <- renderPlot({
    df <- data_for_plot()
    dt_col <- attr(df, "dt_col")
    
    req(length(input$plot_y_multi) >= 1)
    yvars <- unique(input$plot_y_multi)
    
    # Keep only datetime + selected columns
    keep_cols <- unique(c(dt_col, yvars))
    df_small  <- df[, keep_cols, drop = FALSE]
    
    # Order by time (defensive)
    ord <- try(order(df_small[[dt_col]]), silent = TRUE)
    if (!inherits(ord, "try-error")) df_small <- df_small[ord, , drop = FALSE]
    
    # Long format
    long_df <- df_small |>
      tidyr::pivot_longer(
        cols = tidyselect::all_of(yvars),
        names_to  = "variable",
        values_to = "value"
      ) |>
      dplyr::filter(!is.na(.data$value))
    
    req(nrow(long_df) > 0)
    
    long_df$variable <- factor(long_df$variable, levels = yvars)
    
    
    ncol_facets <- {
      val <- input$facet_ncol
      if (is.null(val) || is.na(val) || val < 1) 1L else as.integer(val)
    }

    
    ggplot(long_df, aes(x = .data[[dt_col]], y = .data$value)) +
      geom_line(color = "#2C7FB8", linewidth = 0.6, na.rm = TRUE) +
      geom_point(color = "#2C7FB8", size = 0.8, alpha = 0.7, na.rm = TRUE) +
      facet_wrap(
        ~ variable,
        ncol   = ncol_facets,
        scales = if (isTRUE(input$facet_free_y)) "free_y" else "fixed"
      ) +
      labs(
        x = dt_col,
        y = NULL,
        title = if (length(yvars) == 1)
          paste(yvars, "over time")
        else
          "Selected variables over time"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold")
      )
  })
  
  
  
  
  
  
  # Shinylive/webR-friendly CSV read: fast path fread(), fallback to base
  raw_file <- reactive({
    req(input$csv)
    tryCatch(
      data.table::fread(
        input$csv$datapath,
        sep = ",",
        na.strings = c("", "NA", "NaN", "null"),
        header = input$has_header
      ),
      error = function(e)
        utils::read.csv(
          input$csv$datapath,
          header = input$has_header,
          na.strings = c("", "NA", "NaN", "null"),
          check.names = F,
          stringsAsFactors = FALSE
        )
    )
  })
  
  observeEvent(raw_file(), {
    df <- raw_file()
    
    # Try to find a datetime column
    datetime_col <- names(df)[grepl("datetime|timestamp", names(df), ignore.case = TRUE)][1]
    
    if (!is.na(datetime_col)) {
      first_date <- as.Date(df[[datetime_col]][1])
    } else {
      # Try to build date from year/month/day columns
      year_col <- names(df)[grepl("^year$|^yr$", names(df), ignore.case = TRUE)][1]
      month_col <- names(df)[grepl("^month$|^mon$", names(df), ignore.case = TRUE)][1]
      day_col <- names(df)[grepl("^day$|^dy$", names(df), ignore.case = TRUE)][1]
      
      if (!any(is.na(c(year_col, month_col, day_col)))) {
        first_date <- as.Date(paste(df[[year_col]][1], df[[month_col]][1], df[[day_col]][1]), format = "%Y %m %d")
      } else {
        first_date <- NULL
      }
    }
    
    # Update the dateInput if a valid date was found
    if (!is.null(first_date) && !is.na(first_date)) {
      updateDateInput(session, "start_date", value = first_date)
    }
  })
  
  
  # Map UI helpers
  get_col <- function(df, nm) {
    if (isTruthy(nm) &&
        nzchar(nm) && nm %in% names(df))
      df[[nm]]
    else
      NULL
  }
  
  # ======================
  # PATCH: Time zone inference (client-side, via tz-lookup)
  # ======================
  
  tz_guess <- reactiveVal(NULL)
  
  # Receive IANA zone from browser JS
  observeEvent(input$tz_lookup_result, ignoreInit = TRUE, {
    if (is.character(input$tz_lookup_result) &&
        nzchar(input$tz_lookup_result)) {
      tz_guess(input$tz_lookup_result)
    }
  })
  
  # Display current guess (works independent of "Run")
  output$tz_out <- renderPrint({
    tz <- tz_guess()
    if (is.null(tz))
      "Timezone not yet inferred."
    else
      paste("IANA timezone:", tz)
  })
  
  # Compute a representative lat/lon to send to browser (median of mapped cols)
  representative_latlon <- function(df) {
    lat <- lon <- NULL
    if (!missing(df) && !is.null(df)) {
      lat <- suppressWarnings(as.numeric(get_col(df, input$col_lat)))
      lon <- suppressWarnings(as.numeric(get_col(df, input$col_lon)))
      if (length(lat) && length(lon)) {
        la <- suppressWarnings(median(lat, na.rm = TRUE))
        lo <- suppressWarnings(median(lon, na.rm = TRUE))
        if (is.finite(la) &&
            is.finite(lo))
          return(c(lat = la, lon = lo))
      }
    }
    c(lat = NA_real_, lon = NA_real_)
  }
  
  # Trigger browser tz-lookup when switching to 'auto' or when user hits Run
  observeEvent(list(input$tz_mode, input$run, input$csv), ignoreInit = F, {
    if (identical(input$tz_mode, "auto")) {
      df <- try(raw_file(), silent = TRUE)
      if (!inherits(df, "try-error")) {
        coords <- representative_latlon(as.data.frame(df))
        print(coords)
        if (is.finite(coords["lat"]) && is.finite(coords["lon"])) {
          session$sendCustomMessage("tz_lookup", list(lat = unname(coords["lat"]), lon = unname(coords["lon"])))
        }
      }
    }
  })
  
  # ======================
  # Column mapping UI
  # ======================
  
  output$mapping_ui <- renderUI({
    df <- raw_file()
    cols <- names(df)
    tagList(
      helpText(
        "Map your columns to what NG-CFFDRS expects. Provide either a single Date-Time column, or Year/Month/Day/Hour."
      ),
      selectInput(
        "col_datetime",
        "Date-Time (optional)",
        choices = c("", cols),
        selected = find_col(cols, c("datetime","timestamp"))
      ),
      fluidRow(
        column(3, selectInput(
          "col_year", "Year", choices = c("", cols), selected = find_col(cols,c("year",'yr',"y"))
        )),
        column(3, selectInput(
          "col_month", "Month", choices = c("", cols), selected = find_col(cols, c("month", "mon","m"))
        )),
        column(3, selectInput("col_day", "Day", choices = c("", cols), selected = find_col(cols, c("day","dy","d"))
        )),
        column(3, selectInput(
          "col_hour", "Hour (0-23)", choices = c("", cols), selected = find_col(cols, c("hour","hr","h")),
        ))
      ),
      fluidRow(
        column(
          3,
          selectInput("col_temp", "Temperature (°C)", choices = cols, selected = find_col(cols, c("temp","temperature","t"))
          )),
        column(3, selectInput("col_rh", "RH (%)", choices = c("", cols), selected = find_col(cols, c("rh","relative humidity","relative.humidity","relative_humidity","humidity")))),
        column(3, selectInput("col_ws", "Wind (km/h)", choices = c("", cols), selected = find_col(cols, c("ws","windspeed","wind_speed","wind.speed","wind speed")))),
        column(3, selectInput("col_rain", "Rain (mm)", choices = c("", cols), selected = find_col(cols, c("rain","precip","prec","precip_mm","prec_mm","rain_mm"))))
      ),
      fluidRow(
        column(6,
               selectInput("col_lat", "Latitude (deg)", choices = c("", cols), selected = find_col(cols, c("lat","latitude"))
               )), 
        column(6, 
               selectInput("col_lon", "Longitude (deg)", choices = c("", cols), selected = find_col(cols, c("lon","long","longitude"))
               ))
      )
    )
  })
  
  shaped_input <- eventReactive(input$run, {
    validate(need(!is.null(raw_file()), "Upload a CSV first."))
    df <- tibble::as_tibble(raw_file())
    
    needed <- c(input$col_temp,
                input$col_rh,
                input$col_ws,
                input$col_rain)
    validate(need(
      all(nzchar(needed)),
      "Please map temperature, RH, wind, and rain columns."
    ))
    
    tz_use <- switch(input$tz_mode,
                     "fixed" = input$fixed_tz,
                     "auto"  = {
                       # Prefer the JS lat/lon-derived guess if available; else the browser timezone; else validate
                       if (!is.null(tz_guess()))
                         tz_guess()
                       else if (is.character(input$tz_browser) &&
                                nzchar(input$tz_browser))
                         input$tz_browser
                       else
                         validate(need(
                           FALSE,
                           "Timezone not inferred yet—map lat/lon or use a fixed timezone."
                         ))
                     })
    
    
    dt_col <- get_col(df, input$col_datetime)
    
    has_explicit_zone <- function(x) {
      if (is.null(x))
        return(FALSE)
      x <- as.character(x)
      any(grepl("(Z$)|([+-]\\d{2}:?\\d{2}$)", x, ignore.case = TRUE),
          na.rm = TRUE)
    }
    
    if (!is.null(dt_col)) {
      if (has_explicit_zone(dt_col)) {
        dt_utc <- lubridate::parse_date_time(
          dt_col,
          orders = c(
            "Y-m-d H:M:S",
            "Y-m-d H:M",
            "Y/m/d H:M:S",
            "Y/m/d H:M",
            "d-m-Y H:M:S",
            "d/m/Y H:M:S",
            "m/d/Y H:M:S",
            "m/d/Y H:M",
            "Ymd HMS",
            "Ymd HM",
            "Ymd H"
          ),
          tz = "UTC"
        )
        validate(need(
          !all(is.na(dt_utc)),
          "Could not parse your Date-Time column (UTC/offset)."
        ))
        dt_local <- lubridate::with_tz(dt_utc, tz = tz_use)
      } else {
        dt_local <- lubridate::parse_date_time(
          dt_col,
          orders = c(
            "Y-m-d H:M:S",
            "Y-m-d H:M",
            "Y/m/d H:M:S",
            "Y/m/d H:M",
            "d-m-Y H:M:S",
            "d/m/Y H:M:S",
            "m/d/Y H:M:S",
            "m/d/Y H:M",
            "Ymd HMS",
            "Ymd HM",
            "Ymd H"
          ),
          tz = tz_use
        )
        validate(need(!all(is.na(dt_local)), "Could not parse your Date-Time column."))
      }
    } else {
      y <- get_col(df, input$col_year)
      m <- get_col(df, input$col_month)
      d <- get_col(df, input$col_day)
      h <- get_col(df, input$col_hour)
      validate(need(
        all(!is.null(c(y, m, d, h))),
        "Provide Date-Time or Year/Month/Day/Hour."
      ))
      dt_local <- lubridate::make_datetime(
        year  = as.integer(y),
        month = as.integer(m),
        day   = as.integer(d),
        hour  = as.integer(h),
        tz    = tz_use
      )
    }
    
    # Optional start-date filter (in local time)
    if (!is.null(input$start_date) && !is.na(input$start_date)) {
      keep <- as.Date(dt_local, tz = tz_use) >= as.Date(input$start_date)
      validate(need(any(keep), "All rows were filtered out by the start date."))
      df       <- df[keep, , drop = FALSE]
      validate(
        need(
          nrow(df) > 0,
          "No rows remain after filtering; check your date filter or input data."
        )
      )
      dt_local <- dt_local[keep]
    }# else {
    # dt_local <- dt_local
    #}
    
    # ---- Compute offsets (hours) with correct %z parsing ----
    std_probe <- as.POSIXct("2025-01-15 12:00:00", tz = tz_use)
    std_z     <- format(std_probe, "%z")           # for diagnostics
    std_h     <- tz_standard_offset_hours(tz_use)
    
    mmodal    <- tryCatch(
      tz_modal_offset_hours(dt_local),
      error = function(e)
        list(offset = NA_real_, z_mode = NA_character_)
    )
    modal_h   <- mmodal$offset
    z_mode    <- mmodal$z_mode
    
    offset_hours <- if (input$tz_offset_policy == "std")
      std_h
    else
      modal_h
    # ---- If NG requires integer hours, keep your rounding policy ----
    if (!isTRUE(all.equal(offset_hours, round(offset_hours)))) {
      warning(
        sprintf(
          "Time-zone offset has minutes (%.2f h). Rounding to nearest hour for make_inputs().",
          offset_hours
        )
      )
      offset_hours <- round(offset_hours)
    }
    if (is.na(offset_hours) || abs(offset_hours) > 14) {
      stop(sprintf("Computed GMT offset (%.2f) seems invalid.", offset_hours))
    }
    wx <- tibble::tibble(
      datetime = dt_local,
      year  = lubridate::year(dt_local),
      month = lubridate::month(dt_local),
      day   = lubridate::day(dt_local),
      hour  = lubridate::hour(dt_local),
      temp  = as.numeric(get_col(df, input$col_temp)),
      rh    = as.numeric(get_col(df, input$col_rh)),
      ws    = as.numeric(get_col(df, input$col_ws)),
      rain  = as.numeric(get_col(df, input$col_rain)),
      lat   = suppressWarnings(as.numeric(get_col(
        df, input$col_lat
      ))),
      long   = suppressWarnings(as.numeric(get_col(
        df, input$col_lon
      ))),
      tz    = tz_use
    )
    # print(all(!is.na(wx$temp)))
    # print(all(!is.na(wx$rh)))
    # print(all(!is.na(wx$ws)))
    # print(all(!is.na(wx$rain)))
    validate(need(all(!is.na(wx$temp)), "Temperature has NA after parsing."))
    validate(need(all(!is.na(wx$rh)), "RH has NA after parsing."))
    validate(need(all(!is.na(wx$ws)), "Wind has NA after parsing."))
    validate(need(all(!is.na(wx$rain)), "Rain has NA after parsing."))
    offset_arg <- as.numeric(offset_hours)
    # print(offset_arg)
    # inputs <- tryCatch({
    #   make_inputs(data.table::as.data.table(wx), timezone = offset_arg)
    # }, error = function(e) {
    #   stop("make_inputs() failed: ", conditionMessage(e))
    # })
    inputs<-wx
    list(
      inputs       = inputs,
      tz           = tz_use,
      tz_offset    = offset_hours,
      start_date   = input$start_date,
      n_rows       = nrow(wx),
      diag_std_z   = std_z,
      diag_modal_z = z_mode
    )
  })
  
  run_model <- reactive({
    req(shaped_input())
    si     <- shaped_input()
    inputs <- si$inputs
    
    validate(need(exists("hFWI"), "hFWI() not found after sourcing NG_FWI.r"))
    fml <- tryCatch(
      formals(hFWI),
      error = function(e)
        NULL
    )
    out <- NULL
    try({
      if (!is.null(fml)) {
        argn <- names(fml)
        if (all(c("df_wx", "timezone") %in% argn)) {
          out <- hFWI(
            df_wx = inputs,
            timezone = si$tz_offset,
            ffmc_old = input$ffmc0,
            dmc_old = input$dmc0,
            dc_old = input$dc0
          )
        } else if ("inputs" %in% argn) {
          out <- hFWI(
            inputs = inputs,
            ffmc0 = input$ffmc0,
            dmc0 = input$dmc0,
            dc0 = input$dc0
          )
        } else if ("df" %in% argn) {
          out <- hFWI(
            df = inputs,
            ffmc0 = input$ffmc0,
            dmc0 = input$dmc0,
            dc0 = input$dc0
          )
        } else {
          if (length(argn) >= 5) {
            out <- hFWI(inputs,
                        si$tz_offset,
                        input$ffmc0,
                        input$dmc0,
                        input$dc0)
          } else {
            out <- hFWI(inputs, input$ffmc0, input$dmc0, input$dc0)
          }
        }
      } else {
        out <- try(hFWI(
          df_wx = inputs,
          timezone = si$tz_offset,
          ffmc_old = input$ffmc0,
          dmc_old = input$dmc0,
          dc_old = input$dc0
        ),
        silent = TRUE)
        if (inherits(out, "try-error")) {
          out <- hFWI(
            inputs = inputs,
            ffmc0 = input$ffmc0,
            dmc0 = input$dmc0,
            dc0 = input$dc0
          )
        }
      }
    }, silent = TRUE)
    validate(need(
      !is.null(out),
      "hFWI() call failed; check the log for details."
    ))
    data.table::as.data.table(as.data.frame(out))
  })
  
  output$tbl <- DT::renderDT({
    req(run_model())
    DT::datatable(
      run_model(),
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        deferRender = T,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        processing=TRUE
      )
    )
  })
  
  # PATCH: safe fwrite fallback for Shinylive
  safe_fwrite <- function(x, file) {
    tryCatch(
      data.table::fwrite(x, file),
      error = function(e)
        utils::write.csv(x, file, row.names = FALSE)
    )
  }
  
  output$dl <- downloadHandler(
    filename = function()
      sprintf("hfwi_%s.csv", basename(input$csv)),
    content = function(file) {
      safe_fwrite(run_model(), file)
    },
    contentType = "text/csv"
  )
  
  output$log <- renderPrint({
    # Compute once to avoid re-running shaped_input() multiple times
    si <- shaped_input()
    
    cat("Rows read:", nrow(req(raw_file())), "\n")
    
    if (!is.null(si$start_date) && !is.na(si$start_date)) {
      cat("Start-date filter (local):",
          as.character(si$start_date),
          "\n")
    } else {
      cat("Start-date filter: (none)\n")
    }
    
    cat("Rows after filtering:", si$n_rows, "\n")
    cat("Time zone used:", si$tz, "\n")
    cat("GMT offset (hours) passed to make_inputs():",
        si$tz_offset,
        "\n")
    cat("Standard %z probe:", si$diag_std_z, "\n")
    if (!is.null(si$diag_modal_z) && nzchar(si$diag_modal_z)) {
      cat("Modal %z:", si$diag_modal_z, "\n")
    }
    
    cat("Initial codes: FFMC =",
        input$ffmc0,
        " DMC =",
        input$dmc0,
        " DC =",
        input$dc0,
        "\n")
    
    cat("hFWI() formals:\n")
    fml <- try(formals(hFWI), silent = TRUE)
    print(fml)
    
    # Robust arg name extraction
    argn <- if (inherits(fml, "try-error") ||
                is.null(fml))
      character(0)
    else
      names(fml)
    
    mapping <-
      if (all(c("df_wx", "timezone") %in% argn))
        "df_wx + timezone (NG)"
    else if ("inputs" %in% argn)
      "inputs (legacy)"
    else if ("df" %in% argn)
      "df (legacy)"
    else
      "positional fallback / unknown"
    
    cat("hFWI() call mapping -> ", mapping, "\n")
    
    # Optional: show the NG commit vendored during CI (if present)
    ng_commit <- tryCatch(
      readLines("ng/_ng_commit.txt", warn = FALSE)[1],
      error = function(e)
        NA_character_
    )
    if (isTRUE(nzchar(ng_commit)))
      cat("cffdrs-ng commit:", ng_commit, "\n")
  })
  
}

