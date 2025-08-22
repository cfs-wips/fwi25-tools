
# app.R (patched v3: simple %z integer/100 parsing + diagnostics)
library(shiny)
library(data.table)
library(dplyr)
library(lubridate)
library(vroom)
library(stringr)
library(purrr)
library(lutz)
library(DT)

# ---- Load NG-CFFDRS R code (local vendor) ----
ng_dir <- "ng"
req_files <- c("NG_FWI.r", "make_inputs.r", "util.r", "old_cffdrs.r")
missing <- req_files[!file.exists(file.path(ng_dir, req_files))]
if (length(missing)) {
  stop("Missing required NG-CFFDRS R files in 'ng/': ", paste(missing, collapse = ", "),
       call. = FALSE)
}
sys.source(file.path(ng_dir, "util.r"), envir = globalenv(), chdir = TRUE)
sys.source(file.path(ng_dir, "old_cffdrs.r"), envir = globalenv(), chdir = TRUE)
sys.source(file.path(ng_dir, "make_inputs.r"),envir = globalenv(), chdir = TRUE)
sys.source(file.path(ng_dir, "NG_FWI.r"), envir = globalenv(), chdir = TRUE)

ui <- fluidPage(
  titlePanel("Hourly FWI (NG-CFFDRS) – upload weather and run hFWI()"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csv", "Upload weather CSV", accept = c(".csv"), buttonLabel = "Browse…"),
      checkboxInput("has_header", "CSV has header", TRUE),
      hr(),
      h4("Column mapping"),
      uiOutput("mapping_ui"),
      hr(),
      h4("Time zone"),
      radioButtons("tz_mode", NULL,
                   choices = c("Specify one time zone for all rows" = "fixed",
                               "Infer a single time zone from latitude/longitude" = "auto"),
                   selected = "auto"),
      conditionalPanel(
        "input.tz_mode === 'fixed'",
        selectInput("fixed_tz", "Time zone", choices = OlsonNames(),
                    selected = "America/Edmonton")
      ),
      radioButtons(
        "tz_offset_policy",
        "Offset for solar calculations",
        choices = c("Standard (ignore DST)" = "std",
                    "From data (modal; may include DST)" = "modal"),
        selected = "std"
      ),
      hr(),
      h4("Filter"),
      dateInput(
        "start_date",
        "Drop rows prior to (local date):",
        value = NA,
        format = "yyyy-mm-dd"
      ),
      helpText("If set, rows with local date < this value are removed before hFWI()."),
      numericInput("ffmc0", "Initial FFMC", value = 85, min = 0),
      numericInput("dmc0", "Initial DMC", value = 6, min = 0),
      numericInput("dc0", "Initial DC", value = 15, min = 0),
      hr(),
      actionButton("run", "Run hFWI()", class = "btn-primary"),
      br(), br(),
      downloadButton("dl", "Download results (CSV)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Output", DTOutput("tbl")),
        tabPanel("Log", verbatimTextOutput("log"))
      )
    )
  )
)

server <- function(input, output, session) {
  # --- Helpers: robust offset computation using integer %z / 100 ---
  # This avoids substring pitfalls across platforms.
  tz_standard_offset_hours <- function(tz, probe_date = "2025-01-15 12:00:00") {
    probe <- as.POSIXct(probe_date, tz = tz)
    z_txt <- format(probe, "%z")         # e.g., "-0500", "+0930", or sometimes "0500"
    z_int <- suppressWarnings(as.integer(z_txt))
    if (!is.na(z_int)) {
      off <- z_int / 100                    # "-0500" -> -5, "0930" -> 9.3 (half-hours preserved)
      # Some platforms might drop leading zeros ("-500"). Still fine: -500/100 = -5
      return(off)
    }
    stop(sprintf("Could not parse %%z for TZ '%s' (got '%s')", tz, z_txt), call. = FALSE)
  }
  tz_modal_offset_hours <- function(datetimes) {
    z_txt <- format(datetimes, "%z")
    z_txt <- z_txt[nzchar(z_txt)]
    if (!length(z_txt)) stop("Could not infer modal offset: empty %z values.")
    z_mode <- names(which.max(table(z_txt)))
    z_int  <- suppressWarnings(as.integer(z_mode))
    if (!is.na(z_int)) return(z_int / 100)
    stop(sprintf("Could not parse modal %%z ('%s')", z_mode))
  }
  
  raw_file <- reactive({
    req(input$csv)
    vroom::vroom(input$csv$datapath, delim = ",",
                 col_types = vroom::cols(.default = vroom::col_guess()),
                 na = c("", "NA", "NaN", "null"),
                 progress = FALSE)
  })
  
  output$mapping_ui <- renderUI({
    df <- raw_file()
    cols <- names(df)
    tagList(
      helpText("Map your columns to what NG-CFFDRS expects. Provide either a single Date-Time column, or Year/Month/Day/Hour."),
      selectInput("col_datetime", "Date-Time (optional)", choices = c("", cols), selected = ""),
      fluidRow(
        column(3, selectInput("col_year",  "Year",  choices = c("", cols))),
        column(3, selectInput("col_month", "Month", choices = c("", cols))),
        column(3, selectInput("col_day",   "Day",   choices = c("", cols))),
        column(3, selectInput("col_hour",  "Hour (0-23)", choices = c("", cols)))
      ),
      fluidRow(
        column(3, selectInput("col_temp", "Temperature (°C)", choices = cols)),
        column(3, selectInput("col_rh",   "RH (%)",          choices = cols)),
        column(3, selectInput("col_ws",   "Wind (km/h)",     choices = cols)),
        column(3, selectInput("col_rain", "Rain (mm)",       choices = cols))
      ),
      fluidRow(
        column(6, selectInput("col_lat", "Latitude (deg)",  choices = c("", cols))),
        column(6, selectInput("col_lon", "Longitude (deg)", choices = c("", cols)))
      )
    )
  })
  
  get_col <- function(df, nm) {
    if (isTruthy(nm) && nzchar(nm) && nm %in% names(df)) df[[nm]] else NULL
  }
  
  shaped_input <- eventReactive(input$run, {
    validate(need(!is.null(raw_file()), "Upload a CSV first."))
    df <- as_tibble(raw_file())
    
    needed <- c(input$col_temp, input$col_rh, input$col_ws, input$col_rain)
    validate(need(all(nzchar(needed)), "Please map temperature, RH, wind, and rain columns."))
    
    tz_use <- switch(
      input$tz_mode,
      "fixed" = input$fixed_tz,
      "auto"  = {
        lat <- suppressWarnings(as.numeric(get_col(df, input$col_lat)))
        lon <- suppressWarnings(as.numeric(get_col(df, input$col_lon)))
        validate(need(!is.null(lat) && !is.null(lon),
                      "To auto-detect time zone, map Latitude and Longitude."))
        tz_guess <- tryCatch({
          unique(lutz::tz_lookup_coords(mean(lat, na.rm = TRUE),
                                        mean(lon, na.rm = TRUE),
                                        method = "accurate", warn = FALSE))
        }, error = function(e) NA_character_)
        validate(need(isTRUE(length(tz_guess) == 1L) && isTRUE(nzchar(tz_guess)),
                      "Could not infer a unique time zone from lat/lon. Use 'Specify one time zone'."))
        tz_guess
      }
    )
    
    dt_col <- get_col(df, input$col_datetime)
    
    has_explicit_zone <- function(x) {
      if (is.null(x)) return(FALSE)
      x <- as.character(x)
      any(grepl("(Z$)|([+-]\\d{2}:?\\d{2}$)", x, ignore.case = TRUE), na.rm = TRUE)
    }
    
    if (!is.null(dt_col)) {
      if (has_explicit_zone(dt_col)) {
        dt_utc <- lubridate::parse_date_time(
          dt_col,
          orders = c("Y-m-d H:M:S","Y-m-d H:M","Y/m/d H:M:S","Y/m/d H:M",
                     "d-m-Y H:M:S","d/m/Y H:M:S","m/d/Y H:M:S","m/d/Y H:M",
                     "Ymd HMS","Ymd HM","Ymd H"),
          tz = "UTC"
        )
        validate(need(!all(is.na(dt_utc)), "Could not parse your Date-Time column (UTC/offset)."))
        dt_local <- lubridate::with_tz(dt_utc, tz = tz_use)
      } else {
        dt_local <- lubridate::parse_date_time(
          dt_col,
          orders = c("Y-m-d H:M:S","Y-m-d H:M","Y/m/d H:M:S","Y/m/d H:M",
                     "d-m-Y H:M:S","d/m/Y H:M:S","m/d/Y H:M:S","m/d/Y H:M",
                     "Ymd HMS","Ymd HM","Ymd H"),
          tz = tz_use
        )
        validate(need(!all(is.na(dt_local)), "Could not parse your Date-Time column."))
      }
    } else {
      y <- get_col(df, input$col_year)
      m <- get_col(df, input$col_month)
      d <- get_col(df, input$col_day)
      h <- get_col(df, input$col_hour)
      validate(need(all(!is.null(c(y,m,d,h))), "Provide Date-Time or Year/Month/Day/Hour."))
      dt_local <- lubridate::make_datetime(
        year  = as.integer(y),
        month = as.integer(m),
        day   = as.integer(d),
        hour  = as.integer(h),
        tz    = tz_use
      )
    }
    
    if (!is.null(input$start_date) && !is.na(input$start_date)) {
      keep <- as.Date(dt_local, tz = tz_use) >= as.Date(input$start_date)
      validate(need(any(keep), "All rows were filtered out by the start date."))
      df       <- df[keep, , drop = FALSE]
      dt_local <- dt_local[keep]
    }
    
    # --- Compute offset (hours) via simple integer %z / 100 ---
    std_probe <- as.POSIXct("2025-01-15 12:00:00", tz = tz_use)
    std_z <- format(std_probe, "%z")
    std_h <- suppressWarnings(as.integer(std_z)) / 100
    
    modal_h <- {
      z_all <- format(dt_local, "%z")
      z_all <- z_all[nzchar(z_all)]
      if (length(z_all)) {
        z_mode <- names(which.max(table(z_all)))
        suppressWarnings(as.integer(z_mode)) / 100
      } else NA_real_
    }
    
    offset_hours <- if (input$tz_offset_policy == "std") std_h else modal_h
    
    # Safety: correct obviously wrong magnitudes (e.g., 50 from bad substring logic)
    if (!is.na(offset_hours) && abs(offset_hours) >= 24 && (offset_hours %% 10 == 0)) {
      offset_hours <- offset_hours / 10
    }
    
    # Round to nearest hour for make_inputs() (preserve half-hours if you remove this)
    if (!isTRUE(all.equal(offset_hours, round(offset_hours)))) {
      warning(sprintf("Time-zone offset has minutes (%.2f h). Rounding to nearest hour for make_inputs().",
                      offset_hours))
      offset_hours <- round(offset_hours)
    }
    if (is.na(offset_hours) || abs(offset_hours) > 14) {
      stop(sprintf("Computed GMT offset (%.2f) seems invalid.", offset_hours))
    }
    
    wx <- tibble(
      datetime = dt_local,
      year  = lubridate::year(dt_local),
      month = lubridate::month(dt_local),
      day   = lubridate::day(dt_local),
      hour  = lubridate::hour(dt_local),
      temp  = as.numeric(get_col(df, input$col_temp)),
      rh    = as.numeric(get_col(df, input$col_rh)),
      ws    = as.numeric(get_col(df, input$col_ws)),
      rain  = as.numeric(get_col(df, input$col_rain)),
      lat   = suppressWarnings(as.numeric(get_col(df, input$col_lat))),
      lon   = suppressWarnings(as.numeric(get_col(df, input$col_lon))),
      tz    = tz_use
    )
    
    validate(need(all(!is.na(wx$temp)), "Temperature has NA after parsing."))
    validate(need(all(!is.na(wx$rh)),   "RH has NA after parsing."))
    validate(need(all(!is.na(wx$ws)),   "Wind has NA after parsing."))
    validate(need(all(!is.na(wx$rain)), "Rain has NA after parsing."))
    
    inputs <- tryCatch({
      make_inputs(as.data.table(wx), timezone = as.numeric(offset_hours))
    }, error = function(e) {
      stop("make_inputs() failed: ", conditionMessage(e))
    })
    
    list(
      inputs     = inputs,
      tz         = tz_use,
      tz_offset  = offset_hours,
      start_date = input$start_date,
      n_rows     = nrow(wx),
      diag_std_z = std_z,
      diag_modal_z = if (exists("z_mode")) z_mode else NA_character_
    )
  })
  
  run_model <- reactive({
    req(shaped_input())
    si <- shaped_input()
    inputs <- si$inputs
    
    # ---- Call hFWI() defensively based on its current signature ----
    validate(need(exists("hFWI"), "hFWI() not found after sourcing NG_FWI.r"))
    fml <- tryCatch(formals(hFWI), error = function(e) NULL)
    out <- NULL
    try({
      if (!is.null(fml)) {
        argn <- names(fml)
        if (all(c("df_wx","timezone") %in% argn)) {
          # NG signature: hFWI(df_wx, timezone, ffmc_old, dmc_old, dc_old)
          out <- hFWI(df_wx   = inputs,
                      timezone = si$tz_offset,
                      ffmc_old = input$ffmc0,
                      dmc_old  = input$dmc0,
                      dc_old   = input$dc0)
        } else if ("inputs" %in% argn) {
          out <- hFWI(inputs = inputs,
                      ffmc0 = input$ffmc0,
                      dmc0  = input$dmc0,
                      dc0   = input$dc0)
        } else if ("df" %in% argn) {
          out <- hFWI(df = inputs,
                      ffmc0 = input$ffmc0,
                      dmc0  = input$dmc0,
                      dc0   = input$dc0)
        } else {
          # LAST resort: attempt a 5-argument positional mapping (df, tz, ffmc, dmc, dc)
          if (length(argn) >= 5) {
            out <- hFWI(inputs, si$tz_offset, input$ffmc0, input$dmc0, input$dc0)
          } else {
            out <- hFWI(inputs, input$ffmc0, input$dmc0, input$dc0)
          }
        }
      } else {
        # If we can't introspect at all, try NG signature first; fallback to legacy
        out <- try(hFWI(df_wx = inputs,
                        timezone = si$tz_offset,
                        ffmc_old = input$ffmc0,
                        dmc_old  = input$dmc0,
                        dc_old   = input$dc0), silent = TRUE)
        if (inherits(out, "try-error")) {
          out <- hFWI(inputs = inputs,
                      ffmc0 = input$ffmc0,
                      dmc0  = input$dmc0,
                      dc0   = input$dc0)
        }
      }
    }, silent = TRUE)
    validate(need(!is.null(out), "hFWI() call failed; check the log for details."))
    as.data.table(out)
  })
  
  
  output$tbl <- renderDT({
    req(run_model())
    datatable(run_model(), filter = "top", extensions = "Buttons",
              options = list(pageLength = 25, scrollX = TRUE,
                             dom = "Bfrtip",
                             buttons = c("copy","csv","excel")))
  })
  
  output$dl <- downloadHandler(
    filename = function() sprintf("hfwi_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      fwrite(run_model(), file)
    }
  )
  
  output$log <- renderPrint({
    cat("Rows read:", nrow(req(raw_file())), "\n")
    if (!is.null(shaped_input()$start_date) && !is.na(shaped_input()$start_date)) {
      cat("Start-date filter (local):", as.character(shaped_input()$start_date), "\n")
    } else {
      cat("Start-date filter: (none)\n")
    }
    cat("Rows after filtering:", shaped_input()$n_rows, "\n")
    cat("Time zone used:", shaped_input()$tz, "\n")
    cat("GMT offset (hours) passed to make_inputs():", shaped_input()$tz_offset, "\n")
    cat("Standard %z probe:", shaped_input()$diag_std_z, "\n")
    if (!is.null(shaped_input()$diag_modal_z)) cat("Modal %z:", shaped_input()$diag_modal_z, "\n")
    cat("Initial codes: FFMC =", input$ffmc0, " DMC =", input$dmc0, " DC =", input$dc0, "\n")
    cat("hFWI() formals:", capture.output(try(formals(hFWI))), sep = "\n")
    cat("hFWI() call mapping -> ",
        if (exists("fml")) {
          argn <- names(try(formals(hFWI)))
          if (all(c("df_wx","timezone") %in% argn)) "df_wx + timezone (NG)"
          else if ("inputs" %in% argn) "inputs (legacy)"
          else if ("df" %in% argn) "df (legacy)"
          else "positional fallback"
        } else "unknown",
        "\n")
    
  })
}

shinyApp(ui, server)
