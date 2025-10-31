if (interactive()) {
  reactlog::reactlog_enable()
}
options(fwi.debug_times = FALSE)

# server.R (modularized)
library(shiny)

# Source NG-CFFDRS vendored code
source("ng/util.r", local = FALSE)
source("ng/make_inputs.r", local = FALSE)
source("ng/NG_FWI.r", local = FALSE)

# Source modules
for (f in list.files("R/modules", pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)

server <- function(input, output, session) {
  # ---- i18n / language & UI texts ----
  i18n <- mod_i18n_server("i18n", session_title = TRUE)
  tr <- i18n$tr
  dt_i18n <- i18n$dt_i18n
  label_for_col <- i18n$label_for_col

  # Tab titles via i18n
  output$tab_output_title <- renderText(tr("tab_output"))
  output$tab_plot_title <- renderText(tr("tab_plot"))
  output$tab_log_title <- renderText(tr("tab_log"))

  # ---- Upload + inputs ----
  up <- mod_upload_server("upload", tr)
  map <- mod_mapping_server("mapping", tr, cols = up$cols, df = up$raw_file)
  tz <- mod_timezone_server(
    "tz", tr,
    manual_lat = map$manual_lat, manual_lon = map$manual_lon,
    browser_tz = reactive(input$tz_browser),
    lookup_result = reactive(input$tz_lookup_result)
  )
  fil <- mod_filter_server("filter", tr, tz, raw_file = up$raw_file, mapping = map)
  init <- mod_init_server("init", tr)

  # ---- Actions (Run button) ----
  # NOTE: eng$run_model is referenced in actions for download naming, so acts is created before eng
  acts <- mod_actions_server(
    "actions", tr,
    results = reactive(eng$run_model()),
    csv_name = up$csv_name
  )

  # ---- Engine (compute on Run) ----
  eng <- mod_engine_server(
    "engine",
    raw_file = up$raw_file, mapping = map, tz = tz, filt = fil, init = init, tr = tr,
    run_click = acts$run,
    debounce_ms = 400,
    cache = "app", enable_cache = TRUE
  )

  # ---- Plot reseed on Plot tab enter (unchanged) ----
  reseed_tick <- reactiveVal(0L)
  bump_reseed <- function() reseed_tick(isolate(reseed_tick()) + 1L)
  observeEvent(input$main_tabs,
    {
      if (identical(input$main_tabs, "Plot")) bump_reseed()
    },
    ignoreInit = TRUE
  )

  # =====================================================================
  # Item 3: Reset on CSV upload + require Run HFWI again for outputs
  # =====================================================================

  # App-level "ran" flag: TRUE only after Run HFWI pressed post-upload
  app_state <- reactiveValues(ran = FALSE)

  # Expose a reactive output for conditionalPanel gating in the UI
  output$can_show_results <- reactive({
    isTRUE(app_state$ran)
  })
  outputOptions(output, "can_show_results", suspendWhenHidden = FALSE)

  # When a NEW CSV is selected -> reset inputs (except file), hide results
  observeEvent(up$csv_name(), ignoreInit = TRUE, {
    app_state$ran <- FALSE
    # Reset all user inputs except the fileInput by resetting the 'controls' form
    shinyjs::reset("controls")
  })

  # When Run HFWI is clicked -> mark results as current/visible
  observeEvent(acts$run(), {
    app_state$ran <- TRUE
  })

  # ---- Outputs ----
  mod_results_table_server(
    "results_table", tr, dt_i18n,
    results = eng$run_model,
    tz_reactive = tz$tz_use,
    ignore_dst_reactive = tz$tz_offset_policy # <- wire the toggle here
  )

  mod_fwi87_table_server(
    "fwi87_table", tr, dt_i18n,
    df87 = eng$daily_fwi_df,
    tz_reactive = tz$tz_use,
    ignore_dst_reactive = tz$tz_offset_policy # <- and here
  )

  mod_plot_server(
    "plot", tr, i18n, label_for_col,
    shaped_input = eng$shaped_input_preview,
    results = eng$run_model,
    df87 = eng$daily_fwi_df, # overlay only
    tz_reactive = tz$tz_use, # <- Olson TZ
    ignore_dst_reactive = tz$tz_offset_policy # <- your radio/toggle
  )

  mod_log_server(
    "log",
    shaped_input = reactive(eng$shaped_input()),
    raw_file     = up$raw_file,
    df87         = reactive(eng$daily_fwi_df()),
    init         = init,
    metrics      = eng$metrics
  )
}
