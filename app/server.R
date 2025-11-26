options(fwi.debug_times = FALSE)
options(shiny.bindcache.default = "session")
reactlog::reactlog_enable()
library(shiny)
library(munsell)

# Source NG-CFFDRS vendored code
# source("ng/util.r", local = FALSE)
# source("ng/NG_FWI.r", local = FALSE)
# source("ng/make_inputs.r", local = FALSE)
# Source my vectorized version of the ng-cffdrs code
source("ng/util_vectorized.r", local = FALSE)
source("ng/NG_FWI_vectorized.r", local = FALSE)


# Source modules
for (f in list.files("R/modules", pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)

server <- function(input, output, session) {
  # ---- i18n / language & UI texts ----
  i18n <- mod_i18n_server("i18n", session_title = TRUE)
  tr <- i18n$tr
  dt_i18n <- i18n$dt_i18n
  label_for_col <- i18n$label_for_col

  # Tab titles
  output$tab_output_title <- renderText(tr("tab_output"))
  output$tab_plot_title <- renderText(tr("tab_plot"))
  output$tab_log_title <- renderText(tr("tab_log"))
  output$tab_inputs_title <- renderText(tr("data_src_inputs"))

  # Dynamic pre-run messages
  output$pre_run_output_msg <- output$pre_run_tables_msg <- renderUI({
    tags$p(tr("hint_upload_and_run"), " ", tags$strong(tr("run_hfwi")), " ", tr("hint_generate_tables"))
  })

  output$pre_run_plot_msg <- renderUI({
    tags$p(tr("hint_upload_to_view"))
  })

  # ---- Upload + mapping ----
  up <- mod_upload_server("upload", tr)
  map <- mod_mapping_server("mapping", tr, cols = up$cols, df = up$raw_file)
  tz <- mod_timezone_server(
    "tz", tr,
    manual_lat = map$manual_lat,
    manual_lon = map$manual_lon,
    browser_tz = reactive(input$tz_browser),
    lookup_result = reactive(input$tz_lookup_result)
  )
  init <- mod_init_server("init", tr)

  # ---- Prepare (daily→hourly) ----
  prep <- mod_prepare_server(
    id = "prepare",
    raw_file = up$raw_file,
    mapping = map,
    tz = tz,
    diurnal_method_reactive = reactive(input$diurnal_method),
    skip_invalid = TRUE,
    notify = TRUE
  )

  # Inputs tab
  mod_inputs_server(
    id = "inputs",
    tr = tr,
    dt_i18n = dt_i18n,
    raw_data = prep$raw_uploaded, # original upload
    hourly_data = prep$hourly_file # hourly (converted or passthrough)
  )

  # Show/hide Log tab
  output$can_show_log <- reactive({
    !is.null(prep$hourly_file())
  })
  outputOptions(output, "can_show_log", suspendWhenHidden = FALSE)

  fil <- mod_filter_server("filter", tr, tz, raw_file = up$raw_file, mapping = map)

  run_token <- reactiveVal(0L)

  # ---- Engine ----
  eng <- mod_engine_server(
    id = "engine",
    raw_hourly = prep$hourly_file,
    daily_src = prep$src_daily,
    mapping = map,
    tz = tz,
    filt = fil,
    init = init,
    tr = tr,
    run_click = debounce(run_token, 400),
    cache = "app",
    enable_cache = TRUE
  )

  # ---- Actions ----
  acts <- mod_actions_server(
    "actions", tr,
    results = reactive(eng$run_model()),
    csv_name = up$csv_name
  )

  # ---- Plot reseed ----
  reseed_tick <- reactiveVal(0L)
  bump_reseed <- function() reseed_tick(isolate(reseed_tick()) + 1L)
  observeEvent(input$main_tabs,
    {
      if (identical(input$main_tabs, "Plot")) bump_reseed()
    },
    ignoreInit = TRUE
  )

  # ---- Results visibility ----
  app_state <- reactiveValues(ran = FALSE)
  output$can_show_results <- reactive({
    isTRUE(app_state$ran)
  })
  outputOptions(output, "can_show_results", suspendWhenHidden = FALSE)

  observeEvent(up$csv_name(), ignoreInit = TRUE, {
    app_state$ran <- FALSE
    session$sendCustomMessage("form-reset", "controls")
  })

  observeEvent(acts$run(), {
    app_state$ran <- TRUE
    session$onFlushed(function() {
      run_token(isolate(run_token()) + 1L)
    }, once = TRUE)
  })

  # ---- Outputs ----
  mod_results_table_server(
    "results_table", tr, dt_i18n,
    results = eng$run_model,
    tz_reactive = tz$tz_use,
    ignore_dst_reactive = tz$tz_offset_policy
  )
  mod_fwi87_table_server(
    "fwi87_table", tr, dt_i18n,
    df87 = eng$daily_fwi_df,
    tz_reactive = tz$tz_use,
    ignore_dst_reactive = tz$tz_offset_policy
  )
  mod_plot_server(
    "plot", tr, i18n, label_for_col,
    shaped_input = eng$shaped_input_preview,
    results = eng$run_model,
    df87 = eng$daily_fwi_df,
    tz_reactive = tz$tz_use,
    ignore_dst_reactive = tz$tz_offset_policy,
    tab_active = reactive(input$main_tabs)
  )
  mod_log_server(
    "log",
    shaped_input = reactive(eng$shaped_input()),
    raw_file = prep$prep_meta,
    df87 = reactive(eng$daily_fwi_df()),
    init = init,
    metrics = eng$metrics
  )
}
