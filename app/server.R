server <- function(input, output, session) {
  # --- Reset token (kept from your original; used by tz module) ----------------
  reset_token <- reactiveVal(0L)
  bump_reset <- function() reset_token(isolate(reset_token()) + 1L)

  # --- i18n / language & UI texts ---------------------------------------------
  i18n <- mod_i18n_server("i18n", session_title = TRUE)
  tr <- i18n$tr
  dt_i18n <- i18n$dt_i18n
  label_for_col <- i18n$label_for_col

  # Tab titles
  output$tab_output_title <- renderText(tr("tab_output"))
  output$tab_plot_title <- renderText(tr("tab_plot"))
  output$tab_log_title <- renderText(tr("tab_log"))
  output$tab_inputs_title <- renderText(tr("data_src_inputs"))

  # Pre-run messages
  output$pre_run_output_msg <- output$pre_run_tables_msg <- renderUI({
    tags$p(tr("hint_upload_and_run"), " ", tags$strong(tr("run_hfwi")), " ", tr("hint_generate_tables"))
  })
  output$pre_run_plot_msg <- renderUI({
    tags$p(tr("hint_upload_to_view"))
  })

  # --- Upload + mapping -------------------------------------------------------
  up <- mod_upload_server("upload", isolate(tr), i18n$lang)
  map <- mod_mapping_server("mapping", isolate(tr), lang = i18n$lang, cols = up$cols, df = up$raw_file)

  tz <- mod_timezone_server(
    id = "tz", tr,
    manual_lat = map$manual_lat,
    manual_lon = map$manual_lon,
    browser_tz = reactive(input$tz_browser),
    lookup_result = reactive(input$tz_lookup_result),
    raw_file = up$raw_file,
    reset = reactive(reset_token())
  )

  fil <- mod_filter_server("filter", isolate(tr), tz, raw_file = up$raw_file, mapping = map)
  init <- mod_init_server("init", isolate(tr))

  # --- Prepare (daily→hourly) -------------------------------------------------
  prep <- mod_prepare_server(
    id = "prepare",
    raw_file = up$raw_file,
    mapping = map,
    tz = tz,
    filter = fil,
    diurnal_method_reactive = reactive(input$diurnal_method),
    notify = TRUE
  )

  # --- Inputs tab -------------------------------------------------------------
  mod_inputs_server(
    id = "inputs",
    tr = isolate(tr),
    dt_i18n = dt_i18n,
    raw_data = prep$raw_uploaded, # original upload
    hourly_data = prep$hourly_file # hourly (converted or passthrough)
  )

  # --- Visibility gates -------------------------------------------------------
  # Gate for Inputs tab (Log visibility): show only when hourly is ready AND Prepare isn't busy.
  output$can_show_log <- reactive({
    !is.null(prep$hourly_file()) && !isTRUE(prep$busy())
  })
  outputOptions(output, "can_show_log", suspendWhenHidden = FALSE)

  # Gate for Results & Plot tabs: show only after an explicit "Run HFWI".
  app_state <- reactiveValues(ran = FALSE)
  output$can_show_results <- reactive({
    isTRUE(app_state$ran)
  })
  outputOptions(output, "can_show_results", suspendWhenHidden = FALSE)

  # --- Engine -----------------------------------------------------------------
  run_token <- reactiveVal(0L)
  eng <- mod_engine_server(
    id = "engine",
    raw_hourly = prep$hourly_file,
    daily_src = prep$src_daily,
    mapping = map,
    tz = tz,
    filt = fil,
    init = init,
    tr = isolate(tr),
    run_click = debounce(run_token, 400),
    cache = "app",
    enable_cache = TRUE
  )

  # --- Actions / Run HFWI -----------------------------------------------------
  acts <- mod_actions_server(
    "actions", isolate(tr),
    results = reactive(eng$run_model()),
    csv_name = up$csv_name
  )

  # When the user presses Run HFWI: flip to results-visible and bump the run token.
  observeEvent(acts$run(), {
    app_state$ran <- TRUE
    session$onFlushed(function() {
      run_token(isolate(run_token()) + 1L)
    }, once = TRUE)
  })

  # --- Clear displays when source changes ------------------------------------
  # A) New CSV uploaded -> back to pre-run state (hides Results/Plot; Inputs will show once Prepare finishes)
  observeEvent(up$raw_file(), ignoreInit = TRUE, {
    app_state$ran <- FALSE
    # If you ever want to reset TZ/UI forms visually, you can call:
    session$sendCustomMessage("form-reset", "controls")
    bump_reset() # optional: only if you want to trigger a TZ reset on new uploads
  })

  # B) Prepare publishes a new dataset (e.g., mapping or filter causes a new output)
  #    -> back to pre-run state until the next Run HFWI.
  observeEvent(prep$prep_meta(), ignoreInit = TRUE, {
    pm <- prep$prep_meta()
    # Use run_id existence as a simple signal of a completed Prepare publish
    if (!is.null(pm$run_id)) {
      app_state$ran <- FALSE
    }
  })

  # --- Plot reseed (kept from your original) ----------------------------------
  reseed_tick <- reactiveVal(0L)
  bump_reseed <- function() reseed_tick(isolate(reseed_tick()) + 1L)
  observeEvent(input$main_tabs,
    {
      if (identical(input$main_tabs, "Plot")) bump_reseed()
    },
    ignoreInit = TRUE
  )

  # --- Outputs (tables, plot, log) -------------------------------------------
  mod_results_table_server(
    "results_table", isolate(tr), dt_i18n,
    results = eng$run_model,
    tz_reactive = tz$tz_use,
    ignore_dst_reactive = tz$tz_offset_policy
  )

  mod_fwi87_table_server(
    "fwi87_table", isolate(tr), dt_i18n,
    df87 = eng$daily_fwi_df,
    tz_reactive = tz$tz_use,
    ignore_dst_reactive = tz$tz_offset_policy
  )

  mod_plot_server(
    "plot", isolate(tr), i18n$lang, label_for_col,
    shaped_input = eng$shaped_input,
    results = eng$run_model,
    df87 = eng$daily_fwi_df,
    tz_reactive = tz$tz_use,
    ignore_dst_reactive = tz$tz_offset_policy,
    tab_active = reactive(input$main_tabs)
  )

  mod_log_server(
    "log",
    shaped_input = reactive(eng$shaped_input()),
    raw_file     = prep$prep_meta,
    df87         = reactive(eng$daily_fwi_df()),
    init         = init,
    prep_meta    = prep$prep_meta,
    metrics      = eng$metrics
  )
}
