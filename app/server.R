options(fwi.debug_times = FALSE)
# Set a safe default for all bindCache() calls that don't pass cache=
options(shiny.bindcache.default = "session")

# server.R (modularized)
library(shiny)
library(munsell)

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
    manual_lat = map$manual_lat,
    manual_lon = map$manual_lon,
    browser_tz = reactive(input$tz_browser),
    lookup_result = reactive(input$tz_lookup_result)
  )


  init <- mod_init_server("init", tr)

  # ---- Prepare (daily→hourly) ----
  prep <- mod_prepare_server(
    "prepare",
    raw_file = up$raw_file,
    mapping = map,
    tz = tz,
    diurnal_method_reactive = reactive(input$diurnal_method) # e.g., "BT-default"
  )

  # Make the LOG tab visible as soon as Prepare produces a data frame
  output$can_show_log <- reactive({
    !is.null(prep$raw_file())
  })
  outputOptions(output, "can_show_log", suspendWhenHidden = FALSE)

  fil <- mod_filter_server("filter", tr, tz, raw_file = up$raw_file, mapping = map)

  run_token <- reactiveVal(0L)


  observeEvent(up$raw_file(), {
    cat("[DEBUG] upload nrow:", if (!is.null(up$raw_file())) nrow(up$raw_file()) else NA, "\n")
  })
  observeEvent(prep$raw_file(), {
    cat("[DEBUG] prepare nrow:", if (!is.null(prep$raw_file())) nrow(prep$raw_file()) else NA, "\n")
    print(prep$prep_meta())
  })


  # ---- Engine (compute on run_token instead of button directly) ----
  eng <- mod_engine_server(
    "engine",
    raw_file = prep$raw_file, # USE PREPARED INPUT (was up$raw_file)
    mapping = map,
    tz = tz,
    filt = fil,
    init = init,
    tr = tr,
    run_click = reactive(run_token()),
    debounce_ms = 400,
    cache = "session",
    enable_cache = TRUE
  )

  # ---- Actions (Run button) ----
  acts <- mod_actions_server(
    "actions", tr,
    results = reactive(eng$run_model()),
    csv_name = up$csv_name
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
  # Show/hide results instantly on Run; reset on new CSV; defer compute
  # =====================================================================

  # App-level "ran" flag: TRUE only after Run HFWI pressed post-upload
  app_state <- reactiveValues(ran = FALSE)

  # Expose a reactive output for conditionalPanel
  output$can_show_results <- reactive({
    isTRUE(app_state$ran)
  })
  outputOptions(output, "can_show_results", suspendWhenHidden = FALSE)

  # When a NEW CSV is selected -> reset controls and return to pre-run state
  observeEvent(up$csv_name(), ignoreInit = TRUE, {
    app_state$ran <- FALSE
    # Reset the <form id="controls"> without shinyjs (native browser reset)
    session$sendCustomMessage("form-reset", "controls")
  })

  # When Run HFWI is clicked:
  # 1) Flip UI right away via app_state$ran (conditionalPanel reacts)
  # 2) Defer the heavy compute to after the next UI flush (one-tick yield)
  observeEvent(acts$run(), {
    app_state$ran <- TRUE
    # Yield to the browser to paint the changes, then trigger compute
    session$onFlushed(function() {
      run_token(isolate(run_token()) + 1L)
    }, once = TRUE)
  })

  # ---- Outputs (modules) ----
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
    tab_active = reactive(input$main_tabs) # NEW
  )

  mod_log_server(
    "log",
    shaped_input = reactive(eng$shaped_input()),
    raw_file     = prep$raw_file, # pass prepared so provenance + prep_log are visible
    df87         = reactive(eng$daily_fwi_df()),
    init         = init,
    metrics      = eng$metrics
  )
}
