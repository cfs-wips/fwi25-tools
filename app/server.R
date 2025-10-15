# server.R (modularized)
library(shiny)

# Source NG-CFFDRS vendored code
source("ng/util.r", local = FALSE)
source("ng/make_inputs.r", local = FALSE)
source("ng/NG_FWI.r", local = FALSE)

# Source modules
for (f in list.files("R/modules", pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)

server <- function(input, output, session) {
  # i18n / language & UI texts
  i18n <- mod_i18n_server("i18n", session_title = TRUE)
  tr <- i18n$tr
  dt_i18n <- i18n$dt_i18n
  label_for_col <- i18n$label_for_col

  # Tabs titles via i18n
  output$tab_output_title <- renderText(tr("tab_output"))
  output$tab_plot_title <- renderText(tr("tab_plot"))
  output$tab_log_title <- renderText(tr("tab_log"))

  # Inputs modules
  up <- mod_upload_server("upload", tr)
  map <- mod_mapping_server("mapping", tr, cols = up$cols, df = up$raw_file)
  tz <- mod_timezone_server("tz", tr,
    manual_lat = map$manual_lat, manual_lon = map$manual_lon,
    browser_tz = reactive(input$tz_browser),
    lookup_result = reactive(input$tz_lookup_result)
  )
  fil <- mod_filter_server("filter", tr, raw_file = up$raw_file, mapping = map)
  init <- mod_init_server("init", tr)
  
  #Button Actions
  acts <- mod_actions_server("actions", tr, results = reactive(eng$run_model()), csv_name = up$csv_name)
  
  # Engine (compute on Run)
  eng <- mod_engine_server("engine",
    raw_file = up$raw_file, mapping = map, tz = tz, filt = fil, init = init, tr = tr,
    run_click=acts$run,
    debounce_ms = 400,   
    cache = "app",       
    enable_cache = TRUE  
  )

  # Outputs
  mod_results_table_server("results_table", tr, dt_i18n, results = reactive(eng$run_model()))
  mod_fwi87_table_server("fwi87_table", tr, dt_i18n, df87 = reactive(eng$daily_fwi_df()))
  mod_plot_server("plot", tr, label_for_col, shaped_input = reactive(eng$shaped_input()), results = reactive(eng$run_model()), df87 = reactive(eng$daily_fwi_df()))
  mod_log_server("log", shaped_input = reactive(eng$shaped_input()), raw_file = up$raw_file, df87 = reactive(eng$daily_fwi_df()), init = init,metrics = eng$metrics)
}
