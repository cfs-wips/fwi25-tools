ui <- fluidPage(
  theme = bslib::bs_theme(),
  title = NULL,
  tags$head(
    tags$title("FWI2025"),
    tags$link(rel = "stylesheet", type = "text/css", href = "gc_custom_style.css"),
    tags$script(src = "tz.js"),
    tags$script(src = "app-init.js")
  ),
  mod_i18n_ui("i18n"),
  tags$main(
    id = "main-content",
    role = "main",
    `aria-label` = "Hourly FWI application",
    div(
      class = "gc-layout-fill",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          mod_upload_ui("upload"),
          tags$form(
            id = "controls",
            mod_mapping_ui("mapping"),
            tags$hr(),
            mod_timezone_ui("tz"),
            # mod_filter_ui("filter"),
            mod_init_ui("init"),
            mod_actions_ui("actions")
          )
        ),
        mainPanel(
          width = 8,
          tags$div(
            id = "tabs-region",
            role = "region",
            `aria-label` = "Primary output tabs",
            tabsetPanel(
              id = "main_tabs",
              selected = "Output",
              tabPanel(
                title = textOutput("tab_output_title"),
                value = "Output",
                conditionalPanel(
                  condition = "!output.can_show_results",
                  tags$div(id = "pre-run-output-card", class = "gc-card", uiOutput("pre_run_output_msg"))
                ),
                conditionalPanel(
                  condition = "output.can_show_results",
                  tags$div(
                    id = "results-wrap",
                    mod_results_table_ui("results_table"),
                    mod_fwi87_table_ui("fwi87_table")
                  )
                )
              ),
              tabPanel(
                title = textOutput("tab_plot_title"),
                value = "Plot",
                conditionalPanel(
                  condition = "!output.can_show_results",
                  tags$div(id = "pre-run-plot", class = "gc-card", uiOutput("pre_run_plot_msg"))
                ),
                conditionalPanel(
                  condition = "output.can_show_results",
                  tags$div(id = "plot-wrap", mod_plot_ui("plot"))
                )
              ),
              tabPanel(
                title = textOutput("tab_log_title"),
                value = "Log",
                tags$div(role = "region", `aria-label` = "Run log", mod_log_ui("log"))
              ),
              tabPanel(
                title = textOutput("tab_inputs_title"),
                value = "Inputs",
                conditionalPanel(
                  condition = "!output.can_show_log",
                  tags$div(id = "pre-run-weather-card", class = "gc-card", uiOutput("pre_run_tables_msg"))
                ),
                conditionalPanel(
                  condition = "output.can_show_log", # Only show Inputs tab when data is ready
                  mod_inputs_ui("inputs")
                )
              )
            )
          )
        )
      )
    )
  ),
  tags$footer(
    class = "gc-footer",
    role = "contentinfo",
    `aria-label` = "Application footer",
    tags$div(
      class = "gc-footer__wrap",
      tags$div(
        class = "gc-footer__team",
        tags$img(src = "logo.png", alt = "Wildfire Intelligence & Predictive Services", class = "gc-footer__team-logo"),
        tags$span(class = "gc-footer__team-text", "Developed by the Wildfire Intelligence & Predictive Services Team")
      ),
      tags$div(
        class = "gc-footer__sig",
        tags$img(src = "nrcan_logo.svg", alt = "Ressources naturelles Canada / Natural Resources Canada", class = "gc-footer__sig-logo"),
        tags$div(class = "gc-footer__legal", HTML("© Government of Canada / Gouvernement du Canada"))
      )
    )
  )
)
