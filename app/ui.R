downloadButton_sl <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}
downloadLink_sl <- function(...) {
  tag <- shiny::downloadLink(...)
  tag$attribs$download <- NULL
  tag
}

for (f in list.files("R/modules", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}

ui <- fluidPage(
  title = NULL,
  # shinyjs::useShinyjs(),
  tags$head(
    tags$title("FWI2025"),
    # Accessibility and dynamic title
    tags$script(HTML("
      Shiny.addCustomMessageHandler('set-title', function(msg){
        try{
          document.title = msg;
          if(window.parent && window.parent !== window){
            window.parent.postMessage({type:'set-title', title: msg}, '*');
          }
        }catch(e){}
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('set-aria-labels', function(msg){
        try{
          if(msg && msg.app) document.getElementById('main-content')?.setAttribute('aria-label', msg.app);
          if(msg && msg.tabs) document.getElementById('tabs-region')?.setAttribute('aria-label', msg.tabs);
        } catch(e){}
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('mappingSetDisabled', function(msg) {
        var el = document.getElementById(msg.id);
        if (!el) return;
        var disabled = !!msg.disabled;
        el.setAttribute('aria-disabled', disabled ? 'true' : 'false');
        el.style.pointerEvents = disabled ? 'none' : 'auto';
        el.style.opacity = disabled ? '0.6' : '1';
      });
    ")),
    tags$script(HTML("
      document.addEventListener('shiny:recalculating', function(ev){
        var el = ev.target; if(!el) return;
        var card = el.closest('.gc-card'); if(card) card.classList.add('busy');
      });
      document.addEventListener('shiny:value', function(ev){
        var el = ev.target; if(!el) return;
        var card = el.closest('.gc-card'); if(card) card.classList.remove('busy');
      });
    ")),
    tags$link(rel = "stylesheet", type = "text/css", href = "gc_custom_style.css"),
    tags$style(HTML("
      .gc-card{overflow-x:auto;padding:.5rem 1rem;border:1px solid var(--gcds-border-default,#7D828B);border-radius:6px;background:#fff;margin-bottom:1rem;}
      .gc-card .dataTables_wrapper{overflow-x:auto;}
    ")),
    tags$script(src = "tz.js"),
    tags$script(src = "upload_labels.js"),
    tags$script(HTML("
      if(document.readyState === 'complete'){ initializeTZ(); } else { window.addEventListener('load', initializeTZ); }
      function initializeTZ(){
        try{
          var browserTZ = Intl.DateTimeFormat().resolvedOptions().timeZone;
          if(browserTZ && typeof Shiny !== 'undefined'){
            Shiny.setInputValue('tz_browser', browserTZ, {priority:'event'});
          }
        } catch(e){ console.log(e); }
      }
      Shiny.addCustomMessageHandler('tz_lookup', function(msg){
        var tz = tzlookup(msg.lat, msg.lon);
        Shiny.setInputValue('tz_lookup_result', tz, {priority:'event'});
      });
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function(){
        try { Shiny.setInputValue('__init__', Math.random(), {priority:'event'}); } catch(e){}
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('form-reset', function(id){
        try { document.getElementById(id)?.reset(); } catch(e){}
      });
    "))
  ),
  mod_i18n_ui("i18n"),
  tags$main(
    id = "main-content",
    role = "main",
    `aria-label` = "Hourly FWI application",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        mod_upload_ui("upload"),
        tags$form(
          id = "controls",
          mod_mapping_ui("mapping"),
          tags$hr(),
          mod_timezone_ui("tz"),
          mod_filter_ui("filter"),
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
                tags$div(
                  id = "pre-run-output-card",
                  class = "gc-card",
                  uiOutput("pre_run_output_msg")
                )
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
                tags$div(
                  id = "pre-run-plot",
                  class = "gc-card",
                  uiOutput("pre_run_plot_msg")
                )
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
                tags$div(
                  id = "pre-run-weather-card",
                  class = "gc-card",
                  uiOutput("pre_run_tables_msg")
                )
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
        tags$p(class = "gc-footer__team-text", "Developed by the Wildfire Intelligence & Predictive Services Team")
      ),
      tags$div(
        class = "gc-footer__sig",
        tags$img(src = "nrcan_logo.svg", alt = "Ressources naturelles Canada / Natural Resources Canada", class = "gc-footer__sig-logo"),
        tags$div(class = "gc-footer__legal", HTML("© Government of Canada / Gouvernement du Canada"))
      )
    )
  )
)
