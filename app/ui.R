# ui.R (modularized)
# NOTE: Head tags and assets largely preserved from your original ui.R

# Shinylive/Chromium download workaround wrappers (also provided in mod_utils)
downloadButton_sl <- function(...) { tag <- shiny::downloadButton(...); tag$attribs$download <- NULL; tag }
downloadLink_sl   <- function(...) { tag <- shiny::downloadLink(...); tag$attribs$download <- NULL; tag }
for (f in list.files("R/modules", pattern = "\\.R$", full.names = TRUE)) source(f, local = TRUE)
ui <- fluidPage(
  title = NULL,
  tags$head(
    tags$title("FWI2025"),
    # JS handlers: update browser tab title; sync & key aria labels
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('set-title', function(msg){try{document.title = msg; if(window.parent && window.parent !== window){window.parent.postMessage({type:'set-title', title: msg}, '*');}}catch(e){}});"
    )),
    tags$script(
      HTML(
              "
        Shiny.addCustomMessageHandler('set-aria-labels', function(msg){
          try{
            if (msg && msg.app)  document.getElementById('main-content')?.setAttribute('aria-label', msg.app);
            if (msg && msg.tabs) document.getElementById('tabs-region')?.setAttribute('aria-label', msg.tabs);
          } catch(e){}
        });
      "
    )),
    # GCDS fonts & tokens
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/@cdssnc/gcds-fonts@1.0.3/dist/gcds-fonts.css"),
    tags$link(rel = "stylesheet", href = "https://cdn.design-system.alpha.canada.ca/cdn/gcds/alpha/0.13.0/tokens.css"),
    # Custom CSS
    tags$link(rel = "stylesheet", type = "text/css", href = "gc_custom_style.css"),
    tags$style(HTML("\n .gc-card{overflow-x:auto;padding:.5rem 1rem;border:1px solid var(--gcds-border-default,#7D828B);border-radius:6px;background:#fff;margin-bottom:1rem;}\n .gc-card .dataTables_wrapper{overflow-x:auto;}\n ")),
    # Timezone assets + init
    tags$script(src = "tz.js"),
    tags$script(HTML(
      "\nif (document.readyState === 'complete'){ initializeTZ(); } else { window.addEventListener('load', initializeTZ); }\nfunction initializeTZ(){ try{ var browserTZ = Intl.DateTimeFormat().resolvedOptions().timeZone; if (browserTZ && typeof Shiny !== 'undefined'){ Shiny.setInputValue('tz_browser', browserTZ, {priority:'event'}); } } catch(e){ console.log(e); } }\nShiny.addCustomMessageHandler('tz_lookup', function(msg){ var tz = tzlookup(msg.lat, msg.lon); Shiny.setInputValue('tz_lookup_result', tz, {priority:'event'}); });\n"
    ))
  ),

  # --- Header & language (module) ---
  mod_i18n_ui("i18n"),

  # --- Main ---
  tags$main(
    id = "main-content", role = "main", `aria-label` = "Hourly FWI application",
    sidebarLayout(
      sidebarPanel(width = 3,
        # Upload
        mod_upload_ui("upload"),
        # Column mapping
        mod_mapping_ui("mapping"),
        tags$hr(),
        # Time zone
        mod_timezone_ui("tz"),
        # Filter
        mod_filter_ui("filter"),
        # Initial codes & actions
        mod_init_ui("init"),
        mod_actions_ui("actions")
      ),
      mainPanel(width = 9,
        tags$div(id = "tabs-region", role = "region", `aria-label` = "Primary output tabs",
          tabsetPanel(id = "main_tabs",
            tabPanel(title = textOutput("tab_output_title"), value = "Output",
              mod_results_table_ui("results_table"),
              # FWI87 table (conditional shown in server if calc_fwi87)
              conditionalPanel("true", mod_fwi87_table_ui("fwi87_table"))
            ),
            tabPanel(title = textOutput("tab_plot_title"), value = "Plot",
              mod_plot_ui("plot")
            ),
            tabPanel(title = textOutput("tab_log_title"), value = "Log",
              mod_log_ui("log")
            )
          )
        )
      )
    )
  ),

  # Footer (unchanged)
  tags$footer(class = "gc-footer", role = "contentinfo", `aria-label` = "Application footer",
    tags$div(class = "gc-footer__wrap",
      tags$div(class = "gc-footer__team",
        tags$img(src = "logo.png", alt = "Wildfire Intelligence & Predictive Services", class = "gc-footer__team-logo"),
        tags$p(class = "gc-footer__team-text", "Developed by the Wildfire Intelligence & Predictive Services Team")
      ),
      tags$div(class = "gc-footer__sig",
        tags$img(src = "nrcan_logo.svg", alt = "Ressources naturelles Canada / Natural Resources Canada", class = "gc-footer__sig-logo"),
        tags$div(class = "gc-footer__legal", HTML("© Government of Canada / Gouvernement du Canada"))
      )
    )
  )
)
