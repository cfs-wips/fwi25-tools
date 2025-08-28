
# --- Shinylive/Chromium download workaround (Shinylive only) ---
# See: https://shiny.posit.co/r/components/inputs/download-link/

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

ui <- fluidPage(title = NULL,
  # --- HEAD ---
  tags$head(
    
    # Static initial title for very first paint (will be overwritten by JS immediately)
    tags$title("FWI2025"),
    
    # JS handler that updates the browser tab title on language change or whenever you call it
    tags$script(HTML("
  Shiny.addCustomMessageHandler('set-title', function(msg){
    try { document.title = msg; } catch(e) {}
  });
")),
    
    
    # GCDS fonts (Lato for headings, Noto Sans for body) - pinned
    tags$link(rel = "stylesheet",
              href = "https://cdn.jsdelivr.net/npm/@cdssnc/gcds-fonts@1.0.3/dist/gcds-fonts.css"),
    # GCDS tokens (CSS variables for colours/typography/spacing) - pinned
    tags$link(rel = "stylesheet",
              href = "https://cdn.design-system.alpha.canada.ca/cdn/gcds/alpha/0.13.0/tokens.css"),
    # Your custom CSS (uses the tokens above)
    tags$link(rel = "stylesheet", type = "text/css", href = "gc_custom_style.css"),

    # Timezone assets and init (unchanged)
    tags$script(src = "tz.js"),
    tags$script(
      HTML(
        "
 if (document.readyState === 'complete') {
   initializeTZ();
 } else {
   window.addEventListener('load', initializeTZ);
 }
 function initializeTZ() {
   try {
     var browserTZ = Intl.DateTimeFormat().resolvedOptions().timeZone;
     if (browserTZ && typeof Shiny !== 'undefined') {
       Shiny.setInputValue('tz_browser', browserTZ, {priority:'event'});
     }
   } catch(e){console.log(e);}
 }
 Shiny.addCustomMessageHandler('tz_lookup', function(msg) {
   var tz = tzlookup(msg.lat, msg.lon);
   Shiny.setInputValue('tz_lookup_result', tz, {priority: 'event'});
 });
        "
      )
    ),

    # Accessibility helpers: set <html lang> and dynamic aria-labels
    tags$script(HTML("
  Shiny.addCustomMessageHandler('set-lang', function(msg){
    try { document.documentElement.setAttribute('lang', msg.lang); } catch(e){}
  });
  Shiny.addCustomMessageHandler('set-aria-labels', function(msg){
    try {
      var main = document.getElementById('main-content');
      if (main && msg.app) main.setAttribute('aria-label', msg.app);
      var tabs = document.getElementById('tabs-region');
      if (tabs && msg.tabs) tabs.setAttribute('aria-label', msg.tabs);
      var run = document.getElementById('run');
      if (run && msg.run_label) run.setAttribute('aria-label', msg.run_label);
    } catch(e){}
  });
    "))
  ),

  # Skip link for keyboard users (translated)
  tags$a(href = "#main-content", class = "skip-link", textOutput("skip_link_txt", inline = TRUE)),

  # --- HEADER (banner) ---
  tags$header(
    role = "banner",
    style = "background:#fff;border-bottom:1px solid var(--gcds-border-default,#7D828B);",
    tags$div(
      class = "gc-header",
      # Left: brand
      tags$a(
        class = "gc-header__brand",
        href = "https://www.canada.ca/en.html",
        `aria-label` = "Government of Canada home",
        tags$img(
          src = "goc_logo.svg",
          alt = "Government of Canada / Gouvernement du Canada",
          height = "28", style = "display:block"
        )
      ),
      # Center: title
      textOutput(
        "app_title",
        container = function(...) h1(class = "gc-page-title gc-header__title",
                                     role = "heading", `aria-level` = "1", ...)
      ),
      # Right: language toggle
      tags$nav(class = "gc-header__lang", `aria-label` = "Language",
               uiOutput("lang_toggle"))
    )
  ),

  # --- MAIN CONTENT ---
  tags$main(
    id = "main-content", role = "main", `aria-label` = "Hourly FWI application",

    sidebarLayout(
      # ===== Sidebar =====
      sidebarPanel(width=3,
        # Upload
        h4(id = "lbl_upload_csv", textOutput("lbl_upload_csv")),
        tags$div(role = "group", `aria-labelledby` = "lbl_upload_csv",
                 uiOutput("csv_input_ui")),
        checkboxInput("has_header", label = "", value = TRUE),

        # Column mapping
        h4(id = "lbl_column_mapping", textOutput("lbl_column_mapping")),
        tags$div(
          role = "group", `aria-labelledby` = "lbl_column_mapping",
          # the class below lets CSS reserve height & provide skeleton look
          tags$div(class = "mapping-block", uiOutput("mapping_ui"))
        ),
        tags$hr(),

        # Time zone (fieldset/legend for better semantics)
        tags$fieldset(
          tags$legend(id = "legend_tz_mode", textOutput("lbl_time_zone")),
          radioButtons(
            "tz_mode",
            label = NULL,
            choices = c("fixed" = "fixed", "auto" = "auto"),
            selected = "auto"
          )
        ),
        conditionalPanel(
          condition = "input.tz_mode == 'fixed'",
          selectInput(
            "fixed_tz",
            label = NULL,
            choices = OlsonNames(),
            selected = "UTC"
          )
        ),

        tags$fieldset(
          tags$legend(id = "legend_tz_offset", textOutput("lbl_tz_offset_policy")),
          radioButtons(
            "tz_offset_policy",
            label = NULL,
            choices = c("std" = "std", "modal" = "modal"),
            selected = "std"
          )
        ),
        # TZ status; announce politely
        tags$div(role = "status", `aria-live` = "polite",
                 verbatimTextOutput("tz_out")),

        # Filter
        h4(id = "lbl_filter", textOutput("lbl_filter")),
        # associate help text to the date input
        div(`aria-describedby` = "help_drop_rows",
            dateInput("start_date", label = NULL, value = NA)
        ),
        tags$p(id = "help_drop_rows", class = "help-block",
               textOutput("txt_drop_rows_help")),


        # Initial codes & Run/Download
        fluidRow(
          column(4,numericInput(
            "ffmc0",
            label = NULL,
            value = 85,
            min = 0,
            max = 101,
            step = 1
          )),
          column(4,numericInput(
            "dmc0",
            label = NULL,
            value = 6,
            min = 0,
            step = 1
          )),
          column(4,numericInput(
            "dc0",
            label = NULL,
            value = 15,
            min = 0,
            step = 1
          ))
        ),
        checkboxInput("calc_fwi87", label = "", value = TRUE),
        fluidRow(
          column(6,uiOutput("run")),
          column(6,uiOutput("dl_ui"))
        ) # translated download button
      ),

      # ===== Main panel =====
      mainPanel(
        width = 9,
        # Wrap the tabset to provide an accessible label without naming args on tabsetPanel()
        tags$div(id = "tabs-region", role = "region", `aria-label` = "Primary output tabs",
          tabsetPanel(
            id = "main_tabs",
            tabPanel(
              title = textOutput("tab_output_title"),
              value = "Output",
              tags$div(
                role = 'region', `aria-label` = "FWI25 results table",
                # label appears only when the table is ready
                conditionalPanel("output.has_tbl", h4(textOutput("lbl_fwi25_title"))),
                # spinner is visible during computation
                shinycssloaders::withSpinner(
                  DT::DTOutput("tbl", width = "100%",height="40vh")
                )
              ),
              tags$hr(),
              tags$div(
                role = "region", `aria-label` = "FWI87 results table",
                # label appears only when checkbox is TRUE and table is ready
                conditionalPanel("input.calc_fwi87 && output.has_tbl87", h4(textOutput("lbl_fwi87_title"))),
                # show spinner/table only if checkbox is TRUE
                conditionalPanel(
                  "input.calc_fwi87",
                  shinycssloaders::withSpinner(
                    DT::DTOutput("tbl_fwi87", width = "100%",height="40vh")
                  )
                )
              )
            ),
            tabPanel(
              title = textOutput("tab_plot_title"),
              value = "Plot",
              fluidRow(
                column(
                  4,
                  selectInput(
                    "plot_dataset",
                    label = NULL,
                    choices = c("results" = "results", "inputs" = "inputs"),
                    selected = "results"
                  )
                ),
                column(
                  4,
                  selectizeInput(
                    "plot_y_multi",
                    label = NULL,
                    choices = NULL,
                    multiple = TRUE,
                    options = list(placeholder = "")
                  )
                ),
                column(
                  2,
                  numericInput(
                    "facet_ncol",
                    label = NULL,
                    value = 2,
                    min = 1,
                    max=4,
                    step = 1
                  )
                ),
                column(2, checkboxInput(
                  "facet_free_y", label = NULL, value = FALSE
                ))
              ),
              plotly::plotlyOutput("plot_ts", height = "80vh")
            ),
            tabPanel(
              title = textOutput("tab_log_title"),
              value = "Log",
              # Treat as a status region so updates are announced
              tags$div(role = "region", `aria-label` = "Run log",
                       verbatimTextOutput("log", placeholder = TRUE))
            )
          )
        )
      )
    )
  ),

  # --- FOOTER (contentinfo) ---
  tags$footer(class = "gc-footer", role = "contentinfo",
              `aria-label` = "Application footer",
    tags$div(class = "gc-footer__wrap",

      # Left: Team logo + attribution
      tags$div(class = "gc-footer__team",
        tags$img(src = "logo.png",
                 alt = "Wildfire Intelligence & Predictive Services",
                 class = "gc-footer__team-logo"),
        tags$p(class = "gc-footer__team-text",
               "Developed by the Wildfire Intelligence & Predictive Services Team")
      ),

      # Right: NRCan signature with legal under it
      tags$div(class = "gc-footer__sig",
        tags$img(src = "nrcan_logo.svg",
                 alt = "Ressources naturelles Canada / Natural Resources Canada",
                 class = "gc-footer__sig-logo"),
        tags$div(class = "gc-footer__legal",
                 HTML("© Government of Canada / Gouvernement du Canada"))
      )
    )
  )
)
