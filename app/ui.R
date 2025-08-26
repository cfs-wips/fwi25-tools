# --- Shinylive/Chromium download workaround (Shinylive only) ---
# See: https://shiny.posit.co/r/components/inputs/download-link/ (workaround block)
library(DT)
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

ui <- fluidPage(
  tags$head(
    tags$script(src = "tz.js"), 
    tags$script(HTML(
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
    ))
  ),
  titlePanel("Hourly FWI (NG-CFFDRS) – upload weather and run hFWI()"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "csv",
        "Upload weather CSV",
        accept = c(".csv"),
        buttonLabel = "Browse…"
      ),
      checkboxInput("has_header", "CSV has header", TRUE),
      hr(),
      h4("Column mapping"),
      uiOutput("mapping_ui"),
      hr(),
      h4("Time zone"),
      radioButtons(
        "tz_mode",
        NULL,
        choices = c(
          "Specify one time zone for all rows" = "fixed",
          "Infer a single time zone from latitude/longitude" = "auto"
        ),
        selected = "auto"
      ),
      conditionalPanel(
        "input.tz_mode === 'fixed'",
        selectInput(
          "fixed_tz",
          "Time zone",
          choices = OlsonNames(),
          selected = "America/Edmonton"
        )
      ),
      radioButtons(
        "tz_offset_policy",
        "Offset for solar calculations",
        choices = c(
          "Standard (ignore DST)" = "std",
          "From data (modal; may include DST)" = "modal"
        ),
        selected = "std"
      ),
      hr(),
      h4("Filter"),
      dateInput(
        "start_date",
        "Drop rows prior to (local date):",
        value = NULL,
        format = "yyyy-mm-dd"
      ),
      helpText("If set, rows with local date < this value are removed before hFWI()."),
      numericInput("ffmc0", "Initial FFMC", value = 85, min = 0),
      numericInput("dmc0", "Initial DMC", value = 6, min = 0),
      numericInput("dc0", "Initial DC", value = 15, min = 0),
      hr(),
      actionButton("run", "Run hFWI()", class = "btn-primary"),
      br(),
      br(),
      downloadButton_sl("dl", "Download results (CSV)")
    ),
    mainPanel(tabsetPanel(id = "main_tabs",
      tabPanel("Output", DTOutput("tbl")),
      tabPanel(
        "Plot",
        fluidRow(
          column(
            4,
            selectInput(
              "plot_dataset",
              "Data source",
              choices = c("Results (hFWI output)" = "results",
                          "Inputs (weather)"     = "inputs"),
              selected = "results"
            )
          ),
          column(
            8,
            selectizeInput(
              "plot_y_multi",
              "Variables to plot (facetted)",
              choices = NULL,        # populated after you click Run
              multiple = TRUE,
              options = list(
                plugins = list("remove_button"),
                placeholder = "Select one or more numeric columns…"
              )
            )
          )
        ),
        fluidRow(
          column(
            4,
            sliderInput(
              "facet_ncol",
              "Facets per row",
              min = 1, max = 4, value = 1, step = 1
            )
          ),
          column(
            4,
            checkboxInput(
              "facet_free_y",
              "Free y-scale per facet",
              value = TRUE
            )
          )
        ),
        plotOutput("plot_ts", height="90vh")
      ),
      
      tabPanel("Log", verbatimTextOutput("log"))
    ))
  )
)