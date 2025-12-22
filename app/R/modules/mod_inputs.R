# R/modules/mod_inputs.R
# Inputs module: shows uploaded raw data and hourly data (converted or passthrough)
# Adds translations (via tr) and accessibility improvements.

mod_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "gc-card",
      # Translated label and choices
      radioButtons(
        inputId = ns("view_mode"),
        label = NULL, # We'll add aria-label for accessibility
        choices = c("raw", "hourly"), # actual values
        selected = "raw",
        inline = TRUE
      ),
      div(
        class = "gc-spin-wrap",
        DT::dataTableOutput(ns("inputs_table"), width = "100%", height = "100%", fill = TRUE),
        div(
          class = "gc-spin-overlay",
          div(class = "gc-spinner", `aria-hidden` = "true"),
          span(class = "sr-only", "Loading…")
        )
      )
    )
  )
}

mod_inputs_server <- function(id, tr, dt_i18n, raw_data, hourly_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update radio button labels dynamically for translations
    observe({
      updateRadioButtons(
        session,
        "view_mode",
        label = tr("inputs_view_label"), # e.g., "View" / "Vue"
        choices = setNames(c("raw", "hourly"), c(tr("inputs_view_raw"), tr("inputs_view_hourly"))),
        inline = T
      )
    })

    # Render table based on selected view mode
    output$inputs_table <- DT::renderDataTable(
      {
        cb <- DT::JS("
        var tbl = table;
        function adjust(){ try { tbl.columns.adjust(); } catch(e){} }
        setTimeout(adjust, 0);
        tbl.on('draw.dt', adjust);
        $(window).on('resize.dt', adjust);
      ")

        if (input$view_mode == "raw") {
          validate(need(!is.null(raw_data()), tr("err_no_raw")))
          data <- raw_data()
        } else {
          validate(need(!is.null(hourly_data()), tr("err_no_hourly")))
          data <- hourly_data()
        }

        DT::datatable(
          data,
          rownames = FALSE,
          fillContainer = TRUE,
          autoHideNavigation = TRUE,
          escape = TRUE,
          filter = "top",
          class = "display nowrap compact hover stripe gc-dt",
          extensions = c("Buttons"),
          options = list(
            language = dt_i18n(),
            autoWidth = FALSE,
            scrollX = FALSE,
            scrollY = 360,
            pageLength = 20,
            lengthMenu = list(c(10, 20, 50, 100, -1), c("10", "20", "50", "100", "All")),
            dom = "Blrtip",
            buttons = list(
              list(extend = "copy", text = tr("dt_btn_copy")),
              list(extend = "csv", text = tr("dt_btn_csv"), filename = "inputs"),
              list(extend = "excel", text = tr("dt_btn_excel"), filename = "inputs")
            ),
            initComplete = DT::JS("function(){ this.api().columns.adjust(); }")
          ),
          callback = cb
        )
      },
    )
  })
}
