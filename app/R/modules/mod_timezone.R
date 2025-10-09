# R/modules/mod_timezone.R

# R/modules/mod_timezone.R  (UI only)

mod_timezone_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Two columns side-by-side (flex)
    tags$div(
      class = "tz-grid",
      
      # --- Column 1: TZ mode + (conditional) Fixed TZ select ---
      tags$div(
        class = "tz-col",
        tags$fieldset(
          tags$legend(id = ns("legend_tz_mode"), uiOutput(ns("lbl_time_zone"))),
          # Keep choices vertical (inline = FALSE or omit)
          radioButtons(
            ns("tz_mode"), label = NULL,
            choices  = c("fixed" = "fixed", "auto" = "auto"),
            selected = "auto"
          )
        ),
        conditionalPanel(
          sprintf("input['%s'] == 'fixed'", ns("tz_mode")),
          selectInput(ns("fixed_tz"), label = NULL, choices = OlsonNames(), selected = "UTC")
        )
      ),
      
      # --- Column 2: Offset policy ---
      tags$div(
        class = "tz-col",
        tags$fieldset(
          tags$legend(id = ns("legend_tz_offset"), uiOutput(ns("lbl_tz_offset_policy"))),
          radioButtons(
            ns("tz_offset_policy"), label = NULL,
            choices  = c("std" = "std", "modal" = "modal"),
            selected = "std"
          )
        )
      )
    ),
    
    tags$div(role = "status", `aria-live` = "polite", verbatimTextOutput(ns("tz_out")))
  )
}

mod_timezone_server <- function(id, tr, manual_lat, manual_lon, browser_tz, lookup_result) {
  moduleServer(id, function(input, output, session) {
    
    tz_guess <- reactiveVal(NULL)
    

    output$lbl_time_zone <- renderUI({
      label_with_help_rich(
        label_text   = tr("time_zone"),
        tip_text     = tr("tt_time_zone"),
        popover_html = tr("pop_time_zone_html"),
        sr_label     = tr("time_zone")
      )
    })
    
    output$lbl_tz_offset_policy <- renderUI({
      label_with_help_rich(
        label_text   = tr("tz_offset_policy"),
        tip_text     = tr("tt_tz_offset_policy"),
        popover_html = tr("pop_tz_offset_html"),
        sr_label     = tr("tz_offset_policy")
      )
    })
    

    observe({
      updateRadioButtons(
        session, "tz_mode",
        label = NULL,
        choices = setNames(
          c("fixed", "auto"),
          c(tr("tz_fixed_one"), tr("tz_auto_infer"))
        ),
        selected = input$tz_mode %||% "auto",
        inline = F
      )
      
      updateSelectInput(session, "fixed_tz", label = tr("tz_select"))
      
      updateRadioButtons(
        session, "tz_offset_policy",
        label = NULL,
        choices = setNames(
          c("std", "modal"),
          c(tr("tz_offset_std"), tr("tz_offset_modal"))
        ),
        selected = input$tz_offset_policy %||% "std",
        inline = F
      )
    })
    
    # Accept lookup results (JS or server) for the inferred zone
    observeEvent(lookup_result(), ignoreInit = TRUE, {
      val <- lookup_result()
      if (is.character(val) && nzchar(val)) tz_guess(val)
    })
    
    # Live status line
    output$tz_out <- renderPrint({
      tz <- tz_use()
      if (is.null(tz)) tr("tz_not_inferred") else paste(tr("iana_prefix"), tz)
    })
    
    # Trigger JS tz lookup when manual coords change (auto mode)
    observeEvent(list(input$tz_mode, manual_lat(), manual_lon()), ignoreInit = TRUE, {
      if (identical(input$tz_mode, "auto")) {
        la <- suppressWarnings(as.numeric(manual_lat()))
        lo <- suppressWarnings(as.numeric(manual_lon()))
        if (is.finite(la) && is.finite(lo)) {
          session$sendCustomMessage("tz_lookup", list(lat = la, lon = lo))
        } else {
          bt <- browser_tz()
          if (is.character(bt) && nzchar(bt)) tz_guess(bt)
        }
      }
    })
    
    # Expose selected tz
    tz_use <- reactive({
      if (identical(input$tz_mode, "fixed")) {
        input$fixed_tz
      } else {
        if (!is.null(tz_guess())) {
          tz_guess()
        } else {
          bt <- browser_tz()
          if (is.character(bt) && nzchar(bt)) bt else NULL
        }
      }
    })
    
    return(list(
      tz_mode          = reactive(input$tz_mode),
      fixed_tz         = reactive(input$fixed_tz),
      tz_use           = tz_use,
      tz_offset_policy = reactive(input$tz_offset_policy),
      tz_guess         = tz_guess
    ))
  })
}
