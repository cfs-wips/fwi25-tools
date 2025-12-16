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
            ns("tz_mode"),
            label = NULL,
            choices = c("fixed" = "fixed", "auto" = "auto"),
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
            ns("tz_offset_policy"),
            label = NULL,
            choices = c("std" = "std", "modal" = "modal"),
            selected = "std"
          )
        )
      )
    ),
    tags$div(role = "status", `aria-live` = "polite", verbatimTextOutput(ns("tz_out")))
  )
}


mod_timezone_server <- function(
    id,
    tr,
    manual_lat,            # reactive: mapping$manual_lat()
    manual_lon,            # reactive: mapping$manual_lon()
    browser_tz,            # reactive: reactive(input$tz_browser)
    lookup_result,         # reactive: reactive(input$tz_lookup_result)
    raw_file,
    reset = reactive(NULL),# optional reset token; bump to clear state
    log = TRUE
) {
  moduleServer(id, function(input, output, session) {
    
    # --- helpers ---
    say        <- function(...) { if (isTRUE(log)) message("[TZ] ", sprintf(...)) }
    is_nzchar  <- function(x) is.character(x) && length(x) > 0 && nzchar(x[1])
    fin_num    <- function(x) { x <- suppressWarnings(as.numeric(x)); is.finite(x) }
    
    # --- state ---
    tz_stable       <- reactiveVal(NULL)  # latched, "official" zone used by app
    tz_guess        <- reactiveVal(NULL)  # latest guess (browser or interim)
    tz_initialized  <- reactiveVal(FALSE) # one-shot guard (optional)
    last_tz_req_key <- reactiveVal(NULL)  # de-dup lookup requests
    
    # de-bounce lookup_result (avoid bursts)
    lookup_debounced <- debounce(lookup_result, millis = 300)
    
    # --- UI labels (unchanged) ---
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
        label   = NULL,
        choices = setNames(c("fixed","auto"), c(tr("tz_fixed_one"), tr("tz_auto_infer"))),
        selected = input$tz_mode %||% "auto",
        inline  = FALSE
      )
      updateSelectInput(session, "fixed_tz", label = tr("tz_select"))
      updateRadioButtons(
        session, "tz_offset_policy",
        label   = NULL,
        choices = setNames(c("std","modal"), c(tr("tz_offset_std"), tr("tz_offset_modal"))),
        selected = input$tz_offset_policy %||% "std",
        inline  = FALSE
      )
    })
    
    # --- request helper ---
    send_tz_lookup <- function(la, lo) {
      key <- sprintf("%.6f/%.6f", la, lo)
      if (identical(last_tz_req_key(), key)) return(invisible(NULL))
      last_tz_req_key(key)
      session$sendCustomMessage("tz_lookup", list(lat = la, lon = lo))
      say("Requested tz_lookup for lat=%.4f lon=%.4f", la, lo)
    }
    
    # --- RESET handling (optional) ---
    
    observeEvent(reset(), ignoreInit = TRUE, {
      say("Reset token received; clearing tz state and latching browser TZ")
      tz_guess(NULL); last_tz_req_key(NULL)  # clear non-critical state
      # Instead of wiping tz_stable to NULL, re-latch to browser TZ
      bt <- browser_tz()
      if (is_nzchar(bt)) {
        tz_stable(bt); tz_guess(bt)
        say("RESET: tz_stable <- %s", bt)
      } else {
        # leave existing tz_stable if no browser TZ available
        say("RESET: no browser TZ available; keeping current tz_stable")
      }
    })
    
    
    # --- AUTO mode: primary driver ---
    observeEvent(list(input$tz_mode, manual_lat(), manual_lon(), browser_tz()), ignoreInit = TRUE, {
      if (!identical(input$tz_mode, "auto")) return
      
      la <- manual_lat(); lo <- manual_lon()
      if (fin_num(la) && fin_num(lo)) {
        # Lat/lon present → request a geo lookup (deduped)
        send_tz_lookup(as.numeric(la), as.numeric(lo))
        
      } else {
        # Lat/lon blank → LATCH BROWSER TZ as the stable zone so app is ready
        bt <- browser_tz()
        if (is_nzchar(bt) && !identical(tz_stable(), bt)) {
          tz_stable(bt); tz_guess(bt)
          say("AUTO: lat/lon blank; latched browser TZ %s", bt)
        }
      }
    })
    
    # --- FIXED mode: reflect selected Olson name ---
    observeEvent(input$fixed_tz, ignoreInit = TRUE, {
      if (identical(input$tz_mode, "fixed") && is_nzchar(input$fixed_tz)) {
        if (!identical(tz_stable(), input$fixed_tz)) {
          tz_stable(input$fixed_tz); tz_guess(input$fixed_tz)
          say("FIXED: tz_stable <- %s", input$fixed_tz)
        }
      }
    })
    
    
    # R/modules/mod_timezone.R — add this observer near your other observers
    observeEvent(list(raw_file(), browser_tz()), ignoreInit = TRUE, {
      if (!identical(input$tz_mode, "auto")) return
      if (!is_nzchar(tz_stable())) {
        bt <- browser_tz()
        if (is_nzchar(bt)) {
          tz_stable(bt); tz_guess(bt)
          say("AUTO: post-upload fallback; latched browser TZ %s", bt)
        } else {
          say("AUTO: post-upload fallback has no browser TZ; still not inferred.")
        }
      }
    })
    
    
    # --- Lookup adoption & post-upload fallback ---
    observeEvent(lookup_debounced(), ignoreInit = TRUE, {
      val <- lookup_debounced()
      if (is.character(val) && nzchar(val)) {
        tz_stable(val); tz_guess(val)
        say("Lookup adopted: tz_stable <- %s", val)
      } else {
        # Fallback only after a file exists: latch browser TZ
        df <- raw_file()
        if (!is.null(df) && nrow(df) > 0) {
          bt <- browser_tz()
          if (is.character(bt) && nzchar(bt)) {
            tz_stable(bt); tz_guess(bt)
            say("Lookup blank; post-upload fallback: tz_stable <- %s", bt)
          } else {
            say("Lookup blank and no browser TZ; still not inferred.")
          }
        }
      }
    })
    
    # --- Expose tz_use + readiness ---
    tz_use <- reactive({
      # Fixed → explicit or UTC fallback
      if (identical(input$tz_mode, "fixed")) {
        return(if (nzchar(input$fixed_tz %||% "")) input$fixed_tz else "UTC")
      }
      # Auto → prefer latched stable zone
      val <- tz_stable()
      if (is.character(val) && nzchar(val)) return(val)
      NULL
    })
    
    tz_ready <- reactive({
      if (identical(input$tz_mode, "fixed")) {
        return(is.character(tz_use()) && nzchar(tz_use()))
      }
      # Auto: READY only when we have a latched stable zone
      is.character(tz_stable()) && nzchar(tz_stable())
    })
    
    # Logs (optional)
    observeEvent(input$tz_lookup_result, ignoreInit = TRUE, {
      say("lookup_result input -> %s", input$tz_lookup_result %||% "")
    })
    observeEvent(tz_stable(), ignoreInit = TRUE, {
      say("tz_stable changed -> %s", tz_stable())
    })
    
    # Status line
    output$tz_out <- renderPrint({
      tz <- tz_use()
      if (is.null(tz)) tr("tz_not_inferred") else paste(tr("iana_prefix"), tz)
    })
    
    # Return API
    list(
      tz_mode          = reactive(input$tz_mode),
      fixed_tz         = reactive(input$fixed_tz),
      tz_use           = tz_use,
      tz_offset_policy = reactive(input$tz_offset_policy),
      tz_guess         = tz_guess,
      tz_ready         = tz_ready
    )
  })
}

  

