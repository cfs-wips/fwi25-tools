# R/modules/mod_i18n.R
# Centralized translations loaded from external JSON + modal body from HTML files.

get_i18n <- function(key, lang = "en") {
  if (!lang %in% names(i18n_labels)) lang <- "en"
  i18n_labels[[lang]][[key]]
}

# translate with simple {placeholder} replacement
tr_i18n <- function(key, lang = "en", ...) {
  if (!lang %in% names(i18n_labels)) lang <- "en"
  # Special handling: if modal_body_html is external, load HTML from file
  if (identical(key, "modal_body_html")) {
    val <- i18n_labels[[lang]][[key]]
    if (is.character(val) && identical(val, "__EXTERNAL_HTML__")) {
      return(.read_help_html(lang))
    }
  }

  val <- i18n_labels[[lang]][[key]]
  if (is.null(val)) {
    return(paste0("??", key, "??"))
  }
  if (nargs() > 2) {
    dots <- list(...)
    if (is.character(val)) {
      for (nm in names(dots)) {
        val <- sub(paste0("{", nm, "}"), as.character(dots[[nm]]), val, fixed = TRUE)
      }
    }
  }
  val
}

# Simple formatter for strings with {placeholders}
trf <- function(val, ...) {
  dots <- list(...)
  if (is.character(val)) {
    for (nm in names(dots)) {
      val <- sub(paste0("{", nm, "}"), as.character(dots[[nm]]), val, fixed = TRUE)
    }
  }
  val
}

# Small UI helpers reused in modules
help_icon <- function(text) {
  tags$span(class = "help-icon", title = text, `aria-label` = text, tabindex = "0", "?")
}
help_disclosure <- function(popover_html, sr_label = NULL) {
  lab <- if (!is.null(sr_label) && nzchar(sr_label)) sr_label else "More info"
  tags$details(
    class = "help-disclosure",
    tags$summary("?", `aria-label` = lab),
    tags$div(class = "help-panel", HTML(popover_html))
  )
}
label_with_help_rich <- function(label_text, tip_text, popover_html, sr_label = NULL) {
  tagList(tags$span(label_text), help_disclosure(popover_html, sr_label = sr_label))
}
label_with_help <- function(label_text, tip_text) {
  tagList(label_text, HTML(" "), help_icon(tip_text))
}

# --- Module: i18n (header + language state) ---
mod_i18n_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$a(href = "#main-content", class = "skip-link", textOutput(ns("skip_link_txt"), inline = TRUE)),
    tags$header(
      role = "banner",
      style = "background:#fff;border-bottom:1px solid var(--gcds-border-default,#7D828B);",
      tags$div(
        class = "gc-header",
        tags$a(
          class = "gc-header__brand",
          href = "https://www.canada.ca/en.html",
          `aria-label` = "Government of Canada home",
          tags$img(src = "goc_logo.svg", alt = "Government of Canada / Gouvernement du Canada", height = "28", style = "display:block")
        ),
        textOutput(ns("app_title"), container = function(...) h1(class = "gc-page-title gc-header__title", role = "heading", `aria-level` = "1", ...)),
        tags$nav(class = "gc-header__lang", `aria-label` = "Language", uiOutput(ns("lang_toggle")))
      )
    )
  )
}

mod_i18n_server <- function(id, session_title = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lang <- reactiveVal("en")

    observe({
      qs <- parseQueryString(session$clientData$url_search)
      l <- tolower(if (!is.null(qs[["lang"]])) qs[["lang"]] else "en")
      if (l %in% c("en", "fr")) lang(l) else lang("en")
    })

    tr <- function(id, ...) tr_i18n(id, lang(), ...)

    aliases_active <- function(type = c("short", "long")) {
      type <- match.arg(type)
      L <- lang()
      key <- if (type == "short") "aliases_short" else "aliases_long"
      out <- get_i18n(key, L)
      if (is.null(out)) character(0) else out
    }
    label_for_col <- function(nm, type = c("short", "long")) {
      type <- match.arg(type)
      ali <- aliases_active(type)
      key <- tolower(nm)
      if (length(ali) && key %in% names(ali)) ali[[key]] else nm
    }
    labelize_cols <- function(cols, type = c("short", "long")) {
      type <- match.arg(type)
      stats::setNames(cols, vapply(cols, label_for_col, character(1), type = type))
    }

    # Language toggle
    output$lang_toggle <- renderUI({
      cur <- lang()
      actionLink(ns("toggle_lang"),
        label = if (cur == "fr") "English" else "Français",
        class = "link-unstyled",
        `aria-label` = if (cur == "fr") "Switch to English" else "Passer en français"
      )
    })
    observeEvent(input$toggle_lang, {
      lang(if (lang() == "fr") "en" else "fr")
      updateQueryString(paste0("?lang=", lang()), mode = "push")
    })

    # Static labels
    output$app_title <- renderText(tr("title"))
    output$skip_link_txt <- renderText(tr("skip_to_main"))

    # ARIA + title sync
    observeEvent(lang(),
      {
        session$sendCustomMessage("set-aria-labels", list(
          app = tr("aria_app_label"),
          tabs = tr("aria_tabs_label"),
          run_label = tr("aria_run_label")
        ))
        if (isTRUE(session_title)) session$sendCustomMessage("set-title", tr("title"))
      },
      ignoreInit = FALSE
    )

    # DataTables i18n payload
    dt_i18n <- reactive({
      list(
        sSearch = tr("dt_sSearch"), sLengthMenu = tr("dt_sLength"), sInfo = tr("dt_sInfo"),
        sInfoEmpty = tr("dt_sInfoEmpty"), sInfoFiltered = tr("dt_sInfoFilt"), sZeroRecords = tr("dt_sZero"),
        sProcessing = tr("dt_sProc"),
        oPaginate = list(sFirst = tr("dt_pag_first"), sPrevious = tr("dt_pag_prev"), sNext = tr("dt_pag_next"), sLast = tr("dt_pag_last"))
      )
    })

    # First-load modal (same keys as the Help button)
    observeEvent(TRUE,
      {
        showModal(
          modalDialog(
            title = tr("modal_title"),
            HTML(tr("modal_body_html")), # loads external HTML transparently
            easyClose = FALSE,
            footer = modalButton(tr("modal_close")),
            size = "l" # actual width/height controlled by CSS in the HTML
          )
        )
      },
      once = TRUE
    )

    return(list(
      lang = lang,
      tr = tr,
      aliases_active = aliases_active,
      label_for_col = label_for_col,
      labelize_cols = labelize_cols,
      dt_i18n = dt_i18n
    ))
  })
}
