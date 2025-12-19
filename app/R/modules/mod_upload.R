
# R/modules/mod_upload.R
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #
    # tags$head(
    #   tags$link(
    #     rel = "stylesheet",
    #     href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
    #   )
    # ),
    h5(id = ns("lbl_upload_csv"), uiOutput(ns("lbl_upload_csv"))),
    
    div(
      class = "upload-inline",
      role = "group",
      `aria-labelledby` = ns("lbl_upload_csv"),
      div(class = "no-i18n",
        fileInput(
          ns("csv"),
          label       = NULL,
          buttonLabel = shiny::icon("folder-open"),            # visible content comes from the icon
          placeholder = NULL,
          accept      = c(".csv", "text/csv")
        )
      ),
      div(
        class = "upload-checkbox",
        uiOutput(ns("lbl_has_header")),
        checkboxInput(ns("has_header"), label = NULL, value = TRUE, width = "auto")
      )
    )
  )
}


mod_upload_server <- function(id, tr, lang) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # --- UI labels & translations -------------------------------------------
    output$lbl_upload_csv <- shiny::renderUI({
      label_with_help_rich(
        label_text  = tr("upload_csv"),
        tip_text    = tr("csv_spec_sr"),
        popover_html = tr("csv_spec_html"),
        sr_label    = tr("upload_csv")
      )
    })
    output$lbl_has_header <- shiny::renderUI({
      tags$label(
        `for` = session$ns("has_header"),
        label_with_help(tr("csv_has_header"), tr("tt_has_header"))
      )
    })
    
    # Update fileInput labels (initial + when language changes)
    send_fileinput_labels <- function() {
      session$sendCustomMessage("updateFileInputLabels", list(
        id          = session$ns("csv"),
        buttonLabel = tr("csv_button_label"),
        placeholder = tr("csv_place_holder")
      ))
    }
    # Send once on module init
    observeEvent(TRUE, { send_fileinput_labels() }, once = TRUE)
    
    # And re-send whenever language changes
    observeEvent(lang(), { send_fileinput_labels() }, ignoreInit = FALSE)
    
    
    # --- Upload tracking -----------------------------------------------------
    upload_id <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$csv, {
      upload_id(isolate(upload_id()) + 1L)
    }, ignoreInit = TRUE)
    
    # Cache file path
    file_fp <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$csv, {
      if (!is.null(input$csv$datapath)) file_fp(input$csv$datapath)
    })
    
    # Helpers (unchanged)
    clean_blank_rows <- function(dt) {
      char_cols <- names(dt)[vapply(dt, is.character, TRUE)]
      if (length(char_cols)) {
        dt[, (char_cols) := lapply(.SD, function(x) {
          y <- trimws(x); y[y == ""] <- NA_character_; y
        }), .SDcols = char_cols]
      }
      is_blank_col <- lapply(dt, function(col) {
        if (is.character(col)) { is.na(col) | !nzchar(col)
        } else if (is.factor(col)) {
          tmp <- trimws(as.character(col)); is.na(col) | !nzchar(tmp)
        } else { is.na(col) }
      })
      fully_blank_rows <- Reduce(`&`, is_blank_col)
      if (any(fully_blank_rows)) dt <- dt[!fully_blank_rows]
      dt
    }
    read_csv_with_fallback <- function(path, has_header = TRUE) {
      dt <- tryCatch(
        data.table::fread(file = path, sep = ",",
                          na.strings = c("", "NA", "NaN", "null"),
                          header = isTRUE(has_header), blank.lines.skip = TRUE,
                          fill = TRUE, showProgress = FALSE
        ),
        error = function(e) {
          data.table::as.data.table(utils::read.csv(
            file = path, header = isTRUE(has_header),
            na.strings = c("", "NA", "NaN", "null"),
            check.names = FALSE, stringsAsFactors = FALSE,
            blank.lines.skip = TRUE, strip.white = TRUE
          ))
        }
      )
      clean_blank_rows(dt)
    }
    
    # Reactive: cleaned raw file
    raw_file <- shiny::reactive({
      shiny::req(file_fp())
      dt <- read_csv_with_fallback(file_fp(), has_header = isTRUE(input$has_header))
      message(sprintf("[Upload][INFO] Read %d rows, %d cols after cleaning.", nrow(dt), ncol(dt)))
      dt
    })
    shiny::observeEvent(raw_file(), {
      message(sprintf("[Upload][INFO] Columns: %s", paste(names(raw_file()), collapse = ", ")))
    }, ignoreInit = TRUE)
    
    return(list(
      raw_file  = raw_file,
      cols      = shiny::reactive(if (is.null(file_fp())) character(0) else names(raw_file())),
      csv_name  = shiny::reactive(if (is.null(input$csv)) "results" else input$csv$name),
      upload_id = upload_id
    ))
  })
}
