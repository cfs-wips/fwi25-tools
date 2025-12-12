
# R/modules/mod_upload.R

mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(id = ns("lbl_upload_csv"), uiOutput(ns("lbl_upload_csv"))),
    tags$div(
      class = "upload-inline",
      role = "group",
      `aria-labelledby` = ns("lbl_upload_csv"),
      # Static file input (no renderUI)
      fileInput(
        ns("csv"),
        label = NULL,
        buttonLabel = NULL, # We'll override via translation
        placeholder = NULL,
        accept = c(".csv", "text/csv")
      ),
      tags$div(
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
        label_text = tr("upload_csv"),
        tip_text   = tr("csv_spec_sr"),
        popover_html = tr("csv_spec_html"),
        sr_label   = tr("upload_csv")
      )
    })
    
    # External label + tooltip for checkbox (keeps a11y + avoids [object Object])
    output$lbl_has_header <- shiny::renderUI({
      tags$label(
        `for` = session$ns("has_header"),
        label_with_help(tr("csv_has_header"), tr("tt_has_header"))
      )
    })
    
    # Update fileInput labels (button + placeholder) when language changes
    shiny::observe({
      session$sendCustomMessage("updateFileInputLabels", list(
        id = session$ns("csv"),
        buttonLabel = tr("csv_button_label"),
        placeholder = tr("csv_place_holder")
      ))
    }) |> bindEvent(lang())
    
    # --- Upload tracking -----------------------------------------------------
    upload_id <- shiny::reactiveVal(0L)
    shiny::observeEvent(input$csv, {
      upload_id(isolate(upload_id()) + 1L)
    }, ignoreInit = TRUE)
    
    # Cache file path
    file_fp <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$csv, {
      if (!is.null(input$csv$datapath)) {
        file_fp(input$csv$datapath)
      }
    })
    
    # --- Helpers: robust CSV read + "drop row only if all columns are blank" -
    clean_blank_rows <- function(dt) {
      # Normalize character cols: trim whitespace; turn "" -> NA
      char_cols <- names(dt)[vapply(dt, is.character, TRUE)]
      if (length(char_cols)) {
        dt[, (char_cols) := lapply(.SD, function(x) {
          y <- trimws(x)
          y[y == ""] <- NA_character_
          y
        }), .SDcols = char_cols]
      }
      
      # Per-column blank flags:
      # - character: NA or empty (after trim)
      # - factor   : as.character then same check
      # - other    : NA only
      is_blank_col <- lapply(dt, function(col) {
        if (is.character(col)) {
          is.na(col) | !nzchar(col)
        } else if (is.factor(col)) {
          tmp <- trimws(as.character(col))
          is.na(col) | !nzchar(tmp)
        } else {
          is.na(col)
        }
      })
      
      # A row is "fully blank" iff *all* columns are blank
      fully_blank_rows <- Reduce(`&`, is_blank_col)
      
      if (any(fully_blank_rows)) dt <- dt[!fully_blank_rows]
      dt
    }
    
    read_csv_with_fallback <- function(path, has_header = TRUE) {
      # Primary: data.table::fread (fast; handles BOM; fill short rows)
      dt <- tryCatch(
        data.table::fread(
          file = path,
          sep = ",",
          na.strings = c("", "NA", "NaN", "null"),
          header = isTRUE(has_header),
          blank.lines.skip = TRUE,  # skip pure blank lines
          fill = TRUE,              # pad short rows to full width with NA
          showProgress = FALSE
        ),
        error = function(e) {
          # Fallback: base read.csv with similar NA handling
          data.table::as.data.table(utils::read.csv(
            file = path,
            header = isTRUE(has_header),
            na.strings = c("", "NA", "NaN", "null"),
            check.names = FALSE,
            stringsAsFactors = FALSE,
            blank.lines.skip = TRUE,
            strip.white = TRUE
          ))
        }
      )
      
      # Clean: trim char cols, empty -> NA, drop rows only if all fields blank
      clean_blank_rows(dt)
    }
    
    # --- Reactive: cleaned raw file -----------------------------------------
    raw_file <- shiny::reactive({
      shiny::req(file_fp())
      # Read + clean
      dt <- read_csv_with_fallback(file_fp(), has_header = isTRUE(input$has_header))
      # Optional: emit a small log to console for visibility
      message(sprintf("[Upload][INFO] Read %d rows, %d cols after cleaning.", nrow(dt), ncol(dt)))
      dt
    })
    
    # --- Outputs -------------------------------------------------------------
    shiny::observeEvent(raw_file(), {
      # For visibility, print names and counts once per upload
      message(sprintf("[Upload][INFO] Columns: %s", paste(names(raw_file()), collapse = ", ")))
    }, ignoreInit = TRUE)
    
    return(list(
      raw_file = raw_file,
      cols     = shiny::reactive(if (is.null(file_fp())) character(0) else names(raw_file())),
      csv_name = shiny::reactive(if (is.null(input$csv)) "results" else input$csv$name),
      upload_id = upload_id
    ))
  })
}
