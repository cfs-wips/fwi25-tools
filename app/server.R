
# server.R
library(shiny)

# ---- Load NG-CFFDRS code vendored at build time ----------------------------
source("ng/util.r", local = TRUE)
source("ng/make_inputs.r", local = TRUE)
source("ng/NG_FWI.r", local = TRUE)

# ---- Helpers ----------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

## Shinylive download workaround for Chromium browsers----
downloadButton_sl <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

## Simple GoC theme for ggplot (GCDS colours/fonts) ----
theme_goc <- function(base_size = 12, base_family = "Noto Sans") {
  primary <- "#26374A"  # GCDS primary background
  border  <- "#7D828B"  # GCDS border default
  textcol <- "#333333"  # GCDS text primary
  stripbg <- "#F1F2F3"  # GCDS light background
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", colour = textcol),
      plot.subtitle= ggplot2::element_text(colour = textcol),
      plot.caption = ggplot2::element_text(colour = textcol),
      axis.title   = ggplot2::element_text(face = "bold", colour = textcol),
      axis.text    = ggplot2::element_text(colour = textcol),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = border, linewidth = 0.25),
      strip.text   = ggplot2::element_text(face = "bold", colour = textcol, family = "Lato"),
      strip.background = ggplot2::element_rect(fill = stripbg, colour = border, linewidth = 0.5)
    )
}

## Label helpers / i18n -----
labels <- list(
  en = list(
    title = "Hourly FWI (NG‑CFFDRS)",
    upload_csv = "Upload weather CSV",
    csv_has_header = "CSV has header",
    csv_button_label = "Browse...",
    csv_place_holder = "No file selected",
    lat_label = "Latitude (°)",
    lon_label = "Longitude (°)",
    column_mapping = "Column mapping",
    time_zone = "Time zone",
    tz_fixed_one = "Specify one time zone for all rows",
    tz_auto_infer = "Infer a single time zone from latitude/longitude",
    tz_select = "Time zone",
    tz_offset_policy = "Offset for solar calculations",
    tz_offset_std = "Standard (ignore DST)",
    tz_offset_modal = "From data (modal; may include DST)",
    filter = "Filter",
    drop_rows_prior = "Drop rows prior to (local date):",
    drop_rows_help = "If set, rows with local date < this value are removed before hFWI().",
    init_ffmc = "Initial FFMC",
    init_dmc = "Initial DMC",
    init_dc  = "Initial DC",
    run_hfwi = "Run hFWI()",
    download_results = "Download results (CSV)",
    tab_output = "Output",
    tab_plot   = "Plot",
    tab_log    = "Log",
    data_source = "Data source",
    data_src_results = "Results (hFWI output)",
    data_src_inputs  = "Inputs (weather)",
    vars_to_plot = "Variables to plot (facetted)",
    vars_placeholder = "Select one or more numeric columns…",
    facets_per_row = "Facets per row",
    free_y = "Free y‑scale per facet",
    mapping_help = "Map your columns to what NG‑CFFDRS expects. Provide either a single Date‑Time column, or Year/Month/Day/Hour.",
    col_datetime = "Date‑Time (optional)",
    col_year = "Year",
    col_month = "Month",
    col_day = "Day",
    col_hour = "Hour (0–23)",
    col_temp = "Temperature (°C)",
    col_rh   = "RH (%)",
    col_ws   = "Wind (km/h)",
    col_rain = "Rain (mm)",
    col_lat  = "Latitude (deg)",
    col_lon  = "Longitude (deg)",
    modal_title = "Notice!",
    modal_close = "Dismiss",
    modal_body_html = paste0(
      "<p>This small R Shiny application uses the NG FWI code from the public repository ",
      "<a href=\"https://github.com/nrcan-cfs-fire/cffdrs-ng/tree/main\" target=\"_blank\" rel=\"noopener noreferrer\">cffdrs-ng</a> ",
      "on GitHub to generate hourly Fire Weather Index (FWI2025) outputs. ",
      "Users can upload an hourly weather file and specify key inputs—such as start-up codes and start date—to calculate FWI2025 values.</p>",
      "<p>The app is still in development. Code updates, input handling, and output formatting may change as the FWI2025 system continues to evolve. ",
      "Feedback is welcome, and users should anticipate potential bugs and issues, as the code and application are under development. ",
      "<strong>This should not be utilized for operational purposes and is intended as an exploratory tool.</strong></p>",
      "<p><strong>This application uses Microsoft Copilot, an AI-powered conversational assistant based on the GPT‑5 model, designed to help with information, code generation, and productivity tasks. Version: Copilot (GPT‑5, August 2025).</strong></p>"
    ),
    calc_fwi87 = "Calculate FWI87?",
    fwi25_results_title = "FWI25 (hourly) results",
    fwi87_results_title = "FWI87 (daily) results",
    legend_fwi25 = "FWI2025",
    legend_fwi87 = "FWI87",
    plot_time_x = "Time",
    plot_sel_vars_over_time = "Selected variables over time",
    plot_var_over_time = "%s over time",
    dt_btn_copy = "Copy",
    dt_btn_csv  = "CSV",
    dt_btn_excel= "Excel",
    dt_sSearch = "Search:",
    dt_sLength = "Show _MENU_ entries",
    dt_sInfo   = "Showing _START_ to _END_ of _TOTAL_ entries",
    dt_sInfoEmpty = "Showing 0 to 0 of 0 entries",
    dt_sInfoFilt  = "(filtered from _MAX_ total entries)",
    dt_sZero = "No matching records found",
    dt_sProc = "Processing...",
    dt_pag_first = "First",
    dt_pag_prev  = "Previous",
    dt_pag_next  = "Next",
    dt_pag_last  = "Last",
    aliases_short = c(
      ffmc = "FFMC", dmc = "DMC", dc = "DC", isi = "ISI", bui = "BUI", fwi = "FWI", dsr = "DSR"
    ),
    aliases_long = c(
      ffmc = "Fine Fuel Moisture Code (FFMC)",
      dmc  = "Duff Moisture Code (DMC)",
      dc   = "Drought Code (DC)",
      isi  = "Initial Spread Index (ISI)",
      bui  = "Build‑Up Index (BUI)",
      fwi  = "Fire Weather Index (FWI)",
      dsr  = "Daily Severity Rating (DSR)"
    ),
    # Accessibility & errors
    skip_to_main = "Skip to main content",
    aria_app_label = "Hourly FWI application",
    aria_tabs_label = "Primary output tabs",
    aria_run_label = "Run hFWI",
    tz_not_inferred = "Timezone not yet inferred.",
    err_ffmc_range = "FFMC must be between 0 and 101.",
    err_dmc_range  = "DMC must be a non‑negative number.",
    err_dc_range   = "DC must be a non‑negative number.",
    err_non_numeric_cols = "These columns are not numeric: %s",
    err_tz_invalid = "The selected time zone is not available on this system.",
    iana_prefix = "IANA timezone:"
  ),
  fr = list(
    title = "IFF horaire (NG‑CFFDRS)",
    upload_csv = "Téléverser le fichier CSV météo",
    csv_has_header = "Le CSV comporte une ligne d’en‑tête",
    csv_button_label = "Parcourir...",
    csv_place_holder = "Aucun fichier sélectionné",
    column_mapping = "Correspondance des colonnes",
    lat_label = "Latitude (°)",
    lon_label = "Longitude (°)",
    time_zone = "Fuseau horaire",
    tz_fixed_one = "Utiliser un seul fuseau horaire pour toutes les lignes",
    tz_auto_infer = "Déduire un fuseau horaire à partir de la latitude/longitude",
    tz_select = "Fuseau horaire",
    tz_offset_policy = "Décalage pour les calculs solaires",
    tz_offset_std = "Standard (ignorer l’heure d’été)",
    tz_offset_modal = "À partir des données (modal; peut inclure l’heure d’été)",
    filter = "Filtre",
    drop_rows_prior = "Supprimer les lignes antérieures à (date locale) :",
    drop_rows_help = "Le cas échéant, les lignes dont la date locale est antérieure à cette valeur sont supprimées avant hFWI().",
    init_ffmc = "FFMC initial",
    init_dmc  = "DMC initial",
    init_dc   = "DC initial",
    run_hfwi = "Exécuter hFWI()",
    download_results = "Télécharger les résultats (CSV)",
    tab_output = "Résultats",
    tab_plot   = "Graphique",
    tab_log    = "Journal",
    data_source = "Source de données",
    data_src_results = "Résultats (sortie hFWI)",
    data_src_inputs  = "Entrées (météo)",
    vars_to_plot = "Variables à tracer (par facettes)",
    vars_placeholder = "Sélectionner une ou plusieurs colonnes numériques…",
    facets_per_row = "Nombre de facettes par ligne",
    free_y = "Axe des y libre par facette",
    mapping_help = "Faites correspondre vos colonnes à ce qu’exige NG‑CFFDRS. Fournissez soit une colonne Date‑heure unique, soit Année/Mois/Jour/Heure.",
    col_datetime = "Date‑heure (facultatif)",
    col_year = "Année",
    col_month = "Mois",
    col_day = "Jour",
    col_hour = "Heure (0–23)",
    col_temp = "Température (°C)",
    col_rh   = "HR (%)",
    col_ws   = "Vent (km/h)",
    col_rain = "Pluie (mm)",
    col_lat  = "Latitude (°)",
    col_lon  = "Longitude (°)",
    modal_title = "Avis!",
    modal_close = "Fermer",
    modal_body_html = paste0(
      "<p>Cette petite application R Shiny utilise le code PG‑IFM du dépôt public ",
      "<a href=\"https://github.com/nrcan-cfs-fire/cffdrs-ng/tree/main\" target=\"_blank\" rel=\"noopener noreferrer\">cffdrs‑ng</a> ",
      "sur GitHub pour générer des résultats d'indice horaire (IFM2025). ",
      "Les utilisateurs peuvent téléverser un fichier météorologique horaire et spécifier des intrants clés – tels que les codes de démarrage et la date de début – pour calculer les valeurs IFM2025.</p>",
      "<p>L'application est encore en cours de développement. Les mises à jour des codes, le traitement des intrants et le formatage des résultats peuvent changer au fur et à mesure que le système IFM2025 continue d'évoluer. ",
      "Les commentaires sont les bienvenus et les utilisateurs doivent anticiper les bogues et les problèmes potentiels, car le code et l'application sont en cours de développement. ",
      "<strong>Cette application ne doit pas être utilisée à des fins opérationnelles et est conçue comme un outil exploratoire.</strong></p>",
      "<p><strong>Cette application utilise Microsoft Copilot, un assistant conversationnel alimenté par l’IA basé sur le modèle GPT‑5, conçu pour fournir des informations, générer du code et améliorer la productivité. Version : Copilot (GPT‑5, août 2025).</strong></p>"
    ),
    calc_fwi87 = "Calculer IFM87?",
    fwi25_results_title = "Résultats IFM25 (horaire)",
    fwi87_results_title = "Résultats IFM87 (quotidiens)",
    legend_fwi25 = "IFM2025",
    legend_fwi87 = "IFM87",
    plot_time_x = "Temps",
    plot_sel_vars_over_time = "Variables sélectionnées dans le temps",
    plot_var_over_time = "%s dans le temps",
    dt_btn_copy = "Copier",
    dt_btn_csv  = "CSV",
    dt_btn_excel= "Excel",
    dt_sSearch = "Rechercher :",
    dt_sLength = "Afficher _MENU_ éléments",
    dt_sInfo   = "Affichage de _START_ à _END_ sur _TOTAL_ éléments",
    dt_sInfoEmpty = "Affichage de 0 à 0 sur 0 élément",
    dt_sInfoFilt  = "(filtré à partir de _MAX_ éléments au total)",
    dt_sZero = "Aucun enregistrement correspondant trouvé",
    dt_sProc = "Traitement...",
    dt_pag_first = "Premier",
    dt_pag_prev  = "Précédent",
    dt_pag_next  = "Suivant",
    dt_pag_last  = "Dernier",
    aliases_short = c(
      ffmc = "FFMC", dmc = "DMC", dc = "DC", isi = "IPI (ISI)", bui = "BUI", fwi = "IFM (FWI)", dsr = "IGQ (DSR)"
    ),
    aliases_long = c(
      ffmc = "Indice d’humidité des combustibles fins (FFMC)",
      dmc  = "Indice d’humidité de la couche organique (DMC)",
      dc   = "Indice de sécheresse (DC)",
      isi  = "Indice de propagation initiale (IPI/ISI)",
      bui  = "Indice d’accumulation (BUI)",
      fwi  = "Indice forêt‑météo (IFM/FWI)",
      dsr  = "Indice de gravité quotidienne (IGQ/DSR)"
    ),
    # Accessibility & errors
    skip_to_main = "Passer au contenu principal",
    aria_app_label = "Application IFF horaire",
    aria_tabs_label = "Onglets de sortie principaux",
    aria_run_label = "Exécuter hFWI",
    tz_not_inferred = "Fuseau horaire non encore déduit.",
    err_ffmc_range = "FFMC doit être entre 0 et 101.",
    err_dmc_range  = "Le DMC doit être un nombre positif ou nul.",
    err_dc_range   = "Le DC doit être un nombre positif ou nul.",
    err_non_numeric_cols = "Ces colonnes ne sont pas numériques : %s",
    err_tz_invalid = "Le fuseau horaire sélectionné n’est pas disponible sur ce système.",
    iana_prefix = "Fuseau IANA :"
  )
)

## Precip column finder ----
find_precip_col <- function(cols) {
  cands <- c("rain","precip","prec","prcp","rf")
  hit <- cands[cands %in% cols]
  if (length(hit)) hit[1] else NULL
}



# Server ----------------------------------------------------------------------
server <- function(input, output, session){
  ## Translation helpers ---------------------------------------------------------
  lang <- reactiveVal("en")
  observe({
    qs <- parseQueryString(session$clientData$url_search)
    l  <- tolower(qs[["lang"]]) %||% "en"
    if (l %in% c("en","fr")) lang(l) else lang("en")
  })
  tr <- function(id) {
    L <- lang()
    out <- labels[[L]][[id]]
    if (is.null(out)) id else out
  }
  aliases_active <- function(type = c("short","long")){
    type <- match.arg(type)
    L <- lang()
    key <- if (type == "short") "aliases_short" else "aliases_long"
    out <- labels[[L]][[key]]
    if (is.null(out)) character(0) else out
  }
  label_for_col <- function(nm, type = c("short","long")){
    type <- match.arg(type)
    ali <- aliases_active(type)
    key <- tolower(nm)
    if (length(ali) && key %in% names(ali)) ali[[key]] else nm
  }
  labelize_cols <- function(cols, type = c("short","long")){
    type <- match.arg(type)
    stats::setNames(cols, vapply(cols, label_for_col, character(1), type = type))
  }
  
  # Language toggle (instant, no reload) ----
  output$lang_toggle <- renderUI({
    cur <- lang()
    actionLink(
      "toggle_lang",
      label = if (cur == "fr") "English" else "Français",
      class = "link-unstyled",
      `aria-label` = if (cur == "fr") "Switch to English" else "Passer en français"
    )
  })
  observeEvent(input$toggle_lang, {
    lang(if (lang() == "fr") "en" else "fr")
    shiny::updateQueryString(paste0("?lang=", lang()), mode = "push")
  })

  # Initial labels/outputs translations ----
  output$app_title      <- renderText(tr("title"))
  output$app_subtitle   <- renderText("")
  output$skip_link_txt  <- renderText(tr("skip_to_main"))

  ## Keep <html lang> and key aria-labels synced with current language---
  observeEvent(lang(), {
    session$sendCustomMessage('set-lang', list(lang = lang()))
    session$sendCustomMessage('set-aria-labels', list(
      app = tr('aria_app_label'),
      tabs = tr('aria_tabs_label'),
      run_label = tr('aria_run_label')
    ))
    session$sendCustomMessage('set-title', tr('title'))
  }, ignoreInit = FALSE)

  # Nearest-to-noon record per local day ----------------------------
  nearest_noon_per_day <- function(df, dt_col = "datetime", hour_col = "hour", tz = "UTC"){
    stopifnot(dt_col %in% names(df), hour_col %in% names(df))
    df$date_local <- as.Date(df[[dt_col]], tz = tz)
    dplyr::group_by(df, .data$date_local) |>
      dplyr::slice_min(abs(.data[[hour_col]] - 12), with_ties = FALSE) |>
      dplyr::ungroup()
  }

  # ---- CSV read -------------------------------------------------------------
  raw_file <- reactive({
    req(input$csv)
    tryCatch(
      data.table::fread(
        input$csv$datapath,
        sep = ",",
        na.strings = c("", "NA", "NaN", "null"),
        header = isTRUE(input$has_header)
      ),
      error = function(e)
        utils::read.csv(
          input$csv$datapath,
          header = isTRUE(input$has_header),
          na.strings = c("", "NA", "NaN", "null"),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
    )
  })

  # Prefill start_date from first row if possible
  observeEvent(raw_file(), {
    df <- raw_file()
    cols <- names(df)
    lat_col <- find_col(cols, c("lat","latitude"))
    lon_col <- find_col(cols, c("lon","long","longitude"))
    datetime_col <- find_col(cols, c("datetime","timestamp"))
    lat_default <- suppressWarnings(as.numeric(if (nzchar(lat_col)) df[[lat_col]][1] else NA))
    lon_default <- suppressWarnings(as.numeric(if (nzchar(lon_col)) df[[lon_col]][1] else NA))
    datetime_col <-suppressWarnings(as.numeric(if (nzchar(datetime_col)) df[[datetime_col]][1] else NA))
    if (!is.finite(lat_default)) lat_default <- 55
    if (!is.finite(lon_default)) lon_default <- -120

    updateNumericInput(session, "manual_lat", value = lat_default)
    updateNumericInput(session, "manual_lon", value = lon_default)
    

    first_date <- NULL
    if (!is.na(datetime_col)==T) {
      first_date <- suppressWarnings(as.Date(df[[datetime_col]][1]))
    } else {
      ycol <- find_col(cols, c("year","yr"))
      mcol <- find_col(cols, c("month","mon"))
      dcol <- find_col(cols, c("day","dy"))

      if (!any(is.na(c(ycol, mcol, dcol)))) {
        first_date <- as.Date(paste(df[[ycol]][1], df[[mcol]][1], df[[dcol]][1]), format = "%Y %m %d")
      }
    }
    if (!is.null(first_date)==T && !is.na(first_date)==T) {
      updateDateInput(session, "start_date", value = first_date)
    }
    
  })

  # ---- Mapping UI -----------------------------------------------------------
  find_col <- function(cols, keywords){
    rx <- paste0("^(", paste(keywords, collapse = "|"), ")$")
    m <- cols[grepl(rx, cols, ignore.case = TRUE)]
    if (length(m) > 0) m[1] else ""
  }

  # --- Mapping UI (always visible; disabled until CSV) ---
  output$mapping_ui <- renderUI({
    L <- lang()  # re-render on language switch
    
    has_file <- !is.null(input$csv)
    cols <- if (has_file) names(raw_file()) else character(0)
    
    # Helpers: keep a blank selected when no file is present
    pick_sel <- function(val) if (has_file) val else ""
    fc <- function(x, include_blank = TRUE) if (include_blank) c("", x) else x
    
    # Accessibility: <fieldset disabled> blocks mouse & keyboard interactions
    disabled_attr <- if (!has_file) NA else NULL
    aria_state <- if (!has_file) "true" else "false"
    
    tags$fieldset(
      class = "mapping-ui-fieldset",
      disabled = disabled_attr,            # present when disabled
      `aria-disabled` = aria_state,
      
      helpText(tr("mapping_help")),
      
      selectInput(
        "col_datetime", tr("col_datetime"),
        choices  = fc(cols),
        selected = pick_sel(find_col(cols, c("datetime","timestamp")))
      ),
      
      fluidRow(
        column(3, selectInput("col_year",  tr("col_year"),  choices = fc(cols),
                              selected = pick_sel(find_col(cols, c("year","yr","y"))))),
        column(3, selectInput("col_month", tr("col_month"), choices = fc(cols),
                              selected = pick_sel(find_col(cols, c("month","mon","m"))))),
        column(3, selectInput("col_day",   tr("col_day"),   choices = fc(cols),
                              selected = pick_sel(find_col(cols, c("day","dy","d"))))),
        column(3, selectInput("col_hour",  tr("col_hour"),  choices = fc(cols),
                              selected = pick_sel(find_col(cols, c("hour","hr","h")))))
      ),
      
      fluidRow(
        column(3, selectInput("col_temp", tr("col_temp"), choices = fc(cols),
                              selected = pick_sel(find_col(cols, c("temp","temperature","t"))))),
        column(3, selectInput("col_rh",   tr("col_rh"),   choices = fc(cols),
                              selected = pick_sel(find_col(cols, c("rh","relative humidity",
                                                                   "relative.humidity",
                                                                   "relative_humidity","humidity"))))),
        column(3, selectInput("col_ws",   tr("col_ws"),   choices = fc(cols),
                              selected = pick_sel(find_col(cols, c("ws","windspeed",
                                                                   "wind_speed","wind.speed","wind speed"))))),
        column(3, selectInput("col_rain", tr("col_rain"), choices = fc(cols),
                              selected = pick_sel(find_col(cols, c("rain","precip","prec",
                                                                   "precip_mm","prec_mm","rain_mm")))))
      ),
      
      fluidRow(
        column(6, selectInput("col_lat", tr("col_lat"), choices = fc(cols),
                              selected = pick_sel(find_col(cols, c("lat","latitude"))))),
        column(6, selectInput("col_lon", tr("col_lon"), choices = fc(cols),
                              selected = pick_sel(find_col(cols, c("lon","long","longitude")))))
      ),
      
      fluidRow(
        column(
          6,
          numericInput(
            "manual_lat", tr("lat_label"),
            value = 55, min = -90, max = 90, step = 0.0001
          )
        ),
        column(
          6,
          numericInput(
            "manual_lon", tr("lon_label"),
            value = -120, min = -180, max = 180, step = 0.0001
          )
        )
      )
    )
  })
  
  

  # ---- Language-bound static labels ----------------------------------------
  observeEvent(lang(), {
    output$lbl_upload_csv      <- renderText(tr("upload_csv"))
    output$lbl_column_mapping  <- renderText(tr("column_mapping"))
    updateCheckboxInput(session, "calc_fwi87", label = tr("calc_fwi87"))
    output$lbl_fwi87_title     <- renderText(tr("fwi87_results_title"))
    output$lbl_fwi25_title     <- renderText(tr("fwi25_results_title"))
    updateNumericInput(session, "manual_lat", label = tr("lat_label"))
    updateNumericInput(session, "manual_lon", label = tr("lon_label"))
    output$csv_input_ui <- renderUI({
      fileInput("csv", label = NULL, buttonLabel = tr("csv_button_label"), placeholder = tr("csv_place_holder"), accept = c(".csv","text/csv"))
    })
    updateCheckboxInput(session, "has_header", label = tr("csv_has_header"))

    output$lbl_time_zone <- renderText(tr("time_zone"))
    updateRadioButtons(session, "tz_mode", label = NULL,
      choices  = setNames(c("fixed","auto"), c(tr("tz_fixed_one"), tr("tz_auto_infer"))),
      selected = input$tz_mode %||% "auto"
    )
    updateSelectInput(session, "fixed_tz", label = tr("tz_select"))

    output$lbl_tz_offset_policy <- renderText(tr("tz_offset_policy"))
    updateRadioButtons(session, "tz_offset_policy", label = NULL,
      choices = setNames(c("std","modal"), c(tr("tz_offset_std"), tr("tz_offset_modal"))),
      selected = input$tz_offset_policy %||% "std"
    )

    output$lbl_filter <- renderText(tr("filter"))
    updateDateInput(session, "start_date", label = tr("drop_rows_prior"))
    output$txt_drop_rows_help <- renderText(tr("drop_rows_help"))

    updateNumericInput(session, "ffmc0", label = tr("init_ffmc"))
    updateNumericInput(session, "dmc0",  label = tr("init_dmc"))
    updateNumericInput(session, "dc0",   label = tr("init_dc"))

    output$run <- renderUI(actionButton("run", label = tr("run_hfwi"), class = "btn-primary", `aria-label` = tr("aria_run_label")))
    output$dl_ui <- renderUI(downloadButton_sl("dl", tr("download_results")))

    output$tab_output_title <- renderText(tr("tab_output"))
    output$tab_plot_title   <- renderText(tr("tab_plot"))
    output$tab_log_title    <- renderText(tr("tab_log"))

    updateSelectInput(session, "plot_dataset", label = tr("data_source"),
      choices = setNames(c("results","inputs"), c(tr("data_src_results"), tr("data_src_inputs"))),
      selected = input$plot_dataset %||% "results"
    )
    updateNumericInput(session, "facet_ncol", label = tr("facets_per_row"))
    updateCheckboxInput(session, "facet_free_y", label = tr("free_y"))
  }, ignoreInit = FALSE)

  dt_i18n <- reactive({
    list(
      sSearch = tr("dt_sSearch"),
      sLengthMenu = tr("dt_sLength"),
      sInfo = tr("dt_sInfo"),
      sInfoEmpty = tr("dt_sInfoEmpty"),
      sInfoFiltered = tr("dt_sInfoFilt"),
      sZeroRecords = tr("dt_sZero"),
      sProcessing = tr("dt_sProc"),
      oPaginate = list(
        sFirst = tr("dt_pag_first"), sPrevious = tr("dt_pag_prev"), sNext = tr("dt_pag_next"), sLast = tr("dt_pag_last")
      )
    )
  })

  # ---- Time zone helpers ----------------------------------------------------
  parse_z_to_hours <- function(z_txt){
    z_txt <- as.character(z_txt)
    z_txt <- z_txt[nzchar(z_txt)]
    if (!length(z_txt)) return(NA_real_)
    sgn <- ifelse(substr(z_txt, 1, 1) == "-", -1, 1)
    hh  <- suppressWarnings(as.integer(substr(z_txt, 2, 3)))
    mm  <- suppressWarnings(as.integer(substr(z_txt, 4, 5)))
    sgn * (hh + (mm %||% 0) / 60)
  }
  tz_standard_offset_hours <- function(tz, probe_date = "2025-01-15 12:00:00"){
    probe <- as.POSIXct(probe_date, tz = tz)
    parse_z_to_hours(format(probe, "%z"))
  }
  tz_modal_offset_hours <- function(datetimes){
    z_txt <- format(datetimes, "%z")
    z_txt <- z_txt[nzchar(z_txt)]
    if (!length(z_txt)) stop("Could not infer modal offset: empty %z values.")
    z_mode <- names(which.max(table(z_txt)))
    list(offset = parse_z_to_hours(z_mode), z_mode = z_mode)
  }

  # Splash modal (bilingual body)
  observe({
    showModal(modalDialog(title = tr("modal_title"), HTML(tr("modal_body_html")), easyClose = TRUE, footer = modalButton(tr("modal_close")), size = "l"))
  })

  # Time zone inference state
  observeEvent(input$csv, { tz_guess(NULL) }, ignoreInit = TRUE)
  tz_guess <- reactiveVal(NULL)
  observeEvent(input$tz_lookup_result, ignoreInit = TRUE, {
    if (is.character(input$tz_lookup_result) && nzchar(input$tz_lookup_result)) tz_guess(input$tz_lookup_result)
  }, ignoreNULL = TRUE)

  output$tz_out <- renderPrint({
    tz <- tz_guess()
    if (is.null(tz)) tr("tz_not_inferred") else paste(tr("iana_prefix"), tz)
  })

  get_col <- function(df, nm){
    if (isTruthy(nm) && nzchar(nm) && nm %in% names(df)) df[[nm]] else NULL
  }
  representative_latlon <- function(df){
    mlat <- suppressWarnings(as.numeric(input$manual_lat))
    mlon <- suppressWarnings(as.numeric(input$manual_lon))
    if (length(mlat) == 1 && is.finite(mlat) &&
        length(mlon) == 1 && is.finite(mlon)) {
      return(c(lat = mlat, lon = mlon))
    }
    
    lat <- lon <- NULL
    if (!missing(df) && !is.null(df)) {
      lat <- suppressWarnings(as.numeric(get_col(df, input$col_lat)))
      lon <- suppressWarnings(as.numeric(get_col(df, input$col_lon)))
      if (length(lat) && length(lon)) {
        la <- suppressWarnings(stats::median(lat, na.rm = TRUE))
        lo <- suppressWarnings(stats::median(lon, na.rm = TRUE))
        if (is.finite(la) && is.finite(lo)) return(c(lat = la, lon = lo))
      }
    }
    c(lat = NA_real_, lon = NA_real_)
  }
  observeEvent(list(input$manual_lat, input$manual_lon), {
    if (identical(input$tz_mode, "auto")) {
      mlat <- suppressWarnings(as.numeric(input$manual_lat))
      mlon <- suppressWarnings(as.numeric(input$manual_lon))
      if (is.finite(mlat) && is.finite(mlon)) {
        session$sendCustomMessage("tz_lookup", list(lat = mlat, lon = mlon))
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$tz_mode, input$run, input$csv), ignoreInit = FALSE, {
    if (identical(input$tz_mode, "auto")) {
      df <- try(raw_file(), silent = TRUE)
      if (!inherits(df, "try-error")) {
        coords <- representative_latlon(as.data.frame(df))
        if (is.finite(coords["lat"]) && is.finite(coords["lon"])) {
          session$sendCustomMessage("tz_lookup", list(lat = unname(coords["lat"]), lon = unname(coords["lon"])) )
        } else {
          if (is.character(input$tz_browser) && nzchar(input$tz_browser)) tz_guess(input$tz_browser)
        }
      }
    }
  })

  # ---- Data shaping for model & plot ---------------------------------------
  shaped_input <- eventReactive(input$run, {
    validate(need(!is.null(raw_file()), "Upload a CSV first."))
    df <- tibble::as_tibble(raw_file())

    # required weather columns present
    needed <- c(input$col_temp, input$col_rh, input$col_ws, input$col_rain)
    validate(need(all(nzchar(needed)), "Please map temperature, RH, wind, and rain columns."))

    # must be numeric
    must_be_numeric <- needed
    bad <- Filter(function(nm) !is.numeric(df[[nm]]), must_be_numeric)
    validate(need(length(bad) == 0, sprintf(tr("err_non_numeric_cols"), paste(bad, collapse=", "))))

    # numeric input ranges
    validate(
      need(is.finite(input$ffmc0) && input$ffmc0 >= 0 && input$ffmc0 <= 101, tr("err_ffmc_range")),
      need(is.finite(input$dmc0)  && input$dmc0  >= 0, tr("err_dmc_range")),
      need(is.finite(input$dc0)   && input$dc0   >= 0, tr("err_dc_range")),
      need(is.finite(input$manual_lat) && input$manual_lat >= -90 && input$manual_lat <= 90,
           "Latitude must be between -90 and 90."),
      need(is.finite(input$manual_lon) && input$manual_lon >= -180 && input$manual_lon <= 180,
           "Longitude must be between -180 and 180.")
    )
    

    # timezone to use
    tz_use <- switch(input$tz_mode,
      "fixed" = input$fixed_tz,
      "auto"  = {
        if (!is.null(tz_guess())) tz_guess() else if (is.character(input$tz_browser) && nzchar(input$tz_browser)) input$tz_browser else validate(need(FALSE, "Timezone not inferred yet—map lat/lon or use a fixed timezone."))
      }
    )
    validate(need(tz_use %in% OlsonNames(), tr("err_tz_invalid")))

    # Build datetime
    has_explicit_zone <- function(x){
      if (is.null(x)) return(FALSE)
      x <- as.character(x)
      any(grepl("(Z$)|([\\+\\-]\\d{2}:?\\d{2}$)", x, perl = TRUE), na.rm = TRUE)
    }

    dt_col <- get_col(df, input$col_datetime)
    if (!is.null(dt_col)) {
      if (has_explicit_zone(dt_col)) {
        dt_utc <- lubridate::parse_date_time(dt_col,
          orders = c("Y-m-d H:M:S","Y-m-d H:M","Y/m/d H:M:S","Y/m/d H:M",
                     "d-m-Y H:M:S","d/m/Y H:M:S","m/d/Y H:M:S","m/d/Y H:M",
                     "Ymd HMS","Ymd HM","Ymd H"), tz = "UTC")
        validate(need(!all(is.na(dt_utc)), "Could not parse your Date‑Time column (UTC/offset)."))
        dt_local <- lubridate::with_tz(dt_utc, tz = tz_use)
      } else {
        dt_local <- lubridate::parse_date_time(dt_col,
          orders = c("Y-m-d H:M:S","Y-m-d H:M","Y/m/d H:M:S","Y/m/d H:M",
                     "d-m-Y H:M:S","d/m/Y H:M:S","m/d/Y H:M:S","m/d/Y H:M",
                     "Ymd HMS","Ymd HM","Ymd H"), tz = tz_use)
        validate(need(!all(is.na(dt_local)), "Could not parse your Date‑Time column."))
      }
    } else {
      y <- get_col(df, input$col_year); m <- get_col(df, input$col_month); d <- get_col(df, input$col_day); h <- get_col(df, input$col_hour)
      validate(need(all(!is.null(c(y,m,d,h))), "Provide Date‑Time or Year/Month/Day/Hour."))
      dt_local <- lubridate::make_datetime(year = as.integer(y), month = as.integer(m), day = as.integer(d), hour = as.integer(h), tz = tz_use)
    }

    # Optional start-date filter (local)
    if (!is.null(input$start_date) && !is.na(input$start_date)) {
      keep <- as.Date(dt_local, tz = tz_use) >= as.Date(input$start_date)
      validate(need(any(keep), "All rows were filtered out by the start date."))
      df <- df[keep, , drop = FALSE]
      dt_local <- dt_local[keep]
      validate(need(nrow(df) > 0, "No rows remain after filtering; check your date filter or input data."))
    }

    # Offsets (hours)
    std_probe <- as.POSIXct("2025-01-15 12:00:00", tz = tz_use)
    std_z     <- format(std_probe, "%z")
    std_h     <- tz_standard_offset_hours(tz_use)
    mmodal    <- tryCatch(tz_modal_offset_hours(dt_local), error = function(e) list(offset = NA_real_, z_mode = NA_character_))
    modal_h   <- mmodal$offset
    z_mode    <- mmodal$z_mode

    offset_hours <- if (identical(input$tz_offset_policy, "std")) std_h else modal_h
    if (!isTRUE(all.equal(offset_hours, round(offset_hours)))) {
      warning(sprintf("Time‑zone offset has minutes (%.2f h). Rounding to nearest hour for make_inputs().", offset_hours))
      offset_hours <- round(offset_hours)
    }
    if (is.na(offset_hours) || abs(offset_hours) > 14) stop(sprintf("Computed GMT offset (%.2f) seems invalid.", offset_hours))

    wx <- tibble::tibble(
      datetime = dt_local,
      year  = lubridate::year(dt_local),
      month = lubridate::month(dt_local),
      day   = lubridate::day(dt_local),
      hour  = lubridate::hour(dt_local),
      temp  = as.numeric(get_col(df, input$col_temp)),
      rh    = as.numeric(get_col(df, input$col_rh)),
      ws    = as.numeric(get_col(df, input$col_ws)),
      rain  = as.numeric(get_col(df, input$col_rain)),
      lat = rep(suppressWarnings(as.numeric(input$manual_lat)), nrow(df)),
      long = rep(suppressWarnings(as.numeric(input$manual_lon)), nrow(df)),
      tz    = tz_use
    )

    validate(need(all(!is.na(wx$temp)), "Temperature has NA after parsing."))
    validate(need(all(!is.na(wx$rh)),   "RH has NA after parsing."))
    validate(need(all(!is.na(wx$ws)),   "Wind has NA after parsing."))
    validate(need(all(!is.na(wx$rain)), "Rain has NA after parsing."))

    # inputs <- make_inputs(data.table::as.data.table(wx), timezone = as.numeric(offset_hours))
    inputs <- wx
    list(inputs = inputs, tz = tz_use, tz_offset = offset_hours, start_date = input$start_date,
         n_rows = nrow(wx), diag_std_z = std_z, diag_modal_z = z_mode)
  })
  
  # ---- Model run ------------------------------------------------------------
  run_model <- reactive({
    req(shaped_input())
    si <- shaped_input(); inputs <- si$inputs
    validate(need(exists("hFWI"), "hFWI() not found after sourcing NG_FWI.r"))
    fml <- tryCatch(formals(hFWI), error = function(e) NULL)
    out <- NULL
    try({
      if (!is.null(fml)) {
        argn <- names(fml)
        if (all(c("df_wx","timezone") %in% argn)) {
          out <- hFWI(df_wx = inputs, timezone = si$tz_offset, ffmc_old = input$ffmc0, dmc_old = input$dmc0, dc_old = input$dc0)
        } else if ("inputs" %in% argn) {
          out <- hFWI(inputs = inputs, ffmc0 = input$ffmc0, dmc0 = input$dmc0, dc0 = input$dc0)
        } else if ("df" %in% argn) {
          out <- hFWI(df = inputs, ffmc0 = input$ffmc0, dmc0 = input$dmc0, dc0 = input$dc0)
        } else {
          if (length(argn) >= 5) out <- hFWI(inputs, si$tz_offset, input$ffmc0, input$dmc0, input$dc0) else out <- hFWI(inputs, input$ffmc0, input$dmc0, input$dc0)
        }
      } else {
        out <- try(hFWI(df_wx = inputs, timezone = si$tz_offset, ffmc_old = input$ffmc0, dmc_old = input$dmc0, dc_old = input$dc0), silent = TRUE)
        if (inherits(out, "try-error")) out <- hFWI(inputs = inputs, ffmc0 = input$ffmc0, dmc0 = input$dmc0, dc0 = input$dc0)
      }
    }, silent = TRUE)
    validate(need(!is.null(out), "hFWI() call failed; check the Log tab for details."))
    data.table::as.data.table(as.data.frame(out))
  })

  # ---- Daily FWI87 (optional) ----------------------------------------------
  daily_fwi_df <- eventReactive(input$run, {
    if (!isTRUE(input$calc_fwi87)) return(NULL)
    si <- shaped_input(); req(si, si$inputs)
    tz_use <- if (is.null(si$tz) || !nzchar(si$tz)) "UTC" else si$tz
    wx <- data.table::as.data.table(as.data.frame(si$inputs))
    req("datetime" %in% names(wx) && inherits(wx$datetime, "POSIXt"))

    for (nm in c("temp","rh","ws")) if (!nm %in% names(wx)) stop(sprintf("daily_fwi_df(): '%s' not found.", nm))
    pcol <- find_precip_col(names(wx)); if (is.null(pcol)) stop("daily_fwi_df(): couldn't find precipitation ('rain'/'precip'/'prec'/'prcp'/'rf').")

    use_std <- identical(input$tz_offset_policy, "std")
    if (use_std) {
      cur_off_h <- parse_z_to_hours(format(wx$datetime, "%z"))
      std_off_h <- tz_standard_offset_hours(tz_use)
      delta_h   <- cur_off_h - std_off_h
      wx[, datetime_LST := datetime - lubridate::dhours(delta_h)]
      wx[, dt_base := datetime_LST]
    } else {
      wx[, dt_base := datetime]
    }

    wx[, hour := lubridate::hour(dt_base)]
    wx[, date := as.Date(dt_base, tz = tz_use)]

    noon_tbl <- data.table::as.data.table(nearest_noon_per_day(as.data.frame(wx), dt_col = "dt_base", hour_col = "hour", tz = tz_use))
    noon_tbl[, date := as.Date(dt_base, tz = tz_use)]

    noon_tbl[, `:=`(
      start = as.POSIXct(paste0(format(date - 1L, "%Y-%m-%d"), " 13:00:00"), tz = tz_use),
      end   = as.POSIXct(paste0(format(date,     "%Y-%m-%d"), " 12:00:00"), tz = tz_use)
    )]
    if (use_std) {
      end_delta_h <- parse_z_to_hours(format(noon_tbl$end, "%z")) - tz_standard_offset_hours(tz_use)
      noon_tbl[, `:=`(start = start - lubridate::dhours(end_delta_h), end = end - lubridate::dhours(end_delta_h))]
    }

    data.table::setkey(wx, dt_base)
    acc <- wx[
      noon_tbl,
      on = .(dt_base >= start, dt_base <= end),
      allow.cartesian = TRUE,
      .(prec_24 = sum(get(pcol), na.rm = TRUE), n_hours = sum(!is.na(get(pcol)))),
      by = .EACHI
    ]
    noon_tbl[, `:=`(prec_24 = acc$prec_24, n_hours = acc$n_hours)]
    if (!(pcol %in% names(noon_tbl))) noon_tbl[, (pcol) := NA_real_]

    coords <- representative_latlon(as.data.frame(si$inputs))
    lat_val  <- if (is.finite(coords[["lat"]]))  coords[["lat"]]  else 55
    long_val <- if (is.finite(coords[["lon"]]))  coords[["lon"]]  else -120

    daily_in <- data.table::data.table(
      yr = lubridate::year(noon_tbl$dt_base),
      mon = lubridate::month(noon_tbl$dt_base),
      day = lubridate::day(noon_tbl$dt_base),
      temp = as.numeric(noon_tbl$temp),
      rh   = as.numeric(noon_tbl$rh),
      ws   = as.numeric(noon_tbl$ws),
      prec = as.numeric(noon_tbl$prec_24),
      lat = lat_val,
      long = long_val
    )

    init_df <- data.frame(ffmc = input$ffmc0, dmc = input$dmc0, dc = input$dc0, lat = lat_val)
    out <- tryCatch({
      cffdrs::fwi(input = as.data.frame(daily_in), init = init_df, batch = TRUE, out = "all", lat.adjust = TRUE, uppercase = FALSE)
    }, error = function(e){ message("cffdrs::fwi() failed: ", conditionMessage(e)); NULL })
    if (is.null(out)) return(NULL)

    df87 <- as.data.frame(out); names(df87) <- tolower(names(df87))
    if (!"datetime" %in% names(df87)) {
      mon_col <- if ("mon" %in% names(df87)) "mon" else if ("month" %in% names(df87)) "month" else NULL
      if (!is.null(mon_col) && all(c("yr","day") %in% names(df87))) {
        df87$datetime <- lubridate::make_datetime(year = as.integer(df87$yr), month = as.integer(df87[[mon_col]]), day = as.integer(df87$day), hour = 12L, tz = tz_use)
      }
    }
    if ("datetime" %in% names(df87) && !("date" %in% names(df87))) {
      df87$date <- as.Date(df87$datetime, tz = tz_use)
    } else if (!("date" %in% names(df87)) && all(c("yr","day") %in% names(df87))) {
      mon_col <- if ("mon" %in% names(df87)) "mon" else if ("month" %in% names(df87)) "month" else NULL
      if (!is.null(mon_col)) {
        df87$date <- as.Date(sprintf("%04d-%02d-%02d", as.integer(df87$yr), as.integer(df87[[mon_col]]), as.integer(df87$day)))
      }
    }

    d87 <- data.table::as.data.table(df87)
    if ("date" %in% names(d87)) d87[, date := as.Date(date)]
    d87 <- d87[ noon_tbl[, .(date, precip_12to12 = prec_24, n_hours)], on = "date" ]
    ord <- try(order(d87$datetime), silent = TRUE); if (!inherits(ord, "try-error")) d87 <- d87[ord]
    as.data.frame(d87)
  })

  # ---- Plot data & choices and plots --------------------------------------------------
  ## Plot data ----
  data_for_plot <- reactive({
    req(shaped_input(), input$plot_dataset)
    df <- if (identical(input$plot_dataset, "inputs")) as.data.frame(shaped_input()$inputs) else as.data.frame(run_model())
    validate(need(nrow(df) > 0, "Selected dataset has no rows."))

    dt_candidates <- names(df)[grepl("datetime|timestamp|date|time", names(df), ignore.case = TRUE)]
    dt_col <- character(0)
    if (length(dt_candidates)) {
      typed <- dt_candidates[sapply(df[dt_candidates], function(x) inherits(x, c("POSIXt","Date")) || is.character(x))]
      dt_col <- if (length(typed)) typed[1] else dt_candidates[1]
    }
    if (!length(dt_col) && all(c("year","month","day","hour") %in% names(df))) {
      tz_use <- shaped_input()$tz %||% "UTC"
      df$datetime <- lubridate::make_datetime(year = as.integer(df$year), month = as.integer(df$month), day = as.integer(df$day), hour = as.integer(df$hour), tz = tz_use)
      dt_col <- "datetime"
    }
    validate(need(length(dt_col) == 1, "Couldn't find or construct a datetime/timestamp column."))
    ord <- try(order(df[[dt_col]]), silent = TRUE); if (!inherits(ord, "try-error")) df <- df[ord, , drop = FALSE]
    attr(df, "dt_col") <- dt_col
    df
  })
  ## Plot Choices ----
  populate_plot_choices <- function(){
    df <- data_for_plot(); dt_col <- attr(df, "dt_col")
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    raw_choices <- setdiff(num_cols, dt_col)
    if (length(raw_choices) == 0L) {
      updateSelectizeInput(session, "plot_y_multi", choices = character(0), selected = character(0))
      return(invisible(NULL))
    }
    prev <- isolate(input$plot_y_multi) %||% character(0)
    still_valid <- intersect(prev, raw_choices)

    # Preferred defaults: FFMC, DMC, DC, FWI (case-insensitive)
    pref <- c("ffmc","dmc","dc","fwi")
    lc <- tolower(raw_choices)
    idx <- match(pref, lc, nomatch = 0)
    wanted <- raw_choices[idx[idx > 0]]

    default_sel <- if (length(still_valid)) still_valid else if (length(wanted)) wanted else utils::head(raw_choices, 3)
    named_choices <- labelize_cols(raw_choices, type = "short")
    updateSelectizeInput(session, "plot_y_multi", choices = named_choices, selected = unique(default_sel))
  }
  observeEvent(list(data_for_plot(), input$plot_dataset), { populate_plot_choices() }, ignoreInit = T)
  observeEvent(input$main_tabs, { if (identical(input$main_tabs, "Plot")) populate_plot_choices() }, ignoreInit = T)

  ## Plot ----
  output$plot_ts <- plotly::renderPlotly({
    df <- data_for_plot(); dt_col <- attr(df, "dt_col")
    req(length(input$plot_y_multi) >= 1)
    yvars <- unique(input$plot_y_multi)

    keep_cols <- unique(c(dt_col, yvars))
    df_small <- df[, keep_cols, drop = FALSE]
    ord <- try(order(df_small[[dt_col]]), silent = TRUE); if (!inherits(ord, "try-error")) df_small <- df_small[ord, , drop = FALSE]

    long_df <- df_small |>
      tidyr::pivot_longer(cols = tidyselect::all_of(yvars), names_to = "variable", values_to = "value") |>
      dplyr::filter(!is.na(.data$value))
    req(nrow(long_df) > 0)

    var_label_levels <- vapply(yvars, label_for_col, character(1), type = "short")
    long_df$var_label <- vapply(as.character(long_df$variable), label_for_col, character(1), type = "short")
    long_df$var_label <- factor(long_df$var_label, levels = var_label_levels)
    long_df$source <- tr("legend_fwi25")
    long_df<- long_df |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric),~ round(.x,3)))
    overlay_df <- NULL
    if (identical(input$plot_dataset, "results")) {
      df87 <- daily_fwi_df()
      if (!is.null(df87) && nrow(df87)) {
        dt87 <- if ("datetime" %in% names(df87)) "datetime" else NULL
        if (is.null(dt87) && all(c("year","month","day") %in% names(df87))) {
          si <- shaped_input(); df87$datetime <- lubridate::make_datetime(df87$year, df87$month, df87$day, hour = 12L, tz = si$tz); dt87 <- "datetime"
        }
        if (!is.null(dt87)) {
          common <- intersect(yvars, intersect(names(df87), names(df)))
          if (length(common)) {
            keep87 <- unique(c(dt87, common))
            d87_small <- df87[, keep87, drop = FALSE]
            overlay_df <- d87_small |>
              tidyr::pivot_longer(cols = tidyselect::all_of(common), names_to = "variable", values_to = "value") |>
              dplyr::filter(!is.na(.data$value))
            if (nrow(overlay_df)) {
              overlay_df$var_label <- vapply(as.character(overlay_df$variable), label_for_col, character(1), type = "short")
              overlay_df$var_label <- factor(overlay_df$var_label, levels = var_label_levels)
              overlay_df$source <- tr("legend_fwi87")
              overlay_df<- overlay_df |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric),~ round(.x,3)))
            } else overlay_df <- NULL
          }
        }
      }
    }
    

    ncol_facets <- { val <- input$facet_ncol; if (is.null(val) || is.na(val) || val < 1) 1L else as.integer(val) }
    title_txt <- if (length(yvars) == 1) sprintf(tr("plot_var_over_time"), label_for_col(yvars[1], type = "short")) else tr("plot_sel_vars_over_time")

    col_fwi25 <- "#26374A"  # GCDS primary
    col_fwi87 <- "#BC3331"  # GCDS red
    p <- ggplot2::ggplot(long_df, ggplot2::aes(x = .data[[dt_col]], y = .data$value, colour = .data$source, linetype = .data$source)) +
      ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE) +
      { if (nrow(long_df) < 20000) ggplot2::geom_point(size = 0.8, alpha = 0.7, na.rm = TRUE) else NULL } +
      { if (!is.null(overlay_df)) ggplot2::geom_line(data = overlay_df, ggplot2::aes(x = .data$datetime, y = .data$value, colour = .data$source, linetype = .data$source), linewidth = 0.8, na.rm = TRUE) else NULL } +
      ggplot2::facet_wrap(~var_label, ncol = ncol_facets, scales = if (isTRUE(input$facet_free_y)) "free_y" else "fixed") +
      ggplot2::scale_colour_manual(values = c(`FWI2025` = col_fwi25, `IFM2025` = col_fwi25, `FWI87` = col_fwi87, `IFM87` = col_fwi87)) +
      ggplot2::scale_linetype_manual(values = c(`FWI2025` = "solid", `IFM2025` = "solid", `FWI87` = "dashed", `IFM87` = "dashed")) +
      ggplot2::labs(x = tr("plot_time_x"), y = NULL, title = title_txt, colour = NULL, linetype = NULL) +
      theme_goc()

    plotly::ggplotly(p, tooltip = c("x","y","colour")) |>
      plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","lasso2d")) |> 
      plotly::plotly_build()
  })

 

  # ---- Tables, Download, Log -----------------------------------------------
  ## Tables -----
  
  output$has_tbl   <- reactive({ isTruthy(run_model()) })
  output$has_tbl87 <- reactive({ df87 <- daily_fwi_df(); isTruthy(df87) && NROW(df87) > 0 })
  outputOptions(output, "has_tbl", suspendWhenHidden = FALSE)
  outputOptions(output, "has_tbl87", suspendWhenHidden = FALSE)
  output$tbl <- DT::renderDT({
    req(run_model())
    df <- run_model()
    DT::datatable(
      df, escape = TRUE, filter = "top",
      extensions = c("Buttons","Scroller"),
      class = "display nowrap compact hover stripe gc-dt",
      options = list(
        pageLength = 25, scrollX = TRUE, deferRender = TRUE,
        scrollY = 500, scroller = TRUE, dom = "Bfrtip",
        buttons = list(
          list(extend = "copy", text = tr("dt_btn_copy")),
          list(extend = "csv",  text = tr("dt_btn_csv"),  file = 'hFWI.csv'),
          list(extend = "excel", text = tr("dt_btn_excel"), filename = "hFWI.xlsx")
        ),
        processing = TRUE,
        language = dt_i18n()
      )
    )
  })

  output$tbl_fwi87 <- DT::renderDT({
    df87 <- daily_fwi_df(); req(!is.null(df87), nrow(df87) > 0)
    DT::datatable(
      df87, escape = TRUE, filter = "top",
      extensions = c("Buttons","Scroller"),
      class = "display nowrap compact hover stripe gc-dt",
      options = list(
        pageLength = 25, scrollX = TRUE, deferRender = TRUE,
        scrollY = 400, scroller = TRUE, dom = "Bfrtip",
        buttons = list(
          list(extend = "copy", text = tr("dt_btn_copy")),
          list(extend = "csv",  text = tr("dt_btn_csv"),  filename = "dailyFWI.csv"),
          list(extend = "excel",text = tr("dt_btn_excel"), filename = "dailyFWI.xlsx")
        ),
        processing = TRUE,
        language = dt_i18n()
      )
    )
  })
  ## Downloads ----
  safe_fwrite <- function(x, file){
    tryCatch(data.table::fwrite(x, file), error = function(e) utils::write.csv(x, file, row.names = FALSE))
  }
  
  output$dl <- downloadHandler(
    filename = function() sprintf("hfwi_%s", basename(input$csv$name %||% "results")),
    content  = function(file) safe_fwrite(run_model(), file),
    contentType = "text/csv"
  )

  ## Logs ----
  output$log <- renderPrint({
    si <- shaped_input()
    cat("Rows read:", nrow(req(raw_file())), "\n")
    if (!is.null(si$start_date) && !is.na(si$start_date)) cat("Start-date filter (local):", as.character(si$start_date), "\n") else cat("Start-date filter: (none)\n")
    cat("Rows after filtering:", si$n_rows, "\n")
    cat("Time zone used:", si$tz, "\n")
    cat("GMT offset (hours) passed to make_inputs():", si$tz_offset, "\n")
    cat("Standard %z probe:", si$diag_std_z, "\n")
    if (!is.null(si$diag_modal_z) && nzchar(si$diag_modal_z)) cat("Modal %z:", si$diag_modal_z, "\n")
    cat("Initial codes: FFMC =", input$ffmc0, " DMC =", input$dmc0, " DC =", input$dc0, "\n")
    cat("hFWI() formals: \n"); fml <- try(formals(hFWI), silent = TRUE); print(fml)
    argn <- if (inherits(fml, "try-error") || is.null(fml)) character(0) else names(fml)
    mapping <- if (all(c("df_wx","timezone") %in% argn)) "df_wx + timezone (NG)" else if ("inputs" %in% argn) "inputs (legacy)" else if ("df" %in% argn) "df (legacy)" else "positional fallback / unknown"
    cat("hFWI() call mapping -> ", mapping, "\n")
    ng_commit <- tryCatch(readLines("ng/_ng_commit.txt", warn = FALSE)[1], error = function(e) NA_character_)
    if (isTRUE(nzchar(ng_commit))) cat("cffdrs-ng commit:", ng_commit, "\n")

    df87 <- daily_fwi_df(); if (is.null(df87)) cat("FWI87: not requested or not available\n") else cat("FWI87: rows =", nrow(df87), " (daily)\n"); print(utils::head(df87,10))

    ### --- Sanity checks (precip noon->noon) ----------------------------------
    cat("\n--- Sanity check: noon→noon precipitation (from hourly inputs) ---\n")
    tz_use <- if (is.null(si$tz) || !nzchar(si$tz)) "UTC" else si$tz
    wx <- data.table::as.data.table(as.data.frame(si$inputs))
    if (!"datetime" %in% names(wx)) {
      if (all(c("year","month","day","hour") %in% names(wx))) {
        wx[, datetime := lubridate::make_datetime(year = as.integer(year), month = as.integer(month), day = as.integer(day), hour = as.integer(hour), tz = tz_use)]
      } else { cat(" (Skipping: couldn't find datetime or year/month/day/hour.)\n"); return(invisible()) }
    }
    if (!"hour" %in% names(wx)) wx[, hour := lubridate::hour(datetime)]
    wx[, date := as.Date(datetime, tz = tz_use)]
    if (!"rain" %in% names(wx)) {
      pcol <- find_precip_col(names(wx)); if (is.null(pcol)) { cat(" (Skipping: couldn't find precipitation column.)\n"); return(invisible()) }
      data.table::setnames(wx, pcol, "rain")
    }
    wx[, for_date := ifelse(hour <= 12L, date, date + 1L)]
    daily_chk <- wx[, .(rain_24 = sum(rain, na.rm = TRUE), n_rows = .N, n_non_na = sum(!is.na(rain)), n_prev = sum(hour > 12L), n_am = sum(hour <= 12L)), by = for_date][order(for_date)]
    data.table::setnames(daily_chk, "for_date", "date"); daily_chk[, date := as.Date(date)]
    print(utils::head(daily_chk, 10))

    cat("\nNearest-to-noon selection Δhour (count):\n")
    noon_tbl <- data.table::as.data.table(nearest_noon_per_day(as.data.frame(wx), dt_col = "datetime", hour_col = "hour", tz = tz_use))
    noon_tbl[, noon_hour := lubridate::hour(datetime)]
    delta_tab <- sort(table(noon_tbl$noon_hour - 12L), decreasing = TRUE); print(delta_tab)
    bad <- daily_chk[n_non_na < 24L]; if (nrow(bad)) { cat("\nWARNING: days with < 24 hourly precip values in the noon→noon window:\n"); print(utils::head(bad,10)) } else { cat("\nAll noon→noon windows have 24 hourly precip values.\n") }

    cat("\nLST sanity: standard offset (h) =", tz_standard_offset_hours(si$tz), " current sample %z =", format(head(si$inputs$datetime, 1), "%z"), "\n")
    wx <- data.table::as.data.table(as.data.frame(si$inputs))
    cur_off_h <- parse_z_to_hours(format(wx$datetime, "%z"))
    std_off_h <- tz_standard_offset_hours(si$tz)
    delta_h <- cur_off_h - std_off_h
    wx[, datetime_LST := datetime - lubridate::dhours(delta_h)]
    wx[, hour_LST := lubridate::hour(datetime_LST)]
    wx[, date_LST := as.Date(datetime_LST, tz = si$tz)]
    wx[, for_date_LST := ifelse(hour_LST <= 12L, date_LST, date_LST + 1L)]
    d <- wx$for_date_LST[which.max(wx$for_date_LST)]
    cat("Example window for", as.character(d), ": starts", min(wx[for_date_LST==d, datetime_LST]), " ends", max(wx[for_date_LST==d, datetime_LST]), "\n")
    invisible(NULL)
  })
}
