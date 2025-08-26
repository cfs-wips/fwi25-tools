source("ui.R", local = TRUE)
source("server.R", local = TRUE)

if (FALSE) {
  library(ggplot2); library(DT)
  # ggplot2 deps
  library(munsell); library(colorspace); library(scales)
  library(isoband); library(farver); library(withr)
  library(labeling); library(gtable)
  # your other deps
  library(data.table); library(dplyr); library(tidyr)
  library(tibble); library(lubridate); library(stringr); library(purrr)
}

shiny::shinyApp(ui = ui, server = server)
