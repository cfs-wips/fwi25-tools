options(fwi.debug_times = FALSE)
options(shiny.bindcache.default = "app")
reactlog::reactlog_enable()
library(shiny)
library(munsell)

# Source NG-CFFDRS vendored code
# source("ng/util.r", local = FALSE)
# source("ng/NG_FWI.r", local = FALSE)
# source("ng/make_inputs.r", local = FALSE)
# Source my vectorized version of the ng-cffdrs code
source("ng/util_vectorized.R", local = FALSE)
source("ng/NG_FWI_vectorized.R", local = FALSE)

# Source modules
for (f in list.files("R/modules", pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)
source("R/helpers/i18n_labels.R",local = FALSE)
source("R/helpers/help.R",local = FALSE)

