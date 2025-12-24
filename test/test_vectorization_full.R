# test_vectorization_full.R
# Compare original vs vectorized on wx_prf.csv

library(data.table)
setwd("./app")
source("ng/util.R")
source("ng/NG_FWI.R")

wx <- fread("../test_inputs/wx_prf.csv")
# Provide timezone (-5 hours) consistently to both; DST ignored for parity
orig_time <- system.time(orig <- hFWI(wx, timezone = -5, silent = TRUE, round_out = NA))

source("ng/util_vectorized.R")
source("ng/NG_FWI_vectorized.R")
vec_time <- system.time(vec <- hFWI_vectorized(wx, timezone = -5, silent = TRUE, round_out = NA))


# Align types and order
setDT(orig)
setDT(vec)
setorderv(orig, c("yr", "mon", "day", "hr"))
setorderv(vec, c("yr", "mon", "day", "hr"))

cols <- intersect(names(orig), names(vec))
num_cols <- cols[sapply(orig[, ..cols], is.numeric) & sapply(vec[, ..cols], is.numeric)]

# Differences
diffs <- sapply(num_cols, function(c) max(abs(orig[[c]] - vec[[c]]), na.rm = TRUE))
print(diffs)

all_eq <- all(diffs < 1e-8)
cat("All numeric columns equal within 1e-8 tolerance? ", all_eq, "\n")

# Timings

print(list(original_ms = orig_time["elapsed"] * 1000, vectorized_ms = vec_time["elapsed"] * 1000))
