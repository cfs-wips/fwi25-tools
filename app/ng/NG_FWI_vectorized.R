# NG_FWI_vectorized.R
# Vectorized + preallocated forward-pass implementation of hourly FWI
# Designed for Shinylive/WebR (browser), minimizing per-row overhead.

library(data.table)

# ---- Constants (identical to original) ----
DAILY_K_DMC_DRYING <- 1.894
DAILY_K_DC_DRYING <- 3.937
HOURLY_K_DMC <- 2.22
HOURLY_K_DC <- 0.085
DMC_OFFSET_TEMP <- 0.0
DC_OFFSET_TEMP <- 0.0
DC_DAILY_CONST <- 0.36
DC_HOURLY_CONST <- DC_DAILY_CONST / DAILY_K_DC_DRYING
OFFSET_SUNRISE <- 0.0 # keep 0 to match original defaults
OFFSET_SUNSET <- 0.0
DEFAULT_GRASS_FUEL_LOAD <- 0.35
FFMC_DEFAULT <- 85.0
DMC_DEFAULT <- 6.0
DC_DEFAULT <- 15.0
MPCT_TO_MC <- 250.0 * 59.5 / 101.0
FFMC_INTERCEPT <- 0.5
DMC_INTERCEPT <- 1.5
DC_INTERCEPT <- 2.8
DATE_GRASS <- 181

# ---- Helper conversions (suffix _v to avoid name collisions) ----
ffmc_to_mcffmc_v <- function(ffmc) MPCT_TO_MC * (101 - ffmc) / (59.5 + ffmc)
mcffmc_to_ffmc_v <- function(mcffmc) 59.5 * (250 - mcffmc) / (MPCT_TO_MC + mcffmc)
dmc_to_mcdmc_v <- function(dmc) (280 / exp(dmc / 43.43)) + 20
mcdmc_to_dmc_v <- function(mcdmc) 43.43 * log(280 / (mcdmc - 20))
dc_to_mcdc_v <- function(dc) 400 * exp(-dc / 400)
mcdc_to_dc_v <- function(mcdc) 400 * log(400 / mcdc)

# ---- ISI/BUI/FWI/DSR helpers (vectorized) ----
initial_spread_index_v <- function(ws, ffmc) {
  fm <- ffmc_to_mcffmc_v(ffmc)
  fw <- ifelse(ws >= 40,
    12 * (1 - exp(-0.0818 * (ws - 28))),
    exp(0.05039 * ws)
  )
  ff <- 91.9 * exp(-0.1386 * fm) * (1.0 + (fm^5.31) / 4.93e07)
  0.208 * fw * ff
}

buildup_index_v <- function(dmc, dc) {
  bui <- ifelse(dmc == 0 & dc == 0, 0, 0.8 * dc * dmc / (dmc + 0.4 * dc))
  bui <- ifelse(bui < dmc,
    {
      p <- (dmc - bui) / dmc
      cc <- 0.92 + ((0.0114 * dmc)^1.7)
      dmc - cc * p
    },
    bui
  )
  ifelse(bui <= 0, 0, bui)
}

fire_weather_index_v <- function(isi, bui) {
  bb <- 0.1 * isi * ifelse(bui > 80,
    1000 / (25 + 108.64 / exp(0.023 * bui)),
    0.626 * (bui^0.809) + 2
  )
  ifelse(bb <= 1, bb, exp(2.72 * ((0.434 * log(bb))^0.647)))
}

daily_severity_rating_v <- function(fwi) 0.0272 * (fwi^1.77)

# ---- Grass fuel moisture & indices ----
hourly_grass_fuel_moisture_v <- function(
  lastmc, temp, rh, ws, rain, solrad, load, time_increment = 1.0
) {
  rf <- 0.27
  drf <- 0.389633
  mo <- lastmc
  if (rain != 0) {
    mo <- mo + (rain / load * 100.0)
    if (mo > 250.0) mo <- 250.0
  }
  tf <- temp + 17.9 * solrad * exp(-0.034 * ws)
  rhf <- if (tf > temp) {
    rh * 6.107 * (10.0^(7.5 * temp / (temp + 237.0))) /
      (6.107 * (10.0^(7.5 * tf / (tf + 237.0))))
  } else {
    rh
  }
  e1 <- rf * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)))
  ed <- 1.62 * (rhf^0.532) + (13.7 * exp((rhf - 100) / 13.0)) + e1
  ew <- 1.42 * (rhf^0.512) + (12.0 * exp((rhf - 100) / 18.0)) + e1
  moed <- mo - ed
  moew <- mo - ew
  if (moed == 0 || (moew >= 0 && moed < 0)) {
    m <- mo
  } else {
    if (moed > 0) {
      a1 <- rhf / 100.0
      e <- ed
      moe <- moed
    } else {
      a1 <- (100.0 - rhf) / 100.0
      e <- ew
      moe <- moew
    }
    if (a1 < 0) a1 <- 0
    xkd <- 0.424 * (1 - a1^1.7) + (0.0694 * sqrt(ws) * (1 - a1^8))
    xkd <- xkd * drf * exp(0.0365 * tf)
    m <- e + moe * exp(-1.0 * log(10.0) * xkd * time_increment)
  }
  m
}

Pign_v <- function(mc, wind2m, Cint, Cmc, Cws) 1.0 / (1.0 + exp(-1.0 * (Cint + Cmc * mc + Cws * wind2m)))

curing_factor_v <- function(cur) ifelse(cur >= 20.0, 1.036 / (1 + 103.989 * exp(-0.0996 * (cur - 20))), 0.0)

mcgfmc_to_gfmc_v <- function(mc, cur, wind) {
  wind2m_open_factor <- 0.75
  Intercept <- 1.49
  Cmoisture <- -0.11
  Cwind <- 0.075
  wind2m <- wind2m_open_factor * wind
  probign <- Pign_v(mc, wind2m, Intercept, Cmoisture, Cwind)
  newPign <- curing_factor_v(cur) * probign
  egmc <- ifelse(newPign > 0.0,
    (log(newPign / (1.0 - newPign)) - Intercept - Cwind * wind2m) / Cmoisture,
    250
  )
  egmc[egmc > 250.0] <- 250.0
  mcffmc_to_ffmc_v(egmc)
}

matted_grass_spread_ROS_v <- function(ws, mc, cur) {
  fw <- 16.67 * ifelse(ws < 5, 0.054 + 0.209 * ws, 1.1 + 0.715 * (ws - 5.0)^0.844)
  fm <- ifelse(mc < 12,
    exp(-0.108 * mc),
    ifelse(mc < 20.0 && ws < 10.0,
      0.6838 - 0.0342 * mc,
      ifelse(mc < 23.9 && ws >= 10.0, 0.547 - 0.0228 * mc, 0.0)
    )
  )
  fm[fm < 0] <- 0.0
  cf <- curing_factor_v(cur)
  fw * fm * cf
}

standing_grass_spread_ROS_v <- function(ws, mc, cur) {
  fw <- 16.67 * ifelse(ws < 5, 0.054 + 0.269 * ws, 1.4 + 0.838 * (ws - 5.0)^0.844)
  fm <- ifelse(mc < 12,
    exp(-0.108 * mc),
    ifelse(mc < 20.0 && ws < 10.0,
      0.6838 - 0.0342 * mc,
      ifelse(mc < 23.9 && ws >= 10.0, 0.547 - 0.0228 * mc, 0.0)
    )
  )
  fm[fm < 0] <- 0.0
  cf <- curing_factor_v(cur)
  fw * fm * cf
}

grass_spread_index_v <- function(ws, mc, cur, standing) {
  ros <- if (standing) standing_grass_spread_ROS_v(ws, mc, cur) else matted_grass_spread_ROS_v(ws, mc, cur)
  1.11 * ros
}

grass_fire_weather_index_v <- Vectorize(function(gsi, load) {
  ros <- gsi / 1.11
  Fint <- 300.0 * load * ros
  if (Fint > 100) log(Fint / 60.0) / 0.14 else Fint / 25.0
})

# ---- Wetting/drying helpers ----
drying_units_v <- function() 1.0
rain_since_intercept_reset_v <- function(rain, canopy) {
  TARGET <- 5.0
  if (rain > 0 || canopy$rain_total_prev == 0) {
    canopy$drying_since_intercept <- 0.0
  } else {
    canopy$drying_since_intercept <- canopy$drying_since_intercept + drying_units_v()
    if (canopy$drying_since_intercept >= TARGET) {
      canopy$rain_total_prev <- 0.0
      canopy$drying_since_intercept <- 0.0
    }
  }
  canopy
}

# ---- Station-year engine: vectorized invariants + preallocated forward pass ----

.stnHFWI_fast <- function(
  w, ffmc_old, mcffmc_old, dmc_old, dc_old,
  mcgfmc_matted_old, mcgfmc_standing_old,
  prec_cumulative, canopy_drying
) {
  stopifnot(nrow(w) > 0)
  r <- data.table::copy(w)
  N <- nrow(r)

  # Extract vectors
  temp <- as.numeric(r$temp)
  rh <- as.numeric(r$rh)
  ws <- as.numeric(r$ws)
  rain <- as.numeric(r$prec)
  hr <- as.integer(r$hr)
  sr <- as.numeric(r$sunrise)
  ss <- as.numeric(r$sunset)
  sol <- as.numeric(r$solrad)
  load <- as.numeric(r$grass_fuel_load)
  jday <- julian(r$mon, r$day)
  cured <- as.numeric(r$percent_cured)

  # EMC precomputations
  e1 <- 0.18 * (21.1 - temp) * (1.0 - (1.0 / exp(0.115 * rh)))
  ed <- 0.942 * (rh^0.679) + (11.0 * exp((rh - 100) / 10.0)) + e1
  ew <- 0.618 * (rh^0.753) + (10.0 * exp((rh - 100) / 10.0)) + e1

  # Grass EMC
  tf <- temp + 17.9 * sol * exp(-0.034 * ws)
  rhf <- ifelse(tf > temp,
    rh * 6.107 * (10.0^(7.5 * temp / (temp + 237.0))) /
      (6.107 * (10.0^(7.5 * tf / (tf + 237.0)))),
    rh
  )
  rf_grass <- 0.27
  e1g <- rf_grass * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)))
  edg <- 1.62 * (rhf^0.532) + (13.7 * exp((rhf - 100) / 13.0)) + e1g
  ewg <- 1.42 * (rhf^0.512) + (12.0 * exp((rhf - 100) / 18.0)) + e1g

  # Daytime mask
  day_mask <- ((hr >= (sr + OFFSET_SUNRISE) & hr <= (ss + OFFSET_SUNSET)) |
    (hr < 6 & (hr + 24) >= (sr + OFFSET_SUNRISE) & (hr + 24) <= (ss + OFFSET_SUNSET)))

  # Preallocate outputs
  mcffmc_vec <- ffmc_vec <- mcdmc_vec <- dmc_vec <- mcdc_vec <- dc_vec <- numeric(N)
  isi_vec <- bui_vec <- fwi_vec <- dsr_vec <- numeric(N)
  mcgfmc_matted_vec <- mcgfmc_stand_vec <- gfmc_vec <- gsi_vec <- gfwi_vec <- numeric(N)
  prec_cum_vec <- canopy_dry_vec <- numeric(N)

  # Initialize state
  mcffmc <- if (is.na(mcffmc_old)) ffmc_to_mcffmc_v(ffmc_old) else mcffmc_old
  mcgf_m <- mcgfmc_matted_old
  mcgf_s <- mcgfmc_standing_old
  mcdmc <- dmc_to_mcdmc_v(dmc_old)
  mcdc <- dc_to_mcdc_v(dc_old)
  canopy <- list(rain_total_prev = prec_cumulative, drying_since_intercept = canopy_drying)

  time_increment <- 1.0

  # Forward pass
  for (i in 1:N) {
    canopy <- rain_since_intercept_reset_v(rain[i], canopy)

    # FFMC rain intercept
    rain_ffmc <- if (canopy$rain_total_prev + rain[i] <= FFMC_INTERCEPT) {
      0.0
    } else if (canopy$rain_total_prev > FFMC_INTERCEPT) {
      rain[i]
    } else {
      canopy$rain_total_prev + rain[i] - FFMC_INTERCEPT
    }

    # FFMC moisture update
    mo <- mcffmc
    if (rain_ffmc != 0.0) {
      mo <- mo + 42.5 * rain_ffmc * exp(-100.0 / (251 - mcffmc)) * (1.0 - exp(-6.93 / rain_ffmc))
      if (mcffmc > 150) mo <- mo + 0.0015 * ((mcffmc - 150)^2) * sqrt(rain_ffmc)
      if (mo > 250) mo <- 250.0
    }
    if (mo != ed[i]) {
      a1 <- ifelse(mo > ed[i], rh[i] / 100.0, (100.0 - rh[i]) / 100.0)
      k0k1 <- 0.424 * (1 - (a1^1.7)) + (0.0694 * sqrt(ws[i]) * (1 - (a1^8)))
      kdkw <- (1.0 / 0.50) * 0.0579 * k0k1 * exp(0.0365 * temp[i])
      base <- ifelse(mo < ed[i], ew[i], ed[i])
      mcffmc <- base + (mo - base) * (10^(-kdkw * time_increment))
    } else {
      mcffmc <- mo
    }
    ffmc_vec[i] <- mcffmc_to_ffmc_v(mcffmc)
    mcffmc_vec[i] <- mcffmc

    # --- DMC wetting logic ---
    if (canopy$rain_total_prev + rain[i] > DMC_INTERCEPT) {
      rw <- if (canopy$rain_total_prev < DMC_INTERCEPT) {
        (canopy$rain_total_prev + rain[i]) * 0.92 - 1.27
      } else {
        rain[i] * 0.92
      }
      last_dmc <- mcdmc_to_dmc_v(mcdmc)
      b <- if (last_dmc <= 33) {
        100.0 / (0.3 * last_dmc + 0.5)
      } else if (last_dmc <= 65) {
        -1.3 * log(last_dmc) + 14.0
      } else {
        6.2 * log(last_dmc) - 17.2
      }
      mcdmc <- mcdmc + (1e3 * rw) / (b * rw + 48.77)
    }
    mcdmc <- min(mcdmc, 300.0)

    # DMC drying
    if (day_mask[i]) {
      t_eff <- max(temp[i] + DMC_OFFSET_TEMP, 0.0)
      rk <- HOURLY_K_DMC * 1e-4 * t_eff * (100.0 - rh[i])
      invtau <- rk / 43.43
      mcdmc <- (mcdmc - 20.0) * exp(-time_increment * invtau) + 20.0
    }
    mcdmc <- min(mcdmc, 300.0)
    dmc_vec[i] <- mcdmc_to_dmc_v(mcdmc)
    mcdmc_vec[i] <- mcdmc

    # --- DC wetting logic ---
    if (canopy$rain_total_prev + rain[i] > DC_INTERCEPT) {
      rw <- if (canopy$rain_total_prev <= DC_INTERCEPT) {
        (canopy$rain_total_prev + rain[i]) * 0.83 - 1.27
      } else {
        rain[i] * 0.83
      }
      mcdc <- mcdc + 3.937 * rw / 2.0
    }
    mcdc < -min(mcdc, 400.0)
    # DC drying
    if (day_mask[i]) {
      pe <- if (temp[i] > 0) 0.015 * temp[i] + 3.0 / 16.0 else 0.0
      invtau <- pe / 400.0
      mcdc <- mcdc * exp(-time_increment * invtau)
    }
    mcdc <- min(mcdc, 400.0)
    dc_vec[i] <- mcdc_to_dc_v(mcdc)
    mcdc_vec[i] <- mcdc

    # ISI/BUI/FWI/DSR
    isi_vec[i] <- initial_spread_index_v(ws[i], ffmc_vec[i])
    bui_vec[i] <- buildup_index_v(dmc_vec[i], dc_vec[i])
    fwi_vec[i] <- fire_weather_index_v(isi_vec[i], bui_vec[i])
    dsr_vec[i] <- daily_severity_rating_v(fwi_vec[i])

    # Grass moisture & indices
    mcgf_m <- hourly_grass_fuel_moisture_v(mcgf_m, temp[i], rh[i], ws[i], rain[i], sol[i], load[i])
    mcgf_s <- hourly_grass_fuel_moisture_v(mcgf_s, temp[i], rh[i], ws[i], rain[i] * 0.06, 0.0, load[i])
    mcgfmc_matted_vec[i] <- mcgf_m
    mcgfmc_stand_vec[i] <- mcgf_s
    standing <- jday[i] >= DATE_GRASS
    mcgf <- if (standing) mcgf_s else mcgf_m
    gfmc_vec[i] <- mcgfmc_to_gfmc_v(mcgf, cured[i], ws[i])
    gsi_vec[i] <- grass_spread_index_v(ws[i], mcgf, cured[i], standing)
    gfwi_vec[i] <- grass_fire_weather_index_v(gsi_vec[i], load[i])

    # Canopy accumulators
    canopy$rain_total_prev <- canopy$rain_total_prev + rain[i]
    prec_cum_vec[i] <- canopy$rain_total_prev
    canopy_dry_vec[i] <- canopy$drying_since_intercept
  }

  # Assign outputs
  set(r, j = "mcffmc", value = mcffmc_vec)
  set(r, j = "ffmc", value = ffmc_vec)
  set(r, j = "dmc", value = dmc_vec)
  set(r, j = "dc", value = dc_vec)
  set(r, j = "isi", value = isi_vec)
  set(r, j = "bui", value = bui_vec)
  set(r, j = "fwi", value = fwi_vec)
  set(r, j = "dsr", value = dsr_vec)
  set(r, j = "mcgfmc_matted", value = mcgfmc_matted_vec)
  set(r, j = "mcgfmc_standing", value = mcgfmc_stand_vec)
  set(r, j = "gfmc", value = gfmc_vec)
  set(r, j = "gsi", value = gsi_vec)
  set(r, j = "gfwi", value = gfwi_vec)
  set(r, j = "prec_cumulative", value = prec_cum_vec)
  set(r, j = "canopy_drying", value = canopy_dry_vec)
  r
}


# ---- Wrapper hFWI_vectorized() ----
hFWI <- function(
  df_wx,
  timezone = NA,
  ffmc_old = FFMC_DEFAULT,
  mcffmc_old = NA,
  dmc_old = DMC_DEFAULT,
  dc_old = DC_DEFAULT,
  mcgfmc_matted_old = ffmc_to_mcffmc_v(FFMC_DEFAULT),
  mcgfmc_standing_old = ffmc_to_mcffmc_v(FFMC_DEFAULT),
  prec_cumulative = 0.0,
  canopy_drying = 0.0,
  silent = FALSE,
  round_out = 4
) {
  wasDT <- is.data.table(df_wx)
  wx <- if (wasDT) copy(df_wx) else as.data.table(df_wx)
  setnames(wx, tolower(names(wx)))
  og_names <- names(wx)

  req_cols <- c("lat", "long", "yr", "mon", "day", "hr", "temp", "rh", "ws", "prec")
  for (col in req_cols) if (!(col %in% names(wx))) stop(paste("Missing required input column:", col))

  # timezone handling (numeric offset hours)
  if (is.na(timezone) || timezone == "NA") {
    if (!("timezone" %in% names(wx))) stop("Either provide a timezone column or specify argument in hFWI_vectorized")
  } else {
    wx[, timezone := as.numeric(timezone)]
  }

  # Optional columns
  if (!("id" %in% og_names)) wx[, id := "STN"]
  if (!("minute" %in% og_names)) wx[, minute := 0]
  if (!("date" %in% og_names)) wx[, date := as.character(as.Date(sprintf("%04d-%02d-%02d", yr, mon, day)))]
  if (!("timestamp" %in% og_names)) {
    # Base R POSIXct in UTC (only used for hour extraction / merge)
    ts_str <- sprintf("%04d-%02d-%02d %02d:%02d:00", wx$yr, wx$mon, wx$day, wx$hr, wx$minute)
    wx[, timestamp := as.POSIXct(ts_str, tz = "UTC")]
  }
  if (!("grass_fuel_load" %in% og_names)) wx$grass_fuel_load <- DEFAULT_GRASS_FUEL_LOAD
  if (!("percent_cured" %in% og_names)) wx$percent_cured <- seasonal_curing(julian(wx$mon, wx$day))

  # Range checks (as in original)
  if (is.character(wx$timezone)) stop("UTC offset (timezone) should be numeric")
  stopifnot(all(wx$rh >= 0 & wx$rh <= 100))
  stopifnot(all(wx$ws >= 0))
  stopifnot(all(wx$prec >= 0))
  stopifnot(all(wx$mon >= 1 & wx$mon <= 12))
  stopifnot(all(wx$day >= 1 & wx$day <= 31))
  stopifnot(wx$grass_fuel_load > 0)
  stopifnot(wx$percent_cured >= 0 & wx$percent_cured <= 100)

  if (is.na(mcffmc_old)) {
    if (is.na(ffmc_old)) stop("Either ffmc_old OR mcffmc_old should be NA, not both") else stopifnot(ffmc_old >= 0 & ffmc_old <= 101)
  } else {
    if (is.na(ffmc_old)) stopifnot(mcffmc_old >= 0 & mcffmc_old <= 250) else stop("One of ffmc_old OR mcffmc_old should be NA (omitted), not neither")
  }
  stopifnot(dmc_old >= 0)
  stopifnot(dc_old >= 0)

  # Solar radiation and sunlight: compute once per station-year
  needs_solrad <- !("solrad" %in% og_names)
  wx <- get_sunlight_v(wx, get_solrad = needs_solrad)

  # Run per station-year
  results <- NULL
  for (stn in unique(wx$id)) {
    by_stn <- wx[id == stn]
    for (y in unique(by_stn$yr)) {
      by_y <- by_stn[yr == y]
      if (!silent) message("Running ", stn, " for ", y)
      r <- .stnHFWI_fast(
        w = by_y,
        ffmc_old = ffmc_old,
        mcffmc_old = mcffmc_old,
        dmc_old = dmc_old,
        dc_old = dc_old,
        mcgfmc_matted_old = mcgfmc_matted_old,
        mcgfmc_standing_old = mcgfmc_standing_old,
        prec_cumulative = prec_cumulative,
        canopy_drying = canopy_drying
      )
      results <- if (is.null(results)) r else data.table::rbindlist(list(results, r), use.names = TRUE, fill = TRUE)
    }
  }

  # Remove optional columns added
  if (!("id" %in% og_names)) results <- results[, -c("id")]
  if (!("minute" %in% og_names)) results <- results[, -c("minute")]
  if (!("date" %in% og_names)) results <- results[, -c("date")]
  if (!("timestamp" %in% og_names)) results <- results[, -c("timestamp")]

  if (!(is.na(round_out) || round_out == "NA")) {
    outcols <- c(
      "sunrise", "sunset", "sunlight_hours",
      "mcffmc", "ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr",
      "mcgfmc_matted", "mcgfmc_standing", "gfmc", "gsi", "gfwi",
      "prec_cumulative", "canopy_drying"
    )
    if (!("solrad" %in% og_names)) outcols <- c("solrad", outcols)
    set(results, j = outcols, value = round(results[, ..outcols], as.integer(round_out)))
  }

  if (!wasDT) setDF(results)
  results
}
