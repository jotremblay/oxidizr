# Tests for constant choices and uncertainty propagation

# A crossover study: 8 subjects, glucose and water trials, 2 times, 3 replicates
create_crossover_study <- function(seed = 1, isotopes = TRUE) {
  set.seed(seed)
  ids <- 1:8
  prots <- c("Glucose", "Water")
  calo <- expand.grid(id = ids, protocol = prots, time = c(60, 90), rep = 1:3,
                      stringsAsFactors = FALSE)
  calo$vo2 <- 2.5 + stats::rnorm(nrow(calo), 0, 0.05)
  calo$vco2 <- 2.3 + stats::rnorm(nrow(calo), 0, 0.05)
  calo$rep <- NULL
  rexp <- expand.grid(id = ids, protocol = prots, time = c(60, 90), stringsAsFactors = FALSE)
  background <- stats::rnorm(8, -24, 0.5)
  rexp$rexp <- background[rexp$id] + ifelse(rexp$protocol == "Water", 0.3, 9)
  iso <- IsotopeData(rexp = rexp, rexo = data.frame(protocol = prots, rexo = c(75, NA)),
                     protocol_col = "protocol", rpla = NULL)
  OxidationStudy(
    calorimetry = CalorimetryData(data = calo, protocol_col = "protocol"),
    isotopes = if (isotopes) iso else NULL,
    urea = NULL, environment = NULL, subjects = NULL
  )
}

analyze_quiet <- function(...) suppressMessages(analyze_oxidation(..., validate = FALSE))

# -----------------------------------------------------------------------------
# oxidation_constants
# -----------------------------------------------------------------------------

test_that("the default constant set equals stoich_coefficients and energy_factors", {
  k <- oxidation_constants()
  expect_s3_class(k, "oxidation_constants")
  expect_equal(unclass(k)[names(stoich_coefficients)], stoich_coefficients)
  expect_equal(k$energy, energy_factors)
})

test_that("options reproduce the published equations", {
  pm <- oxidation_constants(lipid = "fatty_acid", molar_volume = "real_gas")
  expect_equal(c(pm$cho_vco2, pm$cho_vo2, pm$fat_vo2, pm$fat_vco2),
               c(4.585, -3.226, 1.695, -1.701), tolerance = 1e-3)
  frayn <- oxidation_constants(lipid = "pos", molar_volume = "ideal_22.4")
  expect_equal(c(frayn$cho_vco2, frayn$cho_vo2, frayn$fat_vo2),
               c(4.55, -3.21, 1.67), tolerance = 2e-3)
})

test_that("protein options give their sources' values", {
  t1 <- oxidation_constants(protein = "telmosse_table1")
  expect_equal(c(t1$protein_vo2, t1$protein_vco2, t1$protein_urea), c(1.0075, 0.8443, 2.915))
  pt <- oxidation_constants(protein = "protein_type", molar_volume = "ideal_22.4")
  # Telmosse (2022, p. 22): 1.0070 and 0.8410 L/g, 2.915 g/g, with 2057.5 g/mol
  expect_equal(c(pt$protein_vo2, pt$protein_vco2, pt$protein_urea),
               c(1.0070, 0.8410, 2.915), tolerance = 2e-3)
})

test_that("constant sets and uncertainty specifications print their choices", {
  expect_output(print(oxidation_constants()), "dioleoyl-palmitoyl-glycerol C55H102O6")
  expect_output(print(oxidation_constants()), "CHO \\(g/min\\) = 4.6143 VCO2 - 3.2746 VO2")
  expect_output(print(oxidation_uncertainty(rexp = 0.1)), "rexp +0.1 absolute +random")
})

test_that("oxidation_constants rejects unknown options and warns on mixed energy", {
  expect_error(oxidation_constants(lipid = "butter"))
  expect_warning(oxidation_constants(energy = "peronnet_massicotte"), "fatty acid")
  expect_no_warning(oxidation_constants(lipid = "fatty_acid", energy = "peronnet_massicotte"))
})

test_that("analyze_oxidation uses the constant set it gets", {
  study <- create_crossover_study(isotopes = FALSE)
  k <- oxidation_constants(lipid = "fatty_acid", molar_volume = "real_gas")
  res <- analyze_quiet(study, constants = k, uncertainty = NULL)
  r <- res@oxidation_rates
  expect_equal(r$cho_total, k$cho_vco2 * r$vco2_lmin + k$cho_vo2 * r$vo2_lmin)
  expect_equal(res@constants$choices$lipid, "fatty_acid")
})

# -----------------------------------------------------------------------------
# oxidation_uncertainty and propagation
# -----------------------------------------------------------------------------

test_that("oxidation_uncertainty checks its inputs", {
  expect_error(oxidation_uncertainty(vo2 = -1))
  expect_error(oxidation_uncertainty(rexo = "data"))
  expect_error(oxidation_uncertainty(kind = list(vo2 = "constant")))
  u <- oxidation_uncertainty(volume = 0.01, kind = list(volume = "random"))
  expect_equal(u$kind$volume, "random")
})

test_that("linear propagation matches the analytic error of the CHO and fat equations", {
  study <- create_crossover_study(isotopes = FALSE)
  res <- analyze_quiet(study, uncertainty = oxidation_uncertainty(vo2 = 0.01, vco2 = 0.01, rref = 0))
  r <- res@oxidation_rates
  k <- stoich_coefficients
  expect_equal(r$cho_total_u, sqrt((k$cho_vco2 * r$vco2_lmin * 0.01)^2 + (k$cho_vo2 * r$vo2_lmin * 0.01)^2))
  expect_equal(r$fat_total_u, sqrt((k$fat_vco2 * r$vco2_lmin * 0.01)^2 + (k$fat_vo2 * r$vo2_lmin * 0.01)^2))
  expect_true(all(r$cho_total_u_sys == 0))
})

test_that("a shared volume error moves CHO and fat by the same relative amount", {
  study <- create_crossover_study(isotopes = FALSE)
  res <- analyze_quiet(study, uncertainty = oxidation_uncertainty(vo2 = 0, vco2 = 0, volume = 0.01, rref = 0))
  r <- res@oxidation_rates
  expect_equal(r$cho_total_u, r$cho_total * 0.01, tolerance = 1e-6)
  expect_equal(r$cho_total_u_sys, r$cho_total_u)
})

test_that("in a crossover, Rref from the data is random, SD * sqrt(1 - 1/n)", {
  study <- create_crossover_study()
  res <- analyze_quiet(study, aggregate = TRUE, control_protocol = "Water",
                       uncertainty = oxidation_uncertainty(vo2 = 0, vco2 = 0))
  expect_equal(unique(res@uncertainty$source), "rref_subject")

  rref <- calc_rref(study@isotopes, "Water")
  g <- res@oxidation_rates$protocol == "Glucose" & res@oxidation_rates$time == 60
  r <- res@oxidation_rates[g, ]
  r <- r[order(r$id), ]
  sd60 <- rref$rref_sd[rref$time == 60]
  iso <- study@isotopes@rexp
  iso <- iso[iso$protocol == "Glucose" & iso$time == 60, ]
  rexp <- iso$rexp[order(iso$id)]
  rref60 <- rref$rref[rref$time == 60]
  # d cho_exo / d rref = VCO2 / k * (Rexp - Rexo) / (Rexo - Rref)^2
  deriv <- r$vco2_lmin / stoich_coefficients$co2_cho_ratio * (rexp - 75) / (75 - rref60)^2
  # The linear method moves Rref by one standard uncertainty, not an infinitesimal step
  expect_equal(r$cho_exo_u, abs(deriv) * sd60 * sqrt(1 - 1 / 8), tolerance = 0.02)
  expect_true(all(r$cho_exo_u_sys == 0))
})

test_that("with a separate control group, Rref adds a systematic SD / sqrt(n)", {
  study <- create_crossover_study()
  # Control trials from other subjects (ids 101-108): a parallel design
  iso <- study@isotopes
  rexp <- iso@rexp
  rexp$id[rexp$protocol == "Water"] <- rexp$id[rexp$protocol == "Water"] + 100
  iso@rexp <- rexp
  study@isotopes <- iso
  res <- analyze_quiet(study, aggregate = TRUE, control_protocol = "Water",
                       uncertainty = oxidation_uncertainty(vo2 = 0, vco2 = 0))
  expect_setequal(unique(res@uncertainty$source), c("rref_subject", "rref_mean"))
  r <- res@oxidation_rates[res@oxidation_rates$protocol == "Glucose", ]
  ratio <- r$cho_exo_u_sys / r$cho_exo_u
  # u_sys / u = (SD / sqrt(n)) / sqrt(SD^2 + SD^2 / n) = 1 / sqrt(n + 1)
  expect_equal(ratio, rep(1 / sqrt(9), nrow(r)), tolerance = 0.02)
})

test_that("a numeric Rref is random with each subject's own control trial", {
  study <- create_crossover_study()
  res <- analyze_quiet(study, aggregate = TRUE, control_protocol = "Water", rref_by_subject = TRUE,
                       uncertainty = oxidation_uncertainty(vo2 = 0, vco2 = 0, rref = 0.25))
  expect_equal(unique(res@uncertainty$kind), "random")
  expect_warning(oxidation_uncertainty(kind = list(rref = "random")), "not used")
})

test_that("the VO2-VCO2 replicate correlation is kept", {
  set.seed(3)
  calo <- expand.grid(id = 1:6, time = c(60, 90), rep = 1:10)
  common <- stats::rnorm(nrow(calo), 0, 0.06)    # breathing moves both gases
  calo$vo2 <- 2.5 * (1 + common + stats::rnorm(nrow(calo), 0, 0.005))
  calo$vco2 <- 2.3 * (1 + common + stats::rnorm(nrow(calo), 0, 0.005))
  calo$rep <- NULL
  study <- OxidationStudy(calorimetry = CalorimetryData(data = calo), isotopes = NULL,
                          urea = NULL, environment = NULL, subjects = NULL)
  res <- analyze_quiet(study, aggregate = TRUE)

  # Direct standard error of the mean of the per-replicate CHO and fat
  k <- stoich_coefficients
  calo$cho <- k$cho_vco2 * calo$vco2 + k$cho_vo2 * calo$vo2
  calo$fat <- k$fat_vo2 * calo$vo2 + k$fat_vco2 * calo$vco2
  se <- stats::aggregate(cbind(cho, fat) ~ id + time, calo, function(x) stats::sd(x) / sqrt(length(x)))
  r <- res@oxidation_rates
  r <- r[order(r$id, r$time), ]
  se <- se[order(se$id, se$time), ]
  expect_equal(r$cho_total_u, se$cho, tolerance = 1e-6)
  expect_equal(r$fat_total_u, se$fat, tolerance = 1e-6)
})

test_that("a mean of one replicate has an NA uncertainty, not 0", {
  study <- create_crossover_study(isotopes = FALSE)
  d <- study@calorimetry@data
  one <- which(d$id == 4 & d$time == 90 & d$protocol == "Glucose")
  study@calorimetry@data <- d[-one[2:3], ]
  res <- suppressWarnings(analyze_quiet(study, aggregate = TRUE))
  r <- res@oxidation_rates
  single <- r$id == 4 & r$time == 90 & r$protocol == "Glucose"
  expect_true(is.na(r$cho_total_u[single]))
  expect_true(all(!is.na(r$cho_total_u[!single])))
})

test_that("replicate and control counts leave out missing values", {
  study <- create_crossover_study()
  d <- study@calorimetry@data
  d$vo2[d$id == 1 & d$time == 60 & d$protocol == "Glucose"][1] <- NA
  agg <- aggregate_calorimetry(CalorimetryData(data = d, protocol_col = "protocol"))@data
  row <- agg$id == 1 & agg$time == 60 & agg$protocol == "Glucose"
  expect_equal(c(agg$vo2_n[row], agg$vco2_n[row], agg$n_replicates[row]), c(2, 3, 3))

  iso <- study@isotopes
  rexp <- iso@rexp
  rexp$rexp[rexp$protocol == "Water" & rexp$time == 60][1:3] <- NA
  iso@rexp <- rexp
  expect_equal(calc_rref(iso, "Water")$n[calc_rref(iso, "Water")$time == 60], 5)
})

test_that("a subject with no own control value gets NA exogenous CHO and a warning", {
  study <- create_crossover_study()
  iso <- study@isotopes
  rexp <- iso@rexp
  rexp$rexp[rexp$protocol == "Water" & rexp$id == 3] <- NA
  iso@rexp <- rexp
  study@isotopes <- iso
  expect_warning(
    res <- analyze_quiet(study, aggregate = TRUE, control_protocol = "Water",
                         rref_by_subject = TRUE, uncertainty = NULL),
    "No own control value"
  )
  r <- res@oxidation_rates
  expect_true(all(is.na(r$cho_exo[r$id == 3 & r$protocol == "Glucose"])))
  expect_true(all(!is.na(r$cho_exo[r$id != 3 & r$protocol == "Glucose"])))
})

test_that("the default uncertainty uses only what the data show", {
  study <- create_crossover_study()
  plain <- analyze_quiet(create_crossover_study(isotopes = FALSE))
  expect_null(plain@uncertainty)
  expect_false("cho_total_u" %in% names(plain@oxidation_rates))

  agg <- analyze_quiet(study, aggregate = TRUE, control_protocol = "Water")
  expect_setequal(unique(agg@uncertainty$source), c("vo2", "vco2", "rref_subject"))
  expect_true(all(c("cho_total_u", "pct_fat_u") %in% c(names(agg@oxidation_rates), names(agg@energy_contributions))))
})

test_that("Monte Carlo agrees with the linear method for the linear equations", {
  study <- create_crossover_study(isotopes = FALSE)
  u_lin <- oxidation_uncertainty(vo2 = 0.01, vco2 = 0.01, rref = 0)
  u_mc <- oxidation_uncertainty(vo2 = 0.01, vco2 = 0.01, rref = 0, method = "monte_carlo", n_draws = 400)
  lin <- analyze_quiet(study, uncertainty = u_lin)
  set.seed(42)
  mc <- analyze_quiet(study, uncertainty = u_mc)
  expect_equal(mc@oxidation_rates$cho_total_u, lin@oxidation_rates$cho_total_u, tolerance = 0.15)
  expect_setequal(unique(mc@uncertainty$source), "monte_carlo_random")
})

test_that("uncertainty_budget ranks the sources and its shares sum to 1", {
  study <- create_crossover_study()
  res <- analyze_quiet(study, aggregate = TRUE, control_protocol = "Water",
                       uncertainty = oxidation_uncertainty(vco2 = 0.01, rexp = 0.1, rexo = 0.2))
  b <- uncertainty_budget(res, "cho_exo", by = "protocol")
  glucose <- b[b$protocol == "Glucose", ]
  expect_equal(sum(glucose$share), 1)
  expect_equal(glucose$source[1], "rref_subject")
  expect_true(all(is.na(b$share[b$protocol == "Water"])))
  expect_error(uncertainty_budget(res, "nothing"), "No uncertainty")
  expect_error(uncertainty_budget(analyze_quiet(create_crossover_study(isotopes = FALSE), uncertainty = NULL),
                                  "cho_total"), "no propagated")
})

test_that("Rref from each subject's own control trial removes the group spread", {
  study <- create_crossover_study()
  rref <- calc_rref(study@isotopes, "Water", by_subject = TRUE)
  expect_equal(nrow(rref), 16)
  expect_true(all(c("id", "time", "rref") %in% names(rref)))

  res <- analyze_quiet(study, aggregate = TRUE, control_protocol = "Water",
                       rref_by_subject = TRUE, uncertainty = NULL)
  r <- res@oxidation_rates[res@oxidation_rates$protocol == "Glucose", ]
  # Each subject's own background: Rexp - Rref is the simulated 9 - 0.3 per mil
  own <- rref$rref[match(paste(r$id, r$time), paste(rref$id, rref$time))]
  expect_equal(r$cho_exo, r$vco2_lmin * 8.7 / (75 - own) / stoich_coefficients$co2_cho_ratio)
})

test_that("crossover isotope data keep one row per subject, protocol and time", {
  study <- create_crossover_study()
  res <- analyze_quiet(study, aggregate = TRUE, control_protocol = "Water", uncertainty = NULL)
  expect_equal(nrow(res@oxidation_rates), 8 * 2 * 2)
  expect_error(analyze_quiet(study, control_protocol = "Water", uncertainty = NULL), "aggregate = TRUE")
})

test_that("aggregate_calorimetry keeps the SD and number of replicates", {
  agg <- aggregate_calorimetry(create_crossover_study()@calorimetry)
  expect_true(all(c("vo2_sd", "vco2_sd", "vo2_n", "vco2_n", "gas_cor", "n_replicates") %in% names(agg@data)))
  expect_true(all(agg@data$n_replicates == 3))
})

test_that("compare_constants gives one row per constant set", {
  study <- create_crossover_study(isotopes = FALSE)
  sets <- list(oxidizr = oxidation_constants(),
               real_gas = oxidation_constants(molar_volume = "real_gas"))
  cmp <- compare_constants(study, sets = sets, vars = c("cho_total", "fat_total"))
  expect_equal(cmp$set, c("oxidizr", "real_gas"))
  expect_gt(cmp$cho_total_mean[2], cmp$cho_total_mean[1])
  expect_lt(cmp$fat_total_mean[2], cmp$fat_total_mean[1])
  variants <- constant_variants()
  # default + other lipids (4) + other molar volumes (2) + other proteins (3) + energy pairing
  expect_equal(length(variants), 1 + 4 + 2 + 3 + 1)
  expect_equal(variants[["energy = peronnet_massicotte, lipid = fatty_acid"]]$choices$lipid, "fatty_acid")
})

test_that("summary() and the tables use the subject as the unit", {
  study <- create_crossover_study(isotopes = FALSE)
  res <- analyze_quiet(study, uncertainty = NULL)
  sm <- summary(res, by = "protocol")
  expect_equal(sm$n, c(8, 8))
  expect_true("protein_ox_mean" %in% names(sm))
  expect_message(tbl_oxidation_summary(res, by = "condition"), "not found")
})
