# Tests for protein oxidation functions

# -----------------------------------------------------------------------------
# Test Data Setup
# -----------------------------------------------------------------------------

create_test_urea_loss <- function() {
  data.frame(
    id = 1:5,
    urine_urea_loss = c(10, 12, 11, 9, 10),  # mmol
    sweat_urea_loss = c(5, 6, 5.5, 4.5, 5),   # mmol
    total_urea_loss = c(15, 18, 16.5, 13.5, 15),  # mmol
    urea_mass_loss = c(0.9, 1.08, 0.99, 0.81, 0.9)  # g
  )
}

# -----------------------------------------------------------------------------
# calc_protein_oxidation Tests
# -----------------------------------------------------------------------------

test_that("calc_protein_oxidation returns correct structure", {
  urea_loss <- create_test_urea_loss()
  result <- calc_protein_oxidation(urea_loss, exercise_duration = 120)

  expect_s3_class(result, "tbl_df")
  expect_true("total_protein_ox" %in% names(result))
  expect_true("protein_ox" %in% names(result))
  expect_true("id" %in% names(result))
})

test_that("calc_protein_oxidation calculates correctly with default conversion factor", {
  urea_loss <- data.frame(
    id = 1,
    urea_mass_loss = 1.0  # 1 gram urea
  )

  result <- calc_protein_oxidation(urea_loss, exercise_duration = 120)

  # Default factor: Kleiber's standard protein without S, 2.8660 g per g urea
  expected_total <- 1.0 * stoich_coefficients$protein_urea
  expected_rate <- expected_total / 120

  expect_equal(result$total_protein_ox, expected_total)
  expect_equal(result$protein_ox, expected_rate)
})

test_that("calc_protein_oxidation respects custom conversion factor", {
  urea_loss <- data.frame(
    id = 1,
    urea_mass_loss = 1.0
  )

  custom_factor <- 3.0
  result <- calc_protein_oxidation(
    urea_loss,
    exercise_duration = 60,
    conversion_factor = custom_factor
  )

  expected_total <- 1.0 * custom_factor
  expected_rate <- expected_total / 60

  expect_equal(result$total_protein_ox, expected_total)
  expect_equal(result$protein_ox, expected_rate)
})

test_that("calc_protein_oxidation respects exercise_duration", {
  urea_loss <- data.frame(
    id = 1,
    urea_mass_loss = 1.0
  )

  result_60 <- calc_protein_oxidation(urea_loss, exercise_duration = 60)
  result_120 <- calc_protein_oxidation(urea_loss, exercise_duration = 120)

  # Rate should be 2x higher with half the duration
  expect_equal(result_60$protein_ox, result_120$protein_ox * 2)
})

test_that("calc_protein_oxidation errors on missing column", {
  bad_data <- data.frame(id = 1, wrong_col = 1.0)

  expect_error(
    calc_protein_oxidation(bad_data),
    "urea_mass_loss"
  )
})

test_that("calc_protein_oxidation handles custom id_col", {
  urea_loss <- data.frame(
    subject = c("A", "B"),
    urea_mass_loss = c(1.0, 1.2)
  )

  result <- calc_protein_oxidation(urea_loss, id_col = "subject")

  expect_true("subject" %in% names(result))
  expect_equal(result$subject, c("A", "B"))
})

# -----------------------------------------------------------------------------
# calc_protein_gas_exchange Tests
# -----------------------------------------------------------------------------

test_that("calc_protein_gas_exchange returns correct structure", {
  protein_ox <- data.frame(
    id = 1:3,
    protein_ox = c(0.1, 0.12, 0.11)
  )

  result <- calc_protein_gas_exchange(protein_ox)

  expect_s3_class(result, "tbl_df")
  expect_true("vo2_protein" %in% names(result))
  expect_true("vco2_protein" %in% names(result))
})

test_that("calc_protein_gas_exchange calculates with default factors", {
  protein_ox <- data.frame(
    id = 1,
    protein_ox = 0.1  # g/min
  )

  result <- calc_protein_gas_exchange(protein_ox)

  # Default factors: Kleiber's standard protein without S oxidized to urea
  expect_equal(result$vo2_protein, 0.1 * stoich_coefficients$protein_vo2)
  expect_equal(result$vco2_protein, 0.1 * stoich_coefficients$protein_vco2)
  expect_equal(round(stoich_coefficients$protein_vo2, 4), 1.0443)
  expect_equal(round(stoich_coefficients$protein_vco2, 4), 0.8715)
})

test_that("calc_protein_gas_exchange accepts custom factors", {
  protein_ox <- data.frame(
    id = 1,
    protein_ox = 0.1
  )

  result <- calc_protein_gas_exchange(
    protein_ox,
    vo2_factor = 1.0,
    vco2_factor = 0.8
  )

  expect_equal(result$vo2_protein, 0.1)
  expect_equal(result$vco2_protein, 0.08)
})

test_that("calc_protein_gas_exchange accepts numeric vector", {
  protein_values <- c(0.1, 0.12, 0.11)

  result <- calc_protein_gas_exchange(protein_values)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(result$vo2_protein, protein_values * stoich_coefficients$protein_vo2)
})

test_that("calc_protein_gas_exchange errors on missing column", {
  bad_data <- data.frame(id = 1, wrong_col = 0.1)

  expect_error(
    calc_protein_gas_exchange(bad_data),
    "protein_ox"
  )
})

# -----------------------------------------------------------------------------
# estimate_protein_simple Tests
# -----------------------------------------------------------------------------

test_that("estimate_protein_simple calculates correctly", {
  total_energy <- 10  # kcal/min

  result <- estimate_protein_simple(total_energy)

  # Default: 5% protein contribution, 4.70 kcal/g
  expected <- (10 * 0.05) / 4.70

  expect_equal(result, expected)
})

test_that("estimate_protein_simple respects custom percent", {
  total_energy <- 10

  result_5pct <- estimate_protein_simple(total_energy, protein_percent = 0.05)
  result_10pct <- estimate_protein_simple(total_energy, protein_percent = 0.10)

  expect_equal(result_10pct, result_5pct * 2)
})

test_that("estimate_protein_simple respects custom energy factor", {
  total_energy <- 10

  result_default <- estimate_protein_simple(total_energy)
  result_custom <- estimate_protein_simple(total_energy, protein_factor = 4.0)

  # Lower factor = higher protein oxidation estimate
  expect_gt(result_custom, result_default)
})

test_that("estimate_protein_simple handles vector input", {
  total_energy <- c(8, 10, 12)

  result <- estimate_protein_simple(total_energy)

  expect_length(result, 3)
  expect_equal(result, (total_energy * 0.05) / 4.70)
})

test_that("estimate_protein_simple handles zero energy", {
  result <- estimate_protein_simple(0)
  expect_equal(result, 0)
})

# -----------------------------------------------------------------------------
# calc_sweat_loss and calc_urea_loss Tests
# -----------------------------------------------------------------------------

create_sweat_inputs <- function(urine = c(200, 300)) {
  list(
    subjects = SubjectData(
      data = data.frame(
        id = c(1, 1), protocol = c("A", "B"),
        initial_body_mass = 70, final_body_mass = c(69, 68.5),
        water_ingested = c(500, 1000), saline = 0, urine = urine
      ),
      protocol_col = "protocol"
    ),
    environment = EnvironmentData(
      data = data.frame(id = 1, temperature = 20, humidity = 50, pressure = 760)
    ),
    calo = CalorimetryData(
      data = data.frame(
        id = 1, time = c(30, 60, 30, 60), protocol = c("A", "A", "B", "B"),
        vo2 = c(2, 2, 3, 3), vco2 = c(1.8, 1.8, 2.7, 2.7)
      ),
      protocol_col = "protocol"
    )
  )
}

# Independent copy of the mass balance, including urine (mL)
reference_sweat_loss <- function(vo2, vco2, ve, temp, hum, p, init, fin, water, saline, t,
                                 base = 0, urine_ml = 0, constants = oxidation_constants()) {
  mv <- constants$molar_volume
  mm <- constants$molar_mass
  gas <- (vco2 * mm[["CO2"]] / mv[["CO2"]] - vo2 * mm[["O2"]] / mv[["O2"]]) * t
  p_amb <- 5.53275 * 10^(0.02585 * temp) * hum / 100
  vin <- ve * t / p * p_amb
  vout <- ve * t / p * 47
  h2o <- (((273 * (((vout - vin) * p) / 760)) / (273 + 37)) / mv[["H2O"]]) * mm[["H2O"]]
  init + (water + base + saline) / 1000 - (gas + h2o + urine_ml) / 1000 - fin
}

# 22.4 L/mol and 44, 32, 18 g/mol: the values hard-coded before constants were used
legacy_sweat_constants <- function() {
  k <- oxidation_constants(molar_volume = "ideal_22.4")
  k$molar_mass <- c(O2 = 32, CO2 = 44, H2O = 18)
  k
}

test_that("calc_sweat_loss gives one row per subject and protocol", {
  x <- create_sweat_inputs()
  result <- calc_sweat_loss(x$subjects, x$calo, x$environment)

  expect_equal(names(result), c("id", "protocol", "sweat_loss"))
  expect_equal(nrow(result), 2)

  # Each protocol uses its own gas exchange and removes its own urine
  expected <- c(
    reference_sweat_loss(2, 1.8, 2 * 25, 20, 50, 760, 70, 69, 500, 0, 120, urine_ml = 200),
    reference_sweat_loss(3, 2.7, 3 * 25, 20, 50, 760, 70, 68.5, 1000, 0, 120, urine_ml = 300)
  )
  expect_equal(result$sweat_loss, expected)
})

test_that("sweat loss uses the default molar volume and molar masses", {
  k <- oxidation_constants()
  expect_equal(k$choices$molar_volume, "livesey_elia")
  expect_equal(k$molar_volume, c(O2 = 22.414, CO2 = 22.414, H2O = 22.414))
  expect_equal(k$molar_mass, c(O2 = 31.9988, CO2 = 44.0098, H2O = 18.01528))
  expect_equal(oxidation_constants(molar_volume = "ideal_22.4")$molar_volume,
               c(O2 = 22.4, CO2 = 22.4, H2O = 22.4))
  expect_equal(oxidation_constants(molar_volume = "real_gas")$molar_volume,
               c(O2 = 22.3858, CO2 = 22.2966, H2O = 22.414))

  x <- create_sweat_inputs()
  default <- calc_sweat_loss(x$subjects, x$calo, x$environment)
  explicit <- calc_sweat_loss(x$subjects, x$calo, x$environment, constants = k)
  expect_equal(default$sweat_loss, explicit$sweat_loss)
})

test_that("sweat loss changes with molar volume and molar mass", {
  x <- create_sweat_inputs()
  default <- calc_sweat_loss(x$subjects, x$calo, x$environment)

  by_volume <- calc_sweat_loss(
    x$subjects, x$calo, x$environment,
    constants = oxidation_constants(molar_volume = "ideal_22.4")
  )
  by_real_gas <- calc_sweat_loss(
    x$subjects, x$calo, x$environment,
    constants = oxidation_constants(molar_volume = "real_gas")
  )
  rounded <- oxidation_constants()
  rounded$molar_mass <- c(O2 = 32, CO2 = 44, H2O = 18)
  by_mass <- calc_sweat_loss(x$subjects, x$calo, x$environment, constants = rounded)

  expect_false(isTRUE(all.equal(default$sweat_loss, by_volume$sweat_loss)))
  expect_false(isTRUE(all.equal(default$sweat_loss, by_real_gas$sweat_loss)))
  expect_false(isTRUE(all.equal(default$sweat_loss, by_mass$sweat_loss)))

  # The previous hard-coded balance is what ideal_22.4 plus rounded masses give
  legacy <- calc_sweat_loss(x$subjects, x$calo, x$environment,
                            constants = legacy_sweat_constants())
  expected <- c(
    reference_sweat_loss(2, 1.8, 2 * 25, 20, 50, 760, 70, 69, 500, 0, 120,
                         urine_ml = 200, constants = legacy_sweat_constants()),
    reference_sweat_loss(3, 2.7, 3 * 25, 20, 50, 760, 70, 68.5, 1000, 0, 120,
                         urine_ml = 300, constants = legacy_sweat_constants())
  )
  expect_equal(legacy$sweat_loss, expected)
  expect_false(isTRUE(all.equal(default$sweat_loss, legacy$sweat_loss)))
})

test_that("calc_sweat_loss adds no baseline water by default", {
  x <- create_sweat_inputs()
  default <- calc_sweat_loss(x$subjects, x$calo, x$environment)
  with_water <- calc_sweat_loss(x$subjects, x$calo, x$environment,
                                baseline_water_ml = 1000)

  expect_equal(with_water$sweat_loss - default$sweat_loss, c(1, 1))
})

test_that("calc_sweat_loss takes urine as 0 when the column is absent", {
  x <- create_sweat_inputs()
  no_urine <- x$subjects
  no_urine@data$urine <- NULL

  with_urine <- calc_sweat_loss(x$subjects, x$calo, x$environment)
  without <- suppressMessages(calc_sweat_loss(no_urine, x$calo, x$environment))

  expect_equal(without$sweat_loss - with_urine$sweat_loss, c(0.2, 0.3))
})

test_that("calc_urea_loss keeps the protocol of each urine volume", {
  x <- create_sweat_inputs()
  sweat <- calc_sweat_loss(x$subjects, x$calo, x$environment)
  urea <- UreaData(data = data.frame(id = 1, sweat_urea = 10, urine_urea = 300))

  result <- calc_urea_loss(urea, sweat, subjects = x$subjects)

  expect_equal(result$protocol, c("A", "B"))
  expect_equal(result$urine_urea_loss, c(200, 300) * 300 / 1000)
  expect_equal(result$sweat_urea_loss, sweat$sweat_loss * 10)
})

test_that("calc_protein_oxidation keeps the protocol column when asked", {
  urea_loss <- data.frame(id = c(1, 1), protocol = c("A", "B"), urea_mass_loss = c(1, 2))

  result <- calc_protein_oxidation(urea_loss, protocol_col = "protocol")

  expect_equal(names(result), c("id", "protocol", "total_protein_ox", "protein_ox"))
  expect_equal(result$total_protein_ox, c(1, 2) * stoich_coefficients$protein_urea)
})

test_that("calc_urea_loss joins sweat loss on IDs with different names", {
  subjects <- SubjectData(
    data = data.frame(subject = c(1, 2), initial_body_mass = 70, final_body_mass = c(69, 68),
                      water_ingested = 500, saline = 0, urine = c(100, 200)),
    id_col = "subject"
  )
  env <- EnvironmentData(data = data.frame(id = c(1, 2), temperature = 20, humidity = 50, pressure = 760))
  calo <- CalorimetryData(data = data.frame(id = c(1, 1, 2, 2), time = c(30, 60, 30, 60),
                                            vo2 = 2, vco2 = 1.8))
  sweat <- calc_sweat_loss(subjects, calo, env)
  urea <- UreaData(data = data.frame(id = c(1, 2), sweat_urea = 10, urine_urea = 300))

  result <- calc_urea_loss(urea, sweat, subjects = subjects)

  expect_equal(nrow(result), 2)
  expect_equal(result$sweat_urea_loss, sweat$sweat_loss * 10)
})

test_that("calc_urea_loss uses the subject urine when both tables have one", {
  x <- create_sweat_inputs()
  sweat <- calc_sweat_loss(x$subjects, x$calo, x$environment)
  urea <- UreaData(data = data.frame(id = 1, sweat_urea = 10, urine_urea = 300, urine_vol = 999))

  result <- calc_urea_loss(urea, sweat, subjects = x$subjects)

  expect_equal(result$urine_urea_loss, c(200, 300) * 300 / 1000)
})

test_that("analyze_oxidation removes urine held only in the urea data", {
  base <- data.frame(id = c(1, 2), initial_body_mass = 70, final_body_mass = c(69, 68.5),
                     water_ingested = 500, saline = 0)
  env <- EnvironmentData(data = data.frame(id = c(1, 2), temperature = 20, humidity = 50, pressure = 760))
  calo <- CalorimetryData(data = data.frame(id = c(1, 1, 2, 2), time = c(30, 60, 30, 60),
                                            vo2 = 2.5, vco2 = 2.3))
  urea <- UreaData(data = data.frame(id = c(1, 2), sweat_urea = 10, urine_urea = 300,
                                     urine_vol = c(100, 300)))
  study <- OxidationStudy(calorimetry = calo, isotopes = NULL, urea = urea,
                          environment = env, subjects = SubjectData(data = base))

  res <- suppressMessages(analyze_oxidation(study, validate = FALSE))

  with_urine <- SubjectData(data = cbind(base, urine = c(100, 300)))
  sweat <- calc_sweat_loss(with_urine, calo, env)
  expected <- calc_protein_oxidation(calc_urea_loss(urea, sweat, with_urine))
  got <- unique(res@oxidation_rates[c("id", "protein_ox")])
  expect_equal(got$protein_ox, expected$protein_ox)
})

test_that("analyze_oxidation protein oxidation follows the sweat constants", {
  base <- data.frame(id = c(1, 2), initial_body_mass = 70, final_body_mass = c(69, 68.5),
                     water_ingested = 500, saline = 0, urine = c(100, 300))
  env <- EnvironmentData(data = data.frame(id = c(1, 2), temperature = 20, humidity = 50, pressure = 760))
  calo <- CalorimetryData(data = data.frame(id = c(1, 1, 2, 2), time = c(30, 60, 30, 60),
                                            vo2 = 2.5, vco2 = 2.3))
  urea <- UreaData(data = data.frame(id = c(1, 2), sweat_urea = 10, urine_urea = 300))
  subjects <- SubjectData(data = base)
  study <- OxidationStudy(calorimetry = calo, isotopes = NULL, urea = urea,
                          environment = env, subjects = subjects)

  res <- suppressMessages(analyze_oxidation(study, validate = FALSE, uncertainty = NULL))
  vol <- oxidation_constants(molar_volume = "ideal_22.4")
  res_vol <- suppressMessages(analyze_oxidation(study, validate = FALSE, uncertainty = NULL,
                                                constants = vol))
  mass <- oxidation_constants()
  mass$molar_mass <- c(O2 = 32, CO2 = 44, H2O = 18)
  res_mass <- suppressMessages(analyze_oxidation(study, validate = FALSE, uncertainty = NULL,
                                                 constants = mass))

  protein_of <- function(result) unique(result@oxidation_rates[c("id", "protein_ox")])$protein_ox
  expect_false(isTRUE(all.equal(protein_of(res), protein_of(res_vol))))
  expect_false(isTRUE(all.equal(protein_of(res), protein_of(res_mass))))

  sweat <- calc_sweat_loss(subjects, calo, env, constants = mass)
  expected <- calc_protein_oxidation(
    calc_urea_loss(urea, sweat, subjects),
    conversion_factor = mass$protein_urea
  )
  expect_equal(protein_of(res_mass), expected$protein_ox)
})
