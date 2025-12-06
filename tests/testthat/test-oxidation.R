# Tests for oxidation calculations

test_that("calc_substrate_oxidation works correctly", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    time = c(30, 60, 30, 60),
    vo2 = c(2.5, 2.6, 2.4, 2.5),
    vco2 = c(2.3, 2.4, 2.2, 2.3)
  )

  calo <- CalorimetryData(data = df)
  ox <- calc_substrate_oxidation(calo)

  expect_true("cho_total" %in% names(ox))
  expect_true("fat_total" %in% names(ox))
  expect_true("rer" %in% names(ox))

  # CHO and fat should be non-negative
  expect_true(all(ox$cho_total >= 0))
  expect_true(all(ox$fat_total >= 0))

  # RER should be VCO2/VO2
  expected_rer <- df$vco2 / df$vo2
  expect_equal(ox$rer, expected_rer, tolerance = 0.001)
})

test_that("calc_substrate_oxidation handles mL/min units", {
  df <- data.frame(
    id = c(1, 2),
    time = c(30, 60),
    vo2 = c(2500, 2600),  # mL/min
    vco2 = c(2300, 2400)
  )

  calo <- CalorimetryData(data = df, vo2_unit = "mL/min")
  ox <- calc_substrate_oxidation(calo)

  # Results should be similar to L/min version (within reasonable tolerance)
  expect_true(all(ox$cho_total > 0))
  expect_true(all(ox$fat_total >= 0))
})

test_that("calc_rer works correctly", {
  df <- data.frame(
    id = 1:3,
    time = c(30, 60, 90),
    vo2 = c(2.5, 2.6, 2.7),
    vco2 = c(2.25, 2.34, 2.43)
  )

  calo <- CalorimetryData(data = df)
  rer <- calc_rer(calo)

  expected <- df$vco2 / df$vo2
  expect_equal(rer, expected)
})

test_that("normalize_gas_units converts correctly", {
  df <- data.frame(
    id = 1:2,
    time = c(30, 60),
    vo2 = c(2.5, 2.6),
    vco2 = c(2.3, 2.4)
  )

  calo <- CalorimetryData(data = df, vo2_unit = "L/min")
  calo_ml <- normalize_gas_units(calo, to_unit = "mL/min")

  expect_equal(calo_ml@vo2_unit, "mL/min")
  expect_equal(calo_ml@data$vo2, df$vo2 * 1000)
  expect_equal(calo_ml@data$vco2, df$vco2 * 1000)
})

test_that("filter_time_range filters correctly", {
  df <- data.frame(
    id = rep(1, 5),
    time = c(0, 30, 60, 90, 120),
    vo2 = c(2.0, 2.5, 2.6, 2.5, 2.4),
    vco2 = c(1.8, 2.3, 2.4, 2.3, 2.2)
  )

  calo <- CalorimetryData(data = df)
  filtered <- filter_time_range(calo, c(30, 90))

  expect_equal(nrow(filtered@data), 3)
  expect_equal(filtered@data$time, c(30, 60, 90))
})
