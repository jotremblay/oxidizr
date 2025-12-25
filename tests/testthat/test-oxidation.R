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

# -----------------------------------------------------------------------------
# aggregate_calorimetry Tests
# -----------------------------------------------------------------------------

test_that("aggregate_calorimetry aggregates by subject and time", {
  # Data with multiple measurements per time point
  df <- data.frame(
    id = rep(1, 6),
    time = rep(c(30, 60), each = 3),
    vo2 = c(2.4, 2.5, 2.6, 2.5, 2.6, 2.7),
    vco2 = c(2.2, 2.3, 2.4, 2.3, 2.4, 2.5)
  )

  calo <- CalorimetryData(data = df)
  agg <- aggregate_calorimetry(calo)

  expect_s3_class(agg, "oxidizr::CalorimetryData")
  expect_equal(nrow(agg@data), 2)  # 2 time points
  expect_equal(agg@data$vo2, c(mean(c(2.4, 2.5, 2.6)), mean(c(2.5, 2.6, 2.7))))
})

test_that("aggregate_calorimetry includes protocol in grouping", {
  df <- data.frame(
    id = rep(1, 4),
    time = c(30, 30, 60, 60),
    protocol = rep(c("CHO", "PLA"), 2),
    vo2 = c(2.5, 2.4, 2.6, 2.3),
    vco2 = c(2.3, 2.2, 2.4, 2.1)
  )

  calo <- CalorimetryData(data = df, protocol_col = "protocol")
  agg <- aggregate_calorimetry(calo)

  expect_equal(nrow(agg@data), 4)  # 2 time points x 2 protocols
})

test_that("aggregate_calorimetry preserves CalorimetryData attributes", {
  df <- data.frame(
    id = rep(1:2, each = 2),
    time = rep(30, 4),
    vo2 = c(2.5, 2.6, 2.4, 2.5),
    vco2 = c(2.3, 2.4, 2.2, 2.3)
  )

  calo <- CalorimetryData(data = df, vo2_unit = "L/min")
  agg <- aggregate_calorimetry(calo)

  expect_equal(agg@vo2_unit, "L/min")
  expect_equal(agg@id_col, "id")
  expect_equal(agg@time_col, "time")
})

test_that("aggregate_calorimetry accepts custom grouping", {
  df <- data.frame(
    id = rep(1:2, each = 4),
    time = rep(c(30, 30, 60, 60), 2),  # Duplicate measurements
    condition = rep(c("A", "B"), 4),
    vo2 = c(2.5, 2.6, 2.5, 2.4, 2.4, 2.5, 2.4, 2.3),
    vco2 = c(2.3, 2.4, 2.3, 2.2, 2.2, 2.3, 2.2, 2.1)
  )

  calo <- CalorimetryData(data = df)

  # Aggregate by id and time (dropping condition)
  agg <- aggregate_calorimetry(calo, by = c("id", "time"))

  expect_equal(nrow(agg@data), 4)  # 2 subjects x 2 time points
})

test_that("aggregate_calorimetry validates column existence", {
  df <- data.frame(
    id = 1:3,
    time = c(30, 60, 90),
    vo2 = c(2.5, 2.6, 2.5),
    vco2 = c(2.3, 2.4, 2.3)
  )

  calo <- CalorimetryData(data = df)

  expect_error(
    aggregate_calorimetry(calo, by = c("id", "nonexistent")),
    "not found"
  )
})

# -----------------------------------------------------------------------------
# calc_cho_partition Tests
# -----------------------------------------------------------------------------

test_that("calc_cho_partition returns correct structure", {
  substrate_ox <- data.frame(
    id = rep(1:2, each = 3),
    time = rep(c(30, 60, 90), 2),
    cho_total = c(2.0, 2.2, 2.1, 1.9, 2.1, 2.0),
    fat_total = c(0.5, 0.4, 0.45, 0.55, 0.45, 0.5)
  )

  cho_exo <- data.frame(
    id = rep(1:2, each = 3),
    time = rep(c(30, 60, 90), 2),
    cho_exo = c(0.3, 0.5, 0.6, 0.25, 0.45, 0.55)
  )

  result <- calc_cho_partition(substrate_ox, cho_exo)

  expect_s3_class(result, "tbl_df")
  expect_true("cho_exo" %in% names(result))
  expect_true("cho_endo" %in% names(result))
})

test_that("calc_cho_partition calculates endogenous correctly", {
  substrate_ox <- data.frame(
    id = 1,
    time = 30,
    cho_total = 2.0,
    fat_total = 0.5
  )

  cho_exo <- data.frame(
    id = 1,
    time = 30,
    cho_exo = 0.5
  )

  result <- calc_cho_partition(substrate_ox, cho_exo)

  # CHOendo = CHOtot - CHOexo
  expect_equal(result$cho_endo, 1.5)
})

test_that("calc_cho_partition ensures non-negative endogenous", {
  substrate_ox <- data.frame(
    id = 1,
    time = 30,
    cho_total = 1.0,
    fat_total = 0.5
  )

  # Exogenous greater than total (edge case)
  cho_exo <- data.frame(
    id = 1,
    time = 30,
    cho_exo = 1.5
  )

  result <- calc_cho_partition(substrate_ox, cho_exo)

  # Should be clamped to 0
  expect_equal(result$cho_endo, 0)
})

test_that("calc_cho_partition calculates muscle and liver when plasma available", {
  substrate_ox <- data.frame(
    id = 1,
    time = 30,
    cho_total = 2.0,
    fat_total = 0.5
  )

  cho_exo <- data.frame(
    id = 1,
    time = 30,
    cho_exo = 0.5
  )

  cho_pla <- data.frame(
    id = 1,
    time = 30,
    cho_pla = 1.2
  )

  result <- calc_cho_partition(substrate_ox, cho_exo, cho_pla)

  expect_true("cho_mus" %in% names(result))
  expect_true("cho_liv" %in% names(result))

  # CHOmus = CHOtot - CHOpla
  expect_equal(result$cho_mus, 0.8)

  # CHOliv = CHOpla - CHOexo
  expect_equal(result$cho_liv, 0.7)
})

test_that("calc_cho_partition ensures non-negative muscle and liver", {
  substrate_ox <- data.frame(
    id = 1,
    time = 30,
    cho_total = 1.0,
    fat_total = 0.5
  )

  # Edge case: plasma > total
  cho_exo <- data.frame(
    id = 1,
    time = 30,
    cho_exo = 0.5
  )

  cho_pla <- data.frame(
    id = 1,
    time = 30,
    cho_pla = 1.5
  )

  result <- calc_cho_partition(substrate_ox, cho_exo, cho_pla)

  # Muscle should be clamped to 0
  expect_equal(result$cho_mus, 0)
})
