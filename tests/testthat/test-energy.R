# Tests for energy calculations

test_that("calc_energy_yield calculates correctly", {
  ox <- data.frame(
    id = 1:2,
    cho_total = c(2.0, 2.5),
    fat_total = c(0.5, 0.4),
    protein_ox = c(0.1, 0.12)
  )

  energy <- calc_energy_yield(ox)

  # Check energy columns created
  expect_true("e_cho_total" %in% names(energy))
  expect_true("e_fat" %in% names(energy))
  expect_true("e_protein" %in% names(energy))
  expect_true("e_total" %in% names(energy))

  # Check calculations
  expect_equal(energy$e_cho_total, ox$cho_total * 3.72)
  expect_equal(energy$e_fat, ox$fat_total * 9.44)
  expect_equal(energy$e_protein, ox$protein_ox * 4.70)
})

test_that("calc_energy_yield uses custom factors", {
  ox <- data.frame(
    id = 1,
    cho_total = 2.0,
    fat_total = 0.5,
    protein_ox = 0.1
  )

  energy <- calc_energy_yield(ox, cho_factor = 4.0, fat_factor = 9.0, protein_factor = 4.0)

  expect_equal(energy$e_cho_total, 2.0 * 4.0)
  expect_equal(energy$e_fat, 0.5 * 9.0)
  expect_equal(energy$e_protein, 0.1 * 4.0)
})

test_that("calc_energy_percent calculates correctly", {
  ox <- data.frame(
    id = 1,
    cho_total = 2.0,
    fat_total = 0.5,
    protein_ox = 0.1
  )

  energy <- calc_energy_yield(ox)
  pct <- calc_energy_percent(energy)

  # Check percentage columns
  expect_true("pct_cho_total" %in% names(pct))
  expect_true("pct_fat" %in% names(pct))
  expect_true("pct_protein" %in% names(pct))

  # Percentages should sum to 100 (within rounding)
  total_pct <- pct$pct_cho_total + pct$pct_fat + pct$pct_protein
  expect_equal(total_pct, 100, tolerance = 0.1)
})

test_that("calc_total_energy uses Weir equation correctly", {
  vo2 <- 2.5  # L/min
  vco2 <- 2.3  # L/min

  # Weir: EE = 3.941 * VO2 + 1.106 * VCO2
  expected <- 3.941 * vo2 + 1.106 * vco2
  result <- calc_total_energy(vo2, vco2, method = "weir")

  expect_equal(result, expected)
})

test_that("calc_total_energy uses Brockway equation correctly", {
  vo2 <- 2.5
  vco2 <- 2.3

  # Brockway: EE = 3.869 * VO2 + 1.195 * VCO2
  expected <- 3.869 * vo2 + 1.195 * vco2
  result <- calc_total_energy(vo2, vco2, method = "brockway")

  expect_equal(result, expected)
})

test_that("summarize_energy works by group", {
  energy <- data.frame(
    protocol = c("A", "A", "B", "B"),
    pct_cho_total = c(60, 62, 55, 57),
    pct_fat = c(35, 33, 40, 38),
    e_total = c(10, 11, 9, 10)
  )

  summary <- summarize_energy(energy, by = "protocol")

  expect_equal(nrow(summary), 2)  # Two protocols
  expect_true("pct_cho_total_mean" %in% names(summary))
  expect_true("pct_cho_total_sd" %in% names(summary))
})
