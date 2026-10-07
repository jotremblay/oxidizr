# Tests for the stoichiometric constants

test_that("gas_yield reproduces Péronnet & Massicotte (1991), Table 1", {
  pm <- c(O2 = 22.3858, CO2 = 22.2966)
  expect_equal(round(gas_yield(c(C = 6, H = 12, O = 6), pm), 4),
               c(o2 = 0.7455, co2 = 0.7426))
  expect_equal(round(gas_yield(c(C = 17.2702, H = 32.7142, O = 2), pm), 4),
               c(o2 = 2.0092, co2 = 1.4136))
})

test_that("gas_yield gives the TAG-type values of Telmosse (2022)", {
  # The thesis uses 22.4 L/mol and 836 g/mol: 2.015 L O2/g and 1.431 L CO2/g
  tag <- gas_yield(c(C = 53.4, H = 99.2, O = 6), molar_volume = 22.4)
  expect_equal(unname(tag), c(2.015, 1.431), tolerance = 2e-3)
})

test_that("gas_yield rejects atoms other than C, H and O", {
  expect_error(gas_yield(c(C = 1, N = 1)), "C, H and O")
})

test_that("stoich_coefficients invert the glucose and TAG-type reactions", {
  k <- stoich_coefficients
  glc <- gas_yield(c(C = 6, H = 12, O = 6))
  tag <- gas_yield(c(C = 53.4, H = 99.2, O = 6))
  cho <- function(g) k$cho_vo2 * g[["o2"]] + k$cho_vco2 * g[["co2"]]
  fat <- function(g) k$fat_vo2 * g[["o2"]] + k$fat_vco2 * g[["co2"]]

  expect_equal(c(cho(glc), fat(glc)), c(1, 0))
  expect_equal(c(cho(tag), fat(tag)), c(0, 1))
  expect_equal(k$co2_cho_ratio, glc[["co2"]])
})

test_that("stoich_coefficients stay close to Telmosse (2022), Eq. 8 and 10", {
  k <- stoich_coefficients
  expect_equal(c(k$cho_vco2, k$cho_vo2, k$fat_vo2, k$fat_vco2),
               c(4.618, -3.279, 1.712, -1.712), tolerance = 2e-3)
})

test_that("protein factors follow Livesey & Elia (1988), Table 6", {
  k <- stoich_coefficients
  expect_equal(k$protein_vo2, 19.68 / 19.48)
  expect_equal(k$protein_vco2 / k$protein_vo2, 0.835)
  expect_equal(k$protein_urea, 2.915, tolerance = 1e-3)
})

test_that("energy_factors match IOM (2005), Table 5-1", {
  expect_equal(energy_factors, list(cho = 3.72, fat = 9.44, protein = 4.70))
})
