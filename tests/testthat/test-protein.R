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

  # Default factor is 2.915 g protein per g urea
  expected_total <- 1.0 * 2.915
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

  # Default factors: vo2 = 1.0075, vco2 = 0.8443
  expect_equal(result$vo2_protein, 0.1 * 1.0075)
  expect_equal(result$vco2_protein, 0.1 * 0.8443)
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
  expect_equal(result$vo2_protein, protein_values * 1.0075)
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
