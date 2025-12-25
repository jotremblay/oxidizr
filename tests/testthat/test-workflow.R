# Tests for analyze_oxidation main workflow

# -----------------------------------------------------------------------------
# Test Data Setup
# -----------------------------------------------------------------------------

create_simple_calorimetry <- function() {
  df <- data.frame(
    id = rep(1:2, each = 4),
    time = rep(c(30, 60, 90, 120), 2),
    vo2 = c(2.5, 2.6, 2.5, 2.4, 2.4, 2.5, 2.4, 2.3),
    vco2 = c(2.3, 2.4, 2.3, 2.2, 2.2, 2.3, 2.2, 2.1)
  )
  CalorimetryData(data = df)
}

# Note: Explicitly set optional slots to NULL to avoid S7 default object behavior
create_test_study_simple <- function() {
  calo <- create_simple_calorimetry()
  OxidationStudy(
    calorimetry = calo,
    isotopes = NULL,
    urea = NULL,
    environment = NULL,
    subjects = NULL
  )
}

# -----------------------------------------------------------------------------
# oxidation_study Helper Tests
# -----------------------------------------------------------------------------

test_that("oxidation_study creates OxidationStudy from CalorimetryData", {
  calo <- create_simple_calorimetry()
  study <- oxidation_study(calorimetry = calo)

  expect_s3_class(study, "oxidizr::OxidationStudy")
  expect_s3_class(study@calorimetry, "oxidizr::CalorimetryData")
})

test_that("oxidation_study creates OxidationStudy from data frame", {
  df <- data.frame(
    id = 1:3,
    time = c(30, 60, 90),
    vo2 = c(2.5, 2.6, 2.5),
    vco2 = c(2.3, 2.4, 2.3)
  )

  study <- oxidation_study(calorimetry = df)

  expect_s3_class(study, "oxidizr::OxidationStudy")
  expect_s3_class(study@calorimetry, "oxidizr::CalorimetryData")
})

test_that("oxidation_study handles protocol factor", {
  calo <- create_simple_calorimetry()
  protocols <- c("CHO", "PLA")

  study <- oxidation_study(calorimetry = calo, protocols = protocols)

  expect_true(is.factor(study@protocols))
  expect_equal(levels(study@protocols), protocols)
})

# -----------------------------------------------------------------------------
# analyze_oxidation Basic Tests
# -----------------------------------------------------------------------------

test_that("analyze_oxidation returns OxidationResults", {
  study <- create_test_study_simple()
  results <- suppressMessages(analyze_oxidation(study, validate = FALSE))

  expect_s3_class(results, "oxidizr::OxidationResults")
})

test_that("analyze_oxidation calculates substrate oxidation", {
  study <- create_test_study_simple()
  results <- suppressMessages(analyze_oxidation(study, validate = FALSE))

  ox_rates <- results@oxidation_rates

  expect_true("cho_total" %in% names(ox_rates))
  expect_true("fat_total" %in% names(ox_rates))
  expect_true("rer" %in% names(ox_rates))
})

test_that("analyze_oxidation calculates energy by default", {
  study <- create_test_study_simple()
  results <- suppressMessages(analyze_oxidation(study, validate = FALSE))

  expect_false(is.null(results@energy_contributions))
  expect_true("e_cho_total" %in% names(results@energy_contributions))
  expect_true("pct_cho_total" %in% names(results@energy_contributions))
})

test_that("analyze_oxidation respects calc_energy = FALSE", {
  study <- create_test_study_simple()
  results <- suppressMessages(
    analyze_oxidation(study, calc_energy = FALSE, validate = FALSE)
  )

  expect_null(results@energy_contributions)
})

test_that("analyze_oxidation stores settings", {
  study <- create_test_study_simple()
  results <- suppressMessages(
    analyze_oxidation(study, time_range = c(30, 90), validate = FALSE)
  )

  expect_equal(results@settings$time_range, c(30, 90))
  expect_false(results@settings$validate)
})

# -----------------------------------------------------------------------------
# analyze_oxidation Time Range Tests
# -----------------------------------------------------------------------------

test_that("analyze_oxidation filters by time_range", {
  study <- create_test_study_simple()
  results <- suppressMessages(
    analyze_oxidation(study, time_range = c(30, 60), validate = FALSE)
  )

  ox_rates <- results@oxidation_rates
  expect_true(all(ox_rates$time >= 30 & ox_rates$time <= 60))
})

test_that("analyze_oxidation handles aggregate option", {
  # Create data with multiple measurements per time point
  df <- data.frame(
    id = rep(1, 6),
    time = rep(c(30, 60), each = 3),
    vo2 = c(2.4, 2.5, 2.6, 2.5, 2.6, 2.7),
    vco2 = c(2.2, 2.3, 2.4, 2.3, 2.4, 2.5)
  )

  calo <- CalorimetryData(data = df)
  study <- OxidationStudy(
    calorimetry = calo,
    isotopes = NULL,
    urea = NULL,
    environment = NULL,
    subjects = NULL
  )

  results <- suppressMessages(
    analyze_oxidation(study, aggregate = TRUE, validate = FALSE)
  )

  # Aggregated data should have 2 rows (2 time points)
  expect_equal(nrow(results@oxidation_rates), 2)
})

# -----------------------------------------------------------------------------
# analyze_oxidation Validation Tests
# -----------------------------------------------------------------------------

test_that("analyze_oxidation with validation returns validation result", {
  study <- create_test_study_simple()
  results <- suppressMessages(
    analyze_oxidation(study, validate = TRUE, strict = FALSE)
  )

  expect_true("validation" %in% names(results@settings))
  expect_s3_class(results@settings$validation, "oxidizr::ValidationResult")
})

test_that("analyze_oxidation stores study reference", {
  study <- create_test_study_simple()
  results <- suppressMessages(analyze_oxidation(study, validate = FALSE))

  expect_s3_class(results@study, "oxidizr::OxidationStudy")
})

# -----------------------------------------------------------------------------
# analyze_oxidation Input Validation Tests
# -----------------------------------------------------------------------------

test_that("analyze_oxidation errors on non-OxidationStudy input", {
  expect_error(
    analyze_oxidation(data.frame(x = 1)),
    "OxidationStudy"
  )
})

test_that("analyze_oxidation warns when isotopes present but no control_protocol", {
  # Create study with isotope data
  calo_df <- data.frame(
    id = rep(1:2, each = 3),
    time = rep(c(30, 60, 90), 2),
    protocol = rep(c("CHO", "PLA"), each = 3),
    vo2 = c(2.5, 2.6, 2.5, 2.4, 2.5, 2.4),
    vco2 = c(2.3, 2.4, 2.3, 2.2, 2.3, 2.2)
  )

  rexp_df <- data.frame(
    id = rep(1:2, each = 3),
    time = rep(c(30, 60, 90), 2),
    protocol = rep(c("CHO", "PLA"), each = 3),
    rexp = c(-22, -18, -14, -26, -26, -26)
  )

  rexo_df <- data.frame(
    protocol = c("CHO", "PLA"),
    rexo = c(100, -26)
  )

  calo <- CalorimetryData(data = calo_df, protocol_col = "protocol")
  iso <- IsotopeData(
    rexp = rexp_df,
    rexo = rexo_df,
    protocol_col = "protocol"
  )

  study <- OxidationStudy(
    calorimetry = calo,
    isotopes = iso,
    urea = NULL,
    environment = NULL,
    subjects = NULL
  )

  # Should warn about missing control_protocol
  expect_warning(
    suppressMessages(analyze_oxidation(study, validate = FALSE)),
    "control_protocol"
  )
})

# -----------------------------------------------------------------------------
# OxidationResults Accessor Tests
# -----------------------------------------------------------------------------

test_that("get_data extracts oxidation_rates from OxidationResults", {
  study <- create_test_study_simple()
  results <- suppressMessages(analyze_oxidation(study, validate = FALSE))

  rates <- get_data(results, what = "rates")

  expect_s3_class(rates, "tbl_df")
  expect_true("cho_total" %in% names(rates))
})

test_that("get_data extracts energy_contributions from OxidationResults", {
  study <- create_test_study_simple()
  results <- suppressMessages(analyze_oxidation(study, validate = FALSE))

  energy <- get_data(results, what = "energy")

  expect_s3_class(energy, "tbl_df")
  expect_true("e_cho_total" %in% names(energy))
})

test_that("summary.OxidationResults works", {
  study <- create_test_study_simple()
  results <- suppressMessages(analyze_oxidation(study, validate = FALSE))

  summary_df <- summary(results)

  expect_s3_class(summary_df, "data.frame")
})

test_that("print.OxidationResults works", {
  study <- create_test_study_simple()
  results <- suppressMessages(analyze_oxidation(study, validate = FALSE))

  expect_no_error(print(results))
  expect_invisible(print(results))
})
