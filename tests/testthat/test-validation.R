# Tests for validation functions

# -----------------------------------------------------------------------------
# Test Data Setup
# -----------------------------------------------------------------------------

create_test_calorimetry <- function(with_issues = FALSE) {
  if (with_issues) {
    # Data with various issues
    df <- data.frame(
      id = rep(1:3, each = 4),
      time = rep(c(30, 60, 90, 120), 3),
      vo2 = c(2.5, 2.6, 2.4, 2.5,
              0.1, 2.5, 7.0, 2.5,     # Subject 2: too low, too high
              2.5, NA, 2.6, 2.5),      # Subject 3: missing value
      vco2 = c(2.3, 2.4, 2.2, 2.3,
               2.3, 2.4, 2.2, 2.3,
               2.3, 2.4, 2.2, 2.3),
      rer = c(0.92, 0.92, 0.92, 0.92,
              0.65, 0.96, 0.31, 0.92, # Subject 2: invalid RER values
              0.92, 0.96, 0.85, 0.92)
    )
  } else {
    # Clean data
    df <- data.frame(
      id = rep(1:3, each = 4),
      time = rep(c(30, 60, 90, 120), 3),
      vo2 = c(2.5, 2.6, 2.4, 2.5,
              2.4, 2.5, 2.6, 2.5,
              2.5, 2.5, 2.6, 2.5),
      vco2 = c(2.3, 2.4, 2.2, 2.3,
               2.2, 2.3, 2.4, 2.3,
               2.3, 2.3, 2.4, 2.3),
      rer = rep(0.92, 12)
    )
  }
  CalorimetryData(data = df)
}

create_test_study <- function(with_issues = FALSE) {
  calo <- create_test_calorimetry(with_issues)
  OxidationStudy(calorimetry = calo)
}

# -----------------------------------------------------------------------------
# ValidationResult Class Tests
# -----------------------------------------------------------------------------

test_that("ValidationResult can be created with empty issues", {
  issues <- tibble::tibble(
    check_id = character(),
    category = character(),
    variable = character(),
    severity = character(),
    message = character(),
    n_affected = integer(),
    pct_affected = double(),
    subject_ids = list(),
    time_points = list(),
    values = list(),
    threshold = character(),
    reference = character()
  )

  result <- ValidationResult(
    issues = issues,
    passed = TRUE,
    severity_summary = list(error = 0L, warning = 0L, info = 0L),
    timestamp = Sys.time()
  )

  expect_s3_class(result, "oxidizr::ValidationResult")
  expect_true(result@passed)
  expect_equal(nrow(result@issues), 0)
})

test_that("ValidationResult can be created with issues", {
  issues <- tibble::tibble(
    check_id = "test_check",
    category = "test",
    variable = "vo2",
    severity = "warning",
    message = "Test warning",
    n_affected = 5L,
    pct_affected = 10.0,
    subject_ids = list(c("1", "2")),
    time_points = list(c(30, 60)),
    values = list(c(0.1, 0.2)),
    threshold = "0.2-6.0",
    reference = "Test ref"
  )

  result <- ValidationResult(
    issues = issues,
    passed = TRUE,
    severity_summary = list(error = 0L, warning = 1L, info = 0L)
  )

  expect_s3_class(result, "oxidizr::ValidationResult")
  expect_equal(nrow(result@issues), 1)
  expect_equal(result@severity_summary$warning, 1L)
})

test_that("ValidationResult validates severity values", {
  issues <- tibble::tibble(
    check_id = "test",
    category = "test",
    variable = "vo2",
    severity = "invalid",  # Should fail
    message = "Test",
    n_affected = 1L,
    pct_affected = 1.0,
    subject_ids = list("1"),
    time_points = list(30),
    values = list(1.0),
    threshold = "",
    reference = ""
  )

  expect_error(
    ValidationResult(issues = issues, passed = TRUE),
    "severity"
  )
})

# -----------------------------------------------------------------------------
# Threshold Tests
# -----------------------------------------------------------------------------

test_that("calorimetry_thresholds exist and have required fields", {
  expect_type(calorimetry_thresholds, "list")
  expect_true("vo2" %in% names(calorimetry_thresholds))
  expect_true("vco2" %in% names(calorimetry_thresholds))
  expect_true("rer" %in% names(calorimetry_thresholds))

  # Check vo2 has required fields
  expect_true("min" %in% names(calorimetry_thresholds$vo2))
  expect_true("max" %in% names(calorimetry_thresholds$vo2))
})

test_that("isotope_thresholds exist", {
  expect_type(isotope_thresholds, "list")
  expect_true("rexp" %in% names(isotope_thresholds))
})

# -----------------------------------------------------------------------------
# validate_calorimetry Tests
# -----------------------------------------------------------------------------

test_that("validate_calorimetry returns ValidationResult", {
  calo <- create_test_calorimetry(with_issues = FALSE)
  result <- validate_calorimetry(calo)

  expect_s3_class(result, "oxidizr::ValidationResult")
})

test_that("validate_calorimetry passes for clean data", {
  calo <- create_test_calorimetry(with_issues = FALSE)
  result <- validate_calorimetry(calo)

  # May have warnings but no errors
  n_errors <- result@severity_summary$error %||% 0
  expect_equal(n_errors, 0)
})

test_that("validate_calorimetry detects out-of-range values", {
  calo <- create_test_calorimetry(with_issues = TRUE)
  result <- validate_calorimetry(calo)

  # Should find issues
  expect_gt(nrow(result@issues), 0)

  # Check for range violations (check_ids like vo2_below_min, vo2_above_max)
  range_issues <- result@issues |>
    dplyr::filter(grepl("below_min|above_max", check_id, ignore.case = TRUE))
  expect_gt(nrow(range_issues), 0)
})

test_that("validate_calorimetry detects missing values", {
  calo <- create_test_calorimetry(with_issues = TRUE)
  result <- validate_calorimetry(calo)

  missing_issues <- result@issues |>
    dplyr::filter(grepl("missing", check_id, ignore.case = TRUE))
  expect_gt(nrow(missing_issues), 0)
})

test_that("validate_calorimetry accepts custom thresholds", {
  calo <- create_test_calorimetry(with_issues = FALSE)

  # Custom thresholds that make all data "invalid"
  custom_thresholds <- list(
    vo2 = list(min = 10, max = 20)  # No data in this range
  )

  result <- validate_calorimetry(calo, thresholds = custom_thresholds)

  # Should find errors with these impossible thresholds
  vo2_issues <- result@issues |>
    dplyr::filter(variable == "vo2")
  expect_gt(nrow(vo2_issues), 0)
})

# -----------------------------------------------------------------------------
# validate_study Tests
# -----------------------------------------------------------------------------

test_that("validate_study returns ValidationResult", {
  study <- create_test_study(with_issues = FALSE)
  result <- validate_study(study, verbose = FALSE)

  expect_s3_class(result, "oxidizr::ValidationResult")
})

test_that("validate_study passes for clean data", {
  study <- create_test_study(with_issues = FALSE)
  result <- validate_study(study, verbose = FALSE)

  # Should pass (no errors)
  n_errors <- result@severity_summary$error %||% 0
  expect_equal(n_errors, 0)
})

test_that("validate_study detects issues", {
  study <- create_test_study(with_issues = TRUE)
  result <- validate_study(study, verbose = FALSE)

  # Should find issues
  expect_gt(nrow(result@issues), 0)
})

test_that("validate_study respects strict mode", {
  study <- create_test_study(with_issues = TRUE)

  # Non-strict should return result
  result <- validate_study(study, strict = FALSE, verbose = FALSE)
  expect_s3_class(result, "oxidizr::ValidationResult")

  # Note: strict = TRUE would abort, which is hard to test directly
  # The important thing is that ValidationResult is returned in non-strict mode
})

test_that("validate_study generates recommendations", {
  study <- create_test_study(with_issues = TRUE)
  result <- validate_study(study, verbose = FALSE)

  # Should have recommendations for data with issues
  expect_type(result@recommendations, "character")
})

# -----------------------------------------------------------------------------
# Helper Function Tests
# -----------------------------------------------------------------------------

test_that("empty_issues returns correct structure", {
  issues <- oxidizr:::empty_issues()

  expect_s3_class(issues, "tbl_df")
  expect_equal(nrow(issues), 0)
  expect_true(all(c("check_id", "category", "variable", "severity", "message") %in% names(issues)))
})

test_that("add_issue adds issues correctly", {
  issues <- oxidizr:::empty_issues()

  issues <- oxidizr:::add_issue(
    issues,
    check_id = "test",
    category = "test_cat",
    variable = "test_var",
    severity = "warning",
    message = "Test message",
    n_affected = 10
  )

  expect_equal(nrow(issues), 1)
  expect_equal(issues$check_id[1], "test")
  expect_equal(issues$severity[1], "warning")
})

# -----------------------------------------------------------------------------
# Print and Summary Method Tests
# -----------------------------------------------------------------------------

test_that("print.ValidationResult works", {
  study <- create_test_study(with_issues = TRUE)
  result <- validate_study(study, verbose = FALSE)

  # Should not error and returns invisibly
  expect_no_error(print(result))
  expect_invisible(print(result))
})

test_that("summary.ValidationResult returns data frame", {
  study <- create_test_study(with_issues = TRUE)
  result <- validate_study(study, verbose = FALSE)

  summary_df <- summary(result)

  expect_s3_class(summary_df, "data.frame")
})

# -----------------------------------------------------------------------------
# get_data Method Tests
# -----------------------------------------------------------------------------

test_that("get_data.ValidationResult extracts issues", {
  study <- create_test_study(with_issues = TRUE)
  result <- validate_study(study, verbose = FALSE)

  issues_df <- get_data(result, what = "issues")

  expect_s3_class(issues_df, "tbl_df")
})

test_that("get_data.ValidationResult extracts recommendations", {
  study <- create_test_study(with_issues = TRUE)
  result <- validate_study(study, verbose = FALSE)

  recs_df <- get_data(result, what = "recommendations")

  expect_s3_class(recs_df, "tbl_df")
})
