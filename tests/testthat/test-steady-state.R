# Tests for steady-state detection functions

# -----------------------------------------------------------------------------
# Test Data Setup
# -----------------------------------------------------------------------------

create_steady_state_data <- function() {
  # Create data with clear steady and non-steady regions
  set.seed(42)

  # Subject 1: Steady throughout
  s1 <- data.frame(
    id = 1,
    time = seq(0, 60, by = 5),
    vo2 = 2.5 + rnorm(13, 0, 0.05),  # Low variability
    vco2 = 2.3 + rnorm(13, 0, 0.05)
  )

  # Subject 2: Initial unsteady, then steady
  s2 <- data.frame(
    id = 2,
    time = seq(0, 60, by = 5),
    vo2 = c(1.5, 2.0, 2.3, 2.5, 2.5 + rnorm(9, 0, 0.05)),
    vco2 = c(1.3, 1.8, 2.1, 2.3, 2.3 + rnorm(9, 0, 0.05))
  )

  # Subject 3: Variable throughout
  s3 <- data.frame(
    id = 3,
    time = seq(0, 60, by = 5),
    vo2 = 2.5 + rnorm(13, 0, 0.3),   # High variability
    vco2 = 2.3 + rnorm(13, 0, 0.3)
  )

  rbind(s1, s2, s3)
}

create_test_calorimetry_for_ss <- function() {
  df <- create_steady_state_data()
  CalorimetryData(data = df)
}

# -----------------------------------------------------------------------------
# detect_steady_state_cv Tests
# -----------------------------------------------------------------------------

test_that("detect_steady_state_cv returns correct structure", {
  df <- create_steady_state_data()
  result <- detect_steady_state_cv(df, variable = "vo2")

  expect_s3_class(result, "tbl_df")
  expect_true("cv" %in% names(result))
  expect_true("is_steady" %in% names(result))
  expect_true("steady_period" %in% names(result))
  expect_true("meets_duration" %in% names(result))
})
test_that("detect_steady_state_cv works with CalorimetryData", {
  calo <- create_test_calorimetry_for_ss()
  result <- detect_steady_state_cv(calo, variable = "vo2")

  expect_s3_class(result, "tbl_df")
  expect_true("cv" %in% names(result))
})

test_that("detect_steady_state_cv calculates CV correctly", {
  df <- create_steady_state_data()
  result <- detect_steady_state_cv(df, variable = "vo2", window_size = 3)

  # CV should be numeric and non-negative
  expect_type(result$cv, "double")
  expect_true(all(result$cv >= 0, na.rm = TRUE))

  # First values should be NA (incomplete window)
  expect_true(is.na(result$cv[1]))
})

test_that("detect_steady_state_cv respects cv_threshold", {
  df <- create_steady_state_data()

  # Strict threshold
  result_strict <- detect_steady_state_cv(df, variable = "vo2", cv_threshold = 0.01)
  # Lenient threshold
  result_lenient <- detect_steady_state_cv(df, variable = "vo2", cv_threshold = 0.50)

  # Lenient should have more steady points
  n_strict <- sum(result_strict$is_steady, na.rm = TRUE)
  n_lenient <- sum(result_lenient$is_steady, na.rm = TRUE)
  expect_gte(n_lenient, n_strict)
})

test_that("detect_steady_state_cv validates inputs", {
  df <- create_steady_state_data()

  # Invalid variable
  expect_error(
    detect_steady_state_cv(df, variable = "nonexistent"),
    "not found"
  )

  # Invalid cv_threshold
  expect_error(
    detect_steady_state_cv(df, variable = "vo2", cv_threshold = 2.0),
    "cv_threshold"
  )

  # Invalid window_size
  expect_error(
    detect_steady_state_cv(df, variable = "vo2", window_size = 1),
    "window_size"
  )
})

test_that("detect_steady_state_cv stores attributes", {
  df <- create_steady_state_data()
  result <- detect_steady_state_cv(df, variable = "vo2", cv_threshold = 0.10, window_size = 3)

  expect_equal(attr(result, "method"), "cv")
  expect_equal(attr(result, "variable"), "vo2")
  expect_equal(attr(result, "cv_threshold"), 0.10)
  expect_equal(attr(result, "window_size"), 3)
})

# -----------------------------------------------------------------------------
# detect_steady_state_variance Tests
# -----------------------------------------------------------------------------

test_that("detect_steady_state_variance returns correct structure", {
  df <- create_steady_state_data()
  result <- detect_steady_state_variance(df, variable = "vo2")

  expect_s3_class(result, "tbl_df")
  expect_true("variance" %in% names(result))
  expect_true("variance_threshold" %in% names(result))
  expect_true("is_steady" %in% names(result))
  expect_true("steady_period" %in% names(result))
})

test_that("detect_steady_state_variance auto-determines threshold", {
  df <- create_steady_state_data()

  # Should not error and should inform about threshold
  expect_message(
    result <- detect_steady_state_variance(df, variable = "vo2"),
    "Auto-determined"
  )

  # Threshold should be stored
  expect_true(!is.null(attr(result, "variance_threshold")))
})

test_that("detect_steady_state_variance accepts custom threshold", {
  df <- create_steady_state_data()
  result <- detect_steady_state_variance(df, variable = "vo2", variance_threshold = 0.001)

  expect_equal(attr(result, "variance_threshold"), 0.001)
})

test_that("detect_steady_state_variance works with CalorimetryData", {
  calo <- create_test_calorimetry_for_ss()
  result <- suppressMessages(
    detect_steady_state_variance(calo, variable = "vo2")
  )

  expect_s3_class(result, "tbl_df")
})

# -----------------------------------------------------------------------------
# detect_steady_state (Combined) Tests
# -----------------------------------------------------------------------------

test_that("detect_steady_state returns correct structure", {
  df <- create_steady_state_data()
  result <- suppressMessages(
    detect_steady_state(df, variables = "vo2", verbose = FALSE)
  )

  expect_s3_class(result, "tbl_df")
  expect_true("is_steady_all" %in% names(result))
  expect_true("method" %in% names(result))
})

test_that("detect_steady_state handles multiple variables", {
  df <- create_steady_state_data()
  result <- suppressMessages(
    detect_steady_state(df, variables = c("vo2", "vco2"), verbose = FALSE)
  )

  expect_true("vo2_steady" %in% names(result) || "vo2_steady_cv" %in% names(result))
  expect_true("vco2_steady" %in% names(result) || "vco2_steady_cv" %in% names(result))
})

test_that("detect_steady_state method='cv' uses only CV", {
  df <- create_steady_state_data()
  result <- detect_steady_state(df, variables = "vo2", method = "cv", verbose = FALSE)

  expect_equal(result$method[1], "cv")
  # Should have CV columns
  expect_true("vo2_cv" %in% names(result))
})

test_that("detect_steady_state method='variance' uses only variance", {
  df <- create_steady_state_data()
  result <- suppressMessages(
    detect_steady_state(df, variables = "vo2", method = "variance", verbose = FALSE)
  )

  expect_equal(result$method[1], "variance")
  expect_true("vo2_variance" %in% names(result))
})

test_that("detect_steady_state method='both' uses both methods", {
  df <- create_steady_state_data()
  result <- suppressMessages(
    detect_steady_state(df, variables = "vo2", method = "both", verbose = FALSE)
  )

  expect_equal(result$method[1], "both")
  expect_true("vo2_cv" %in% names(result))
  expect_true("vo2_variance" %in% names(result))
})

test_that("detect_steady_state combine_method='and' is more strict", {
  df <- create_steady_state_data()

  result_and <- suppressMessages(
    detect_steady_state(df, variables = "vo2", method = "both",
                        combine_method = "and", verbose = FALSE)
  )
  result_or <- suppressMessages(
    detect_steady_state(df, variables = "vo2", method = "both",
                        combine_method = "or", verbose = FALSE)
  )

  # "and" should have fewer steady points than "or"
  n_and <- sum(result_and$is_steady_all, na.rm = TRUE)
  n_or <- sum(result_or$is_steady_all, na.rm = TRUE)
  expect_lte(n_and, n_or)
})

test_that("detect_steady_state validates variables exist", {
  df <- create_steady_state_data()

  expect_error(
    detect_steady_state(df, variables = "nonexistent"),
    "not found"
  )
})

test_that("detect_steady_state stores attributes", {
  df <- create_steady_state_data()
  result <- suppressMessages(
    detect_steady_state(df, variables = "vo2", cv_threshold = 0.08,
                        method = "both", verbose = FALSE)
  )

  expect_equal(attr(result, "method"), "both")
  expect_equal(attr(result, "cv_threshold"), 0.08)
  expect_true("vo2" %in% attr(result, "variables"))
})

# -----------------------------------------------------------------------------
# Helper Function Tests
# -----------------------------------------------------------------------------

test_that("summarize_steady_state returns period summary", {
  df <- create_steady_state_data()
  ss_result <- detect_steady_state_cv(df, variable = "vo2")

  summary <- summarize_steady_state(ss_result)

  expect_s3_class(summary, "tbl_df")
  if (nrow(summary) > 0) {
    expect_true("start_time" %in% names(summary))
    expect_true("end_time" %in% names(summary))
    expect_true("duration_points" %in% names(summary))
  }
})

test_that("filter_steady_state filters to steady points", {
  df <- create_steady_state_data()
  ss_result <- detect_steady_state_cv(df, variable = "vo2")

  filtered <- filter_steady_state(df, ss_result)

  # Should have fewer or equal rows
  expect_lte(nrow(filtered), nrow(df))

  # All remaining points should be in steady state
  # (assuming any steady state was detected)
  if (nrow(filtered) > 0 && any(ss_result$meets_duration, na.rm = TRUE)) {
    expect_true(all(
      filtered[[ss_result[[1, "id"]]]] %in%
        ss_result$id[ss_result$meets_duration]
    ))
  }
})

test_that("calc_steady_state_stats calculates statistics", {
  df <- create_steady_state_data()
  ss_result <- detect_steady_state_cv(df, variable = "vo2")

  stats <- calc_steady_state_stats(df, ss_result, variables = "vo2")

  if (nrow(stats) > 0) {
    expect_s3_class(stats, "tbl_df")
    expect_true("vo2_mean" %in% names(stats))
    expect_true("vo2_sd" %in% names(stats))
  }
})

# -----------------------------------------------------------------------------
# Integration with CalorimetryData Tests
# -----------------------------------------------------------------------------

test_that("detect_steady_state extracts column names from CalorimetryData", {
  calo <- create_test_calorimetry_for_ss()

  result <- suppressMessages(
    detect_steady_state(calo, variables = "vo2", verbose = FALSE)
  )

  # Should use correct id_col and time_col from CalorimetryData
  expect_true("id" %in% names(result))
  expect_true("time" %in% names(result))
})

test_that("filter_steady_state works with CalorimetryData", {
  calo <- create_test_calorimetry_for_ss()
  ss_result <- detect_steady_state_cv(calo, variable = "vo2")

  filtered <- filter_steady_state(calo, ss_result)

  expect_s3_class(filtered, "data.frame")
  expect_lte(nrow(filtered), nrow(calo@data))
})
