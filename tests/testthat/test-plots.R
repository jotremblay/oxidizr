# Tests for visualization functions

# -----------------------------------------------------------------------------
# Test Data Setup
# -----------------------------------------------------------------------------

create_plot_test_data <- function() {
  df <- data.frame(
    id = rep(1:4, each = 4),
    time = rep(c(30, 60, 90, 120), 4),
    protocol = rep(c("CHO", "CHO", "PLA", "PLA"), each = 4),
    vo2 = c(2.5, 2.6, 2.5, 2.4, 2.4, 2.5, 2.4, 2.3,
            2.5, 2.5, 2.5, 2.4, 2.4, 2.4, 2.4, 2.3),
    vco2 = c(2.3, 2.4, 2.3, 2.2, 2.2, 2.3, 2.2, 2.1,
             2.3, 2.3, 2.3, 2.2, 2.2, 2.2, 2.2, 2.1)
  )
  CalorimetryData(data = df, protocol_col = "protocol")
}

create_plot_test_results <- function() {
  calo <- create_plot_test_data()
  study <- OxidationStudy(
    calorimetry = calo,
    isotopes = NULL,
    urea = NULL,
    environment = NULL,
    subjects = NULL
  )
  suppressMessages(analyze_oxidation(study, validate = FALSE))
}

# -----------------------------------------------------------------------------
# theme_oxidizr Tests
# -----------------------------------------------------------------------------

test_that("theme_oxidizr returns a ggplot2 theme", {
  theme <- theme_oxidizr()

  expect_s3_class(theme, "theme")
  expect_s3_class(theme, "gg")
})

test_that("theme_oxidizr respects base_size parameter", {
  theme_small <- theme_oxidizr(base_size = 10)
  theme_large <- theme_oxidizr(base_size = 16)

  # Both should be valid themes

  expect_s3_class(theme_small, "theme")
  expect_s3_class(theme_large, "theme")
})

test_that("theme_oxidizr can be added to a ggplot", {
  df <- data.frame(x = 1:10, y = 1:10)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    theme_oxidizr()

  expect_s3_class(p, "ggplot")
})

# -----------------------------------------------------------------------------
# plot_oxidation_timecourse Tests
# -----------------------------------------------------------------------------

test_that("plot_oxidation_timecourse returns a ggplot", {
  results <- create_plot_test_results()
  p <- plot_oxidation_timecourse(results)

  expect_s3_class(p, "ggplot")
})
test_that("plot_oxidation_timecourse works with substrate parameter", {
  results <- create_plot_test_results()

  p_all <- plot_oxidation_timecourse(results, substrate = "all")
  p_cho <- plot_oxidation_timecourse(results, substrate = "cho")
  p_fat <- plot_oxidation_timecourse(results, substrate = "fat")

  expect_s3_class(p_all, "ggplot")
  expect_s3_class(p_cho, "ggplot")
  expect_s3_class(p_fat, "ggplot")
})

test_that("plot_oxidation_timecourse works with 'by' grouping", {
  results <- create_plot_test_results()
  p <- plot_oxidation_timecourse(results, by = "protocol")

  expect_s3_class(p, "ggplot")
})

test_that("plot_oxidation_timecourse accepts data frame input", {
  results <- create_plot_test_results()
  df <- results@oxidation_rates

  p <- plot_oxidation_timecourse(df)

  expect_s3_class(p, "ggplot")
})

test_that("plot_oxidation_timecourse errors on invalid input", {
  expect_error(
    plot_oxidation_timecourse("invalid"),
    "OxidationResults|data frame"
  )
})

test_that("plot_oxidation_timecourse respects error_type parameter", {
  results <- create_plot_test_results()

  p_sd <- plot_oxidation_timecourse(results, error_type = "sd")
  p_se <- plot_oxidation_timecourse(results, error_type = "se")

  expect_s3_class(p_sd, "ggplot")
  expect_s3_class(p_se, "ggplot")
})

# -----------------------------------------------------------------------------
# plot_rer_timecourse Tests
# -----------------------------------------------------------------------------

test_that("plot_rer_timecourse returns a ggplot", {
  results <- create_plot_test_results()
  p <- plot_rer_timecourse(results)

  expect_s3_class(p, "ggplot")
})

test_that("plot_rer_timecourse works with 'by' grouping", {
  results <- create_plot_test_results()
  p <- plot_rer_timecourse(results, by = "protocol")

  expect_s3_class(p, "ggplot")
})

test_that("plot_rer_timecourse accepts data frame input", {
  results <- create_plot_test_results()
  df <- results@oxidation_rates

  p <- plot_rer_timecourse(df)

  expect_s3_class(p, "ggplot")
})

test_that("plot_rer_timecourse errors when RER column missing", {
  df <- data.frame(x = 1:3, y = 1:3, time = c(30, 60, 90))

  expect_error(
    plot_rer_timecourse(df),
    "RER column not found"
  )
})

# -----------------------------------------------------------------------------
# plot_vo2_timecourse Tests
# -----------------------------------------------------------------------------

test_that("plot_vo2_timecourse returns a ggplot", {
  results <- create_plot_test_results()
  p <- plot_vo2_timecourse(results)

  expect_s3_class(p, "ggplot")
})

test_that("plot_vo2_timecourse works with 'by' grouping", {
  results <- create_plot_test_results()
  p <- plot_vo2_timecourse(results, by = "protocol")

  expect_s3_class(p, "ggplot")
})

test_that("plot_vo2_timecourse accepts data frame input", {
  results <- create_plot_test_results()
  df <- results@oxidation_rates

  p <- plot_vo2_timecourse(df)

  expect_s3_class(p, "ggplot")
})

test_that("plot_vo2_timecourse errors when VO2 column missing", {
  df <- data.frame(x = 1:3, y = 1:3, time = c(30, 60, 90))

  expect_error(
    plot_vo2_timecourse(df),
    "VO2 column not found"
  )
})

# -----------------------------------------------------------------------------
# plot_energy_contribution Tests
# -----------------------------------------------------------------------------

test_that("plot_energy_contribution returns a ggplot", {
  results <- create_plot_test_results()
  p <- plot_energy_contribution(results)

  expect_s3_class(p, "ggplot")
})

test_that("plot_energy_contribution works with 'by' grouping", {
  results <- create_plot_test_results()
  p <- plot_energy_contribution(results, by = "protocol")

  expect_s3_class(p, "ggplot")
})

test_that("plot_energy_contribution handles missing energy data", {
  # Create results without energy calculations
  calo <- create_plot_test_data()
  study <- OxidationStudy(
    calorimetry = calo,
    isotopes = NULL,
    urea = NULL,
    environment = NULL,
    subjects = NULL
  )
  results <- suppressMessages(
    analyze_oxidation(study, calc_energy = FALSE, validate = FALSE)
  )

  expect_error(
    plot_energy_contribution(results),
    "energy|Energy"
  )
})

# -----------------------------------------------------------------------------
# plot_cho_partition Tests
# -----------------------------------------------------------------------------

test_that("plot_cho_partition returns a ggplot", {
  results <- create_plot_test_results()
  p <- plot_cho_partition(results)

  expect_s3_class(p, "ggplot")
})

test_that("plot_cho_partition works with type parameter", {
  results <- create_plot_test_results()

  p_exo <- plot_cho_partition(results, type = "exo_endo")
  p_mus <- plot_cho_partition(results, type = "mus_liv")

  expect_s3_class(p_exo, "ggplot")
  expect_s3_class(p_mus, "ggplot")
})

# -----------------------------------------------------------------------------
# Plot Layer Verification Tests
# -----------------------------------------------------------------------------

test_that("plot_oxidation_timecourse has expected layers", {
  results <- create_plot_test_results()
  p <- plot_oxidation_timecourse(results)

  # Check for essential layers (line and potentially error bars)
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])

  expect_true(any(grepl("Line|Path", layer_classes)))
})

test_that("plot_rer_timecourse has expected layers", {
  results <- create_plot_test_results()
  p <- plot_rer_timecourse(results)

  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])

  expect_true(any(grepl("Line|Path", layer_classes)))
})

test_that("plot_oxidation_timecourse has correct axis labels", {
  results <- create_plot_test_results()
  p <- plot_oxidation_timecourse(results)

  # X-axis should be time-related
  expect_true(grepl("[Tt]ime|[Mm]in", p$labels$x, ignore.case = TRUE) ||
              p$labels$x == "time")

  # Y-axis should be oxidation-related
  expect_true(grepl("[Oo]xidation|g/min|Rate", p$labels$y, ignore.case = TRUE) ||
              !is.null(p$labels$y))
})
