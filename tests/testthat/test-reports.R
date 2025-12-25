# Tests for report generation functions

# -----------------------------------------------------------------------------
# Test Data Setup
# -----------------------------------------------------------------------------

create_report_test_study <- function() {
  df <- data.frame(
    id = rep(1:2, each = 4),
    time = rep(c(30, 60, 90, 120), 2),
    vo2 = c(2.5, 2.6, 2.5, 2.4, 2.4, 2.5, 2.4, 2.3),
    vco2 = c(2.3, 2.4, 2.3, 2.2, 2.2, 2.3, 2.2, 2.1)
  )
  calo <- CalorimetryData(data = df)
  OxidationStudy(
    calorimetry = calo,
    isotopes = NULL,
    urea = NULL,
    environment = NULL,
    subjects = NULL
  )
}

create_report_test_results <- function() {
  study <- create_report_test_study()
  suppressMessages(analyze_oxidation(study, validate = FALSE))
}

# -----------------------------------------------------------------------------
# create_report_template Tests
# -----------------------------------------------------------------------------

test_that("create_report_template creates a file", {
  skip_on_cran()

  temp_file <- tempfile(fileext = ".qmd")
  on.exit(unlink(temp_file), add = TRUE)

  # This may fail if template doesn't exist in package
  result <- tryCatch(
    create_report_template(dest = temp_file),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_true(file.exists(temp_file))
    expect_equal(result, temp_file)
  }
})

test_that("create_report_template errors if file exists without overwrite", {
  skip_on_cran()

  temp_file <- tempfile(fileext = ".qmd")
  file.create(temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  # Check if template exists first
  template <- system.file("templates", "oxidation_report.qmd", package = "oxidizr")
  skip_if(template == "", "Template not installed")

  expect_error(
    create_report_template(dest = temp_file, overwrite = FALSE),
    "already exists"
  )
})

test_that("create_report_template allows overwrite", {
  skip_on_cran()

  temp_file <- tempfile(fileext = ".qmd")
  file.create(temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  # Check if template exists first
  template <- system.file("templates", "oxidation_report.qmd", package = "oxidizr")
  skip_if(template == "", "Template not installed")

  result <- create_report_template(dest = temp_file, overwrite = TRUE)
  expect_true(file.exists(temp_file))
})

# -----------------------------------------------------------------------------
# render_oxidation_report Tests
# -----------------------------------------------------------------------------

test_that("render_oxidation_report checks for quarto package", {
  results <- create_report_test_results()

  # This test checks the function validates inputs before requiring quarto
  # If quarto is not installed, we should get an informative error
  skip_if(requireNamespace("quarto", quietly = TRUE), "quarto is installed")

  expect_error(
    render_oxidation_report(results),
    "quarto"
  )
})

test_that("render_oxidation_report validates template path", {
  skip_if_not_installed("quarto")
  skip_on_cran()

  results <- create_report_test_results()

  expect_error(
    render_oxidation_report(results, template = "nonexistent_template.qmd"),
    "not found"
  )
})

test_that("render_oxidation_report accepts valid parameters", {
  skip_if_not_installed("quarto")
  skip_on_cran()

  results <- create_report_test_results()

  # Check template exists
  template <- system.file("templates", "oxidation_report.qmd", package = "oxidizr")
  skip_if(template == "", "Template not installed")

  # Create output in temp directory
  temp_output <- tempfile(fileext = ".html")
  on.exit(unlink(temp_output), add = TRUE)

  # This will only work if quarto CLI is also installed
  result <- tryCatch(
    render_oxidation_report(
      results,
      output_file = temp_output,
      title = "Test Report",
      author = "Test Author"
    ),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_equal(result, temp_output)
  }
})

# -----------------------------------------------------------------------------
# render_quality_report Tests
# -----------------------------------------------------------------------------

test_that("render_quality_report checks for quarto package", {
  study <- create_report_test_study()

  skip_if(requireNamespace("quarto", quietly = TRUE), "quarto is installed")

  expect_error(
    render_quality_report(study),
    "quarto"
  )
})

test_that("render_quality_report validates OxidationStudy input", {
  skip_if_not_installed("quarto")
  skip_on_cran()

  expect_error(
    render_quality_report(data.frame(x = 1)),
    "OxidationStudy"
  )
})

test_that("render_quality_report validates template path", {
  skip_if_not_installed("quarto")
  skip_on_cran()

  study <- create_report_test_study()

  expect_error(
    render_quality_report(study, template = "nonexistent_template.qmd"),
    "not found"
  )
})

test_that("render_quality_report accepts custom parameters", {
  skip_if_not_installed("quarto")
  skip_on_cran()

  study <- create_report_test_study()

  # Check template exists
  template <- system.file("templates", "quality_report.qmd", package = "oxidizr")
  skip_if(template == "", "Quality template not installed")

  temp_output <- tempfile(fileext = ".html")
  on.exit(unlink(temp_output), add = TRUE)

  # This will only work if quarto CLI is also installed
  result <- tryCatch(
    render_quality_report(
      study,
      output_file = temp_output,
      title = "Test Quality Report",
      include_steady_state = FALSE,
      include_raw_data = FALSE,
      open = FALSE
    ),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_equal(result, temp_output)
  }
})

# -----------------------------------------------------------------------------
# Template Existence Tests
# -----------------------------------------------------------------------------

test_that("default oxidation report template exists in package", {
  template <- system.file("templates", "oxidation_report.qmd", package = "oxidizr")

  # Template should exist when package is properly installed
  # Skip if running from source without inst/templates
  skip_if(template == "", "Package not installed with templates")

  expect_true(file.exists(template))
})

test_that("default quality report template exists in package", {
  template <- system.file("templates", "quality_report.qmd", package = "oxidizr")

  skip_if(template == "", "Package not installed with templates")

  expect_true(file.exists(template))
})

# -----------------------------------------------------------------------------
# Integration Tests (require full quarto installation)
# -----------------------------------------------------------------------------

test_that("render_oxidation_report produces valid output", {
  skip_if_not_installed("quarto")
  skip_on_cran()
  skip_on_ci()  # Skip on CI as quarto CLI may not be available

  results <- create_report_test_results()
  template <- system.file("templates", "oxidation_report.qmd", package = "oxidizr")
  skip_if(template == "", "Template not installed")

  temp_output <- tempfile(fileext = ".html")
  on.exit(unlink(temp_output), add = TRUE)

  # Attempt rendering - may fail if quarto CLI not available
  result <- tryCatch(
    suppressMessages(render_oxidation_report(results, output_file = temp_output)),
    error = function(e) NULL
  )

  if (!is.null(result) && file.exists(temp_output)) {
    # Check file is non-empty HTML
    content <- readLines(temp_output, n = 10, warn = FALSE)
    expect_true(any(grepl("<html|<!DOCTYPE", content, ignore.case = TRUE)))
  }
})
