#' Report Generation Functions
#'
#' @name reports
#' @description Functions for generating analysis reports
NULL

#' Render Oxidation Report
#'
#' Generate a comprehensive Quarto report from oxidation results.
#'
#' @param results An OxidationResults object
#' @param output_file Output file path (e.g., "report.html")
#' @param template Path to custom Quarto template (NULL = default)
#' @param title Report title
#' @param author Report author
#' @param include_stats Include statistical analysis
#' @param include_tables Include summary tables
#' @param include_methods Include methods section
#'
#' @return Path to rendered report
#' @export
render_oxidation_report <- function(results,
                                     output_file = "oxidation_report.html",
                                     template = NULL,
                                     title = "Substrate Oxidation Analysis",
                                     author = Sys.info()["user"],
                                     include_stats = TRUE,
                                     include_tables = TRUE,
                                     include_methods = TRUE) {

  if (!requireNamespace("quarto", quietly = TRUE)) {
    cli::cli_abort("Install the 'quarto' package to render reports")
  }

  # Use default template if not specified
  if (is.null(template)) {
    template <- system.file("templates", "oxidation_report.qmd",
                            package = "oxidizr")
    if (template == "") {
      cli::cli_abort("Default template not found - package may not be installed correctly")
    }
  }

  if (!file.exists(template)) {
    cli::cli_abort("Template file not found: {template}")
  }

  # Create temporary directory for report generation
  temp_dir <- tempfile("oxidizr_report_")
  dir.create(temp_dir)

  # Copy template
  temp_qmd <- file.path(temp_dir, "report.qmd")
  file.copy(template, temp_qmd)

  # Save results data
  results_file <- file.path(temp_dir, "results.rds")
  saveRDS(results, results_file)

  # Create params file
  params <- list(
    results_file = results_file,
    title = title,
    author = author,
    include_stats = include_stats,
    include_tables = include_tables,
    include_methods = include_methods
  )

  params_file <- file.path(temp_dir, "params.rds")
  saveRDS(params, params_file)

  # Render report
  cli::cli_alert_info("Rendering report...")

  tryCatch({
    quarto::quarto_render(
      input = temp_qmd,
      output_file = basename(output_file),
      execute_params = list(
        results_file = results_file,
        title = title,
        author = author,
        include_stats = include_stats,
        include_tables = include_tables,
        include_methods = include_methods
      )
    )

    # Move output to desired location
    rendered_file <- file.path(temp_dir, basename(output_file))
    if (file.exists(rendered_file)) {
      file.copy(rendered_file, output_file, overwrite = TRUE)
      cli::cli_alert_success("Report saved to: {output_file}")
    }
  }, error = function(e) {
    cli::cli_abort("Error rendering report: {e$message}")
  })

  # Cleanup
  unlink(temp_dir, recursive = TRUE)

  invisible(output_file)
}

#' Create Report Template
#'
#' Copy the default report template to a location for customization.
#'
#' @param dest Destination file path
#' @param overwrite Overwrite existing file
#'
#' @return Invisible path to created file
#' @export
create_report_template <- function(dest = "oxidation_report.qmd",
                                    overwrite = FALSE) {

  template <- system.file("templates", "oxidation_report.qmd",
                          package = "oxidizr")

  if (template == "") {
    cli::cli_abort("Default template not found")
  }

  if (file.exists(dest) && !overwrite) {
    cli::cli_abort("File already exists. Use overwrite = TRUE to replace.")
  }

  file.copy(template, dest, overwrite = overwrite)
  cli::cli_alert_success("Template created: {dest}")

  invisible(dest)
}

#' Render Quality Report
#'
#' Generate a comprehensive data quality report from validation results.
#'
#' @param study An OxidationStudy object to validate and report on.
#' @param validation Optional pre-computed ValidationResult object. If NULL,
#'   validation will be performed automatically.
#' @param output_file Output file path (default: "quality_report.html").
#' @param template Path to custom Quarto template (NULL = default).
#' @param title Report title.
#' @param author Report author (default: system user).
#' @param include_steady_state Logical, include steady-state analysis (default: TRUE).
#' @param include_recommendations Logical, include recommendations section (default: TRUE).
#' @param include_raw_data Logical, include raw data tables in appendix (default: FALSE).
#' @param validation_thresholds Optional custom thresholds for validation.
#' @param open Logical, open report in browser after rendering (default: TRUE).
#'
#' @return Path to rendered report (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic quality report
#' study <- oxidation_study(calorimetry = calo_data)
#' render_quality_report(study)
#'
#' # With custom output file
#' render_quality_report(study, output_file = "my_quality_report.html")
#'
#' # With pre-computed validation
#' validation <- validate_study(study)
#' render_quality_report(study, validation = validation)
#' }
render_quality_report <- function(study,
                                   validation = NULL,
                                   output_file = "quality_report.html",
                                   template = NULL,
                                   title = "Data Quality Report",
                                   author = Sys.info()["user"],
                                   include_steady_state = TRUE,
                                   include_recommendations = TRUE,
                                   include_raw_data = FALSE,
                                   validation_thresholds = NULL,
                                   open = TRUE) {

  if (!requireNamespace("quarto", quietly = TRUE)) {
    cli::cli_abort("Install the 'quarto' package to render reports: install.packages('quarto')")
  }

  # Validate inputs
  if (!S7_inherits(study, OxidationStudy)) {
    cli::cli_abort("study must be an OxidationStudy object")
  }

  # Perform validation if not provided
  if (is.null(validation)) {
    cli::cli_alert_info("Running validation...")
    validation <- validate_study(study, thresholds = validation_thresholds)
  }

  if (!S7_inherits(validation, ValidationResult)) {
    cli::cli_abort("validation must be a ValidationResult object")
  }

  # Use default template if not specified
  if (is.null(template)) {
    template <- system.file("templates", "quality_report.qmd", package = "oxidizr")
    if (template == "") {
      cli::cli_abort("Default quality report template not found - package may not be installed correctly")
    }
  }

  if (!file.exists(template)) {
    cli::cli_abort("Template file not found: {template}")
  }

  # Create temporary directory for report generation
  temp_dir <- tempfile("oxidizr_quality_report_")
  dir.create(temp_dir)

  # Copy template to temp directory
 temp_qmd <- file.path(temp_dir, "quality_report.qmd")
  file.copy(template, temp_qmd)

  # Save data for the report
  results_data <- list(
    study = study,
    validation = validation
  )
  results_file <- file.path(temp_dir, "results.rds")
  saveRDS(results_data, results_file)

  # Save validation separately as well
  validation_file <- file.path(temp_dir, "validation.rds")
  saveRDS(validation, validation_file)

  # Render report
  cli::cli_alert_info("Rendering quality report...")

  tryCatch({
    quarto::quarto_render(
      input = temp_qmd,
      output_file = basename(output_file),
      execute_params = list(
        results_file = results_file,
        validation_file = validation_file,
        title = title,
        author = as.character(author),
        include_steady_state = include_steady_state,
        include_recommendations = include_recommendations,
        include_raw_data = include_raw_data
      )
    )

    # Move output to desired location
    rendered_file <- file.path(temp_dir, basename(output_file))
    if (file.exists(rendered_file)) {
      file.copy(rendered_file, output_file, overwrite = TRUE)
      cli::cli_alert_success("Quality report saved to: {output_file}")
    } else {
      cli::cli_abort("Rendered file not found at expected location")
    }

  }, error = function(e) {
    cli::cli_abort("Error rendering quality report: {e$message}")
  })

  # Cleanup temp directory
  unlink(temp_dir, recursive = TRUE)

  # Open in browser if requested
  if (open && interactive()) {
    utils::browseURL(output_file)
  }

  invisible(output_file)
}

#' Create Quality Report Template
#'
#' Copy the default quality report template to a location for customization.
#'
#' @param dest Destination file path.
#' @param overwrite Overwrite existing file (default: FALSE).
#'
#' @return Invisible path to created file.
#' @export
#'
#' @examples
#' \dontrun{
#' create_quality_report_template("my_quality_template.qmd")
#' }
create_quality_report_template <- function(dest = "quality_report.qmd",
                                            overwrite = FALSE) {

  template <- system.file("templates", "quality_report.qmd", package = "oxidizr")

  if (template == "") {
    cli::cli_abort("Default quality report template not found")
  }

  if (file.exists(dest) && !overwrite) {
    cli::cli_abort("File already exists: {dest}. Use overwrite = TRUE to replace.")
  }

  file.copy(template, dest, overwrite = overwrite)
  cli::cli_alert_success("Quality report template created: {dest}")
  cli::cli_inform("Edit this template to customize your quality reports")

  invisible(dest)
}
