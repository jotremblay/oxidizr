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
