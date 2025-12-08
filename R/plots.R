#' Visualization Functions
#'
#' @name plots
#' @description Functions for creating publication-ready plots
NULL

#' Publication Theme
#'
#' A clean ggplot2 theme for publication-ready figures.
#'
#' @param base_size Base font size (default: 12)
#' @param base_family Base font family (default: "")
#'
#' @return A ggplot2 theme object
#' @export
theme_oxidizr <- function(base_size = 12, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "bold", size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 2),
      axis.title = ggplot2::element_text(size = base_size, face = "bold"),
      axis.text = ggplot2::element_text(size = base_size - 2),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = base_size),
      strip.background = ggplot2::element_rect(fill = "grey90")
    )
}

#' Plot Oxidation Time Course
#'
#' Create a time course plot of substrate oxidation rates.
#'
#' @param results An OxidationResults object or data frame
#' @param substrate Which substrate(s) to plot: "cho", "fat", "protein", or "all"
#' @param by Grouping variable for color/linetype (e.g., "protocol")
#' @param time_col Name of the time column
#' @param show_error Show error bars (SD or SE)
#' @param error_type Type of error bars: "sd" or "se"
#' @param dodge_width Width for position_dodge (default: 3)
#'
#' @return A ggplot2 object
#' @export
plot_oxidation_timecourse <- function(results,
                                       substrate = c("all", "cho", "fat", "protein"),
                                       by = NULL,
                                       time_col = "time",
                                       show_error = TRUE,
                                       error_type = c("sd", "se"),
                                       dodge_width = 3) {

  substrate <- match.arg(substrate)
  error_type <- match.arg(error_type)

  # Extract data
  if (S7_inherits(results, OxidationResults)) {
    df <- results@oxidation_rates
  } else if (is.data.frame(results)) {
    df <- results
  } else {
    cli::cli_abort("results must be OxidationResults or data frame")
  }

  # Ensure time is numeric
  if (is.factor(df[[time_col]])) {
    df[[time_col]] <- as.numeric(as.character(df[[time_col]]))
  }

  # Select substrates to plot
  if (substrate == "all") {
    plot_cols <- c("cho_total", "fat_total", "protein_ox")
  } else if (substrate == "cho") {
    plot_cols <- c("cho_total")
    if ("cho_exo" %in% names(df)) plot_cols <- c(plot_cols, "cho_exo", "cho_endo")
  } else if (substrate == "fat") {
    plot_cols <- c("fat_total")
  } else {
    plot_cols <- c("protein_ox")
  }

  plot_cols <- intersect(plot_cols, names(df))

  if (length(plot_cols) == 0) {
    cli::cli_abort("No matching substrate columns found")
  }

  # Prepare data for plotting
  group_cols <- time_col
  if (!is.null(by)) group_cols <- c(group_cols, by)

  # Calculate summary statistics
  plot_df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(plot_cols),
        list(mean = ~ mean(.x, na.rm = TRUE),
             sd = ~ sd(.x, na.rm = TRUE),
             se = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  # Pivot to long format
  mean_cols <- paste0(plot_cols, "_mean")
  error_cols <- paste0(plot_cols, "_", error_type)

  plot_long <- plot_df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(mean_cols),
      names_to = "substrate",
      values_to = "rate"
    ) |>
    dplyr::mutate(
      substrate = gsub("_mean$", "", .data$substrate),
      substrate = dplyr::case_when(
        .data$substrate == "cho_total" ~ "Total CHO",
        .data$substrate == "cho_exo" ~ "Exogenous CHO",
        .data$substrate == "cho_endo" ~ "Endogenous CHO",
        .data$substrate == "fat_total" ~ "Fat",
        .data$substrate == "protein_ox" ~ "Protein",
        TRUE ~ .data$substrate
      )
    )

  # Add error values
  error_long <- plot_df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(error_cols),
      names_to = "substrate_error",
      values_to = "error"
    ) |>
    dplyr::mutate(
      substrate = gsub(paste0("_", error_type, "$"), "", .data$substrate_error),
      substrate = dplyr::case_when(
        .data$substrate == "cho_total" ~ "Total CHO",
        .data$substrate == "cho_exo" ~ "Exogenous CHO",
        .data$substrate == "cho_endo" ~ "Endogenous CHO",
        .data$substrate == "fat_total" ~ "Fat",
        .data$substrate == "protein_ox" ~ "Protein",
        TRUE ~ .data$substrate
      )
    ) |>
    dplyr::select(-"substrate_error")

  # Join mean and error
  join_cols <- c(group_cols, "substrate")
  plot_long <- plot_long |>
    dplyr::left_join(error_long, by = join_cols)

  # Create plot
  if (!is.null(by)) {
    p <- ggplot2::ggplot(plot_long,
                          ggplot2::aes(x = .data[[time_col]], y = .data$rate,
                                       color = .data[[by]], group = interaction(.data$substrate, .data[[by]])))
  } else {
    p <- ggplot2::ggplot(plot_long,
                          ggplot2::aes(x = .data[[time_col]], y = .data$rate,
                                       color = .data$substrate, group = .data$substrate))
  }

  pos <- ggplot2::position_dodge(width = dodge_width)

  p <- p +
    ggplot2::geom_line(position = pos, linewidth = 0.8) +
    ggplot2::geom_point(position = pos, size = 3)

  if (show_error) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$rate - .data$error, ymax = .data$rate + .data$error),
        position = pos, width = dodge_width * 0.8, linewidth = 0.6
      )
  }

  # Facet by substrate if showing multiple
  if (length(plot_cols) > 1 && is.null(by)) {
    p <- p + ggplot2::facet_wrap(~ substrate, scales = "free_y")
  } else if (length(plot_cols) > 1) {
    p <- p + ggplot2::facet_wrap(~ substrate, scales = "free_y")
  }

  p <- p +
    ggplot2::labs(
      x = "Time (min)",
      y = "Oxidation Rate (g/min)",
      color = if (is.null(by)) "Substrate" else by
    ) +
    theme_oxidizr()

  p
}

#' Plot RER Time Course
#'
#' Create a time course plot of Respiratory Exchange Ratio.
#'
#' @param results An OxidationResults object or data frame
#' @param by Grouping variable for color (e.g., "protocol")
#' @param time_col Name of the time column
#' @param show_error Show error bars
#' @param error_type Type of error bars: "sd" or "se"
#'
#' @return A ggplot2 object
#' @export
plot_rer_timecourse <- function(results,
                                 by = NULL,
                                 time_col = "time",
                                 show_error = TRUE,
                                 error_type = c("sd", "se")) {

  error_type <- match.arg(error_type)

  if (S7_inherits(results, OxidationResults)) {
    df <- results@oxidation_rates
  } else {
    df <- results
  }

  if (!"rer" %in% names(df)) {
    cli::cli_abort("RER column not found in data")
  }

  if (is.factor(df[[time_col]])) {
    df[[time_col]] <- as.numeric(as.character(df[[time_col]]))
  }

  group_cols <- time_col
  if (!is.null(by)) group_cols <- c(group_cols, by)

  plot_df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      rer_mean = mean(.data$rer, na.rm = TRUE),
      rer_sd = sd(.data$rer, na.rm = TRUE),
      rer_se = sd(.data$rer, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    ) |>
    dplyr::mutate(error = .data[[paste0("rer_", error_type)]])

  if (!is.null(by)) {
    p <- ggplot2::ggplot(plot_df,
                          ggplot2::aes(x = .data[[time_col]], y = .data$rer_mean,
                                       color = .data[[by]], group = .data[[by]]))
  } else {
    p <- ggplot2::ggplot(plot_df,
                          ggplot2::aes(x = .data[[time_col]], y = .data$rer_mean))
  }

  pos <- ggplot2::position_dodge(width = 3)

  p <- p +
    ggplot2::geom_line(position = pos, linewidth = 0.8) +
    ggplot2::geom_point(position = pos, size = 3)

  if (show_error) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$rer_mean - .data$error, ymax = .data$rer_mean + .data$error),
        position = pos, width = 3, linewidth = 0.6
      )
  }

  p <- p +
    ggplot2::labs(x = "Time (min)", y = "RER", color = by) +
    theme_oxidizr()

  p
}

#' Plot VO2 Time Course
#'
#' Create a time course plot of oxygen consumption.
#'
#' @param results An OxidationResults object or data frame
#' @param by Grouping variable for color
#' @param time_col Name of time column
#' @param show_error Show error bars
#' @param error_type Type of error bars
#'
#' @return A ggplot2 object
#' @export
plot_vo2_timecourse <- function(results,
                                 by = NULL,
                                 time_col = "time",
                                 show_error = TRUE,
                                 error_type = c("sd", "se")) {

  error_type <- match.arg(error_type)

  if (S7_inherits(results, OxidationResults)) {
    df <- results@oxidation_rates
  } else {
    df <- results
  }

  vo2_col <- if ("vo2_lmin" %in% names(df)) "vo2_lmin" else "vo2"
  if (!vo2_col %in% names(df)) {
    cli::cli_abort("VO2 column not found")
  }

  if (is.factor(df[[time_col]])) {
    df[[time_col]] <- as.numeric(as.character(df[[time_col]]))
  }

  group_cols <- time_col
  if (!is.null(by)) group_cols <- c(group_cols, by)

  plot_df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      vo2_mean = mean(.data[[vo2_col]], na.rm = TRUE),
      vo2_sd = sd(.data[[vo2_col]], na.rm = TRUE),
      vo2_se = sd(.data[[vo2_col]], na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    ) |>
    dplyr::mutate(error = .data[[paste0("vo2_", error_type)]])

  if (!is.null(by)) {
    p <- ggplot2::ggplot(plot_df,
                          ggplot2::aes(x = .data[[time_col]], y = .data$vo2_mean,
                                       color = .data[[by]], group = .data[[by]]))
  } else {
    p <- ggplot2::ggplot(plot_df,
                          ggplot2::aes(x = .data[[time_col]], y = .data$vo2_mean))
  }

  pos <- ggplot2::position_dodge(width = 3)

  p <- p +
    ggplot2::geom_line(position = pos, linewidth = 0.8) +
    ggplot2::geom_point(position = pos, size = 3)

  if (show_error) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$vo2_mean - .data$error, ymax = .data$vo2_mean + .data$error),
        position = pos, width = 3, linewidth = 0.6
      )
  }

  p <- p +
    ggplot2::labs(
      x = "Time (min)",
      y = expression(bold(paste(VO[2], " (L/min)"))),
      color = by
    ) +
    theme_oxidizr()

  p
}

#' Plot Energy Contribution
#'
#' Create a stacked bar chart of energy contributions by substrate.
#'
#' @param results An OxidationResults object
#' @param by Grouping variable (typically "protocol")
#' @param partition Show CHO partition (exo/endo) instead of total
#' @param show_error Show error bars
#'
#' @return A ggplot2 object
#' @export
plot_energy_contribution <- function(results,
                                      by = "protocol",
                                      partition = FALSE,
                                      show_error = TRUE) {

  if (S7_inherits(results, OxidationResults)) {
    df <- results@energy_contributions
    if (is.null(df)) {
      cli::cli_abort("Energy contributions not calculated - run analyze_oxidation with calc_energy = TRUE")
    }
  } else {
    df <- results
  }

  # Select columns based on partition
  if (partition && "pct_cho_exo" %in% names(df)) {
    pct_cols <- c("pct_cho_exo", "pct_cho_endo", "pct_fat", "pct_protein")
  } else {
    pct_cols <- c("pct_cho_total", "pct_fat", "pct_protein")
  }

  available_cols <- intersect(pct_cols, names(df))

  # Calculate summary by group
  plot_df <- df |>
    dplyr::group_by(.data[[by]]) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(available_cols),
        list(mean = ~ mean(.x, na.rm = TRUE),
             sd = ~ sd(.x, na.rm = TRUE)),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  # Pivot to long format
  mean_cols <- paste0(available_cols, "_mean")
  sd_cols <- paste0(available_cols, "_sd")

  plot_long <- plot_df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(mean_cols),
      names_to = "substrate",
      values_to = "mean"
    ) |>
    dplyr::mutate(
      substrate = gsub("_mean$", "", .data$substrate),
      substrate = gsub("^pct_", "", .data$substrate)
    )

  # Add SD
  sd_long <- plot_df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(sd_cols),
      names_to = "substrate_sd",
      values_to = "sd"
    ) |>
    dplyr::mutate(
      substrate = gsub("_sd$", "", .data$substrate_sd),
      substrate = gsub("^pct_", "", .data$substrate)
    ) |>
    dplyr::select(-"substrate_sd")

  plot_long <- plot_long |>
    dplyr::left_join(sd_long, by = c(by, "substrate"))

  # Set factor order for stacking
  plot_long <- plot_long |>
    dplyr::mutate(
      substrate = factor(.data$substrate,
                         levels = c("protein", "fat", "cho_total", "cho_endo", "cho_exo"))
    ) |>
    dplyr::arrange(.data[[by]], dplyr::desc(.data$substrate)) |>
    dplyr::group_by(.data[[by]]) |>
    dplyr::mutate(cumsum = cumsum(.data$mean)) |>
    dplyr::ungroup()

  # Clean labels
  plot_long <- plot_long |>
    dplyr::mutate(
      substrate_label = dplyr::case_when(
        .data$substrate == "cho_total" ~ "Total CHO",
        .data$substrate == "cho_exo" ~ "Exogenous CHO",
        .data$substrate == "cho_endo" ~ "Endogenous CHO",
        .data$substrate == "fat" ~ "Fat",
        .data$substrate == "protein" ~ "Protein",
        TRUE ~ as.character(.data$substrate)
      )
    )

  p <- ggplot2::ggplot(plot_long,
                        ggplot2::aes(x = .data[[by]], y = .data$mean, fill = .data$substrate_label)) +
    ggplot2::geom_bar(stat = "identity", color = "black", width = 0.7)

  if (show_error) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$cumsum - .data$sd, ymax = .data$cumsum),
        width = 0.2
      )
  }

  # Set colors
  if (partition) {
    p <- p + ggplot2::scale_fill_manual(
      values = c("Exogenous CHO" = "#E41A1C", "Endogenous CHO" = "#FF7F00",
                 "Fat" = "#FFFF33", "Protein" = "#000000")
    )
  } else {
    p <- p + ggplot2::scale_fill_manual(
      values = c("Total CHO" = "#FFC0CB", "Fat" = "#FFFF33", "Protein" = "#000000")
    )
  }

  p <- p +
    ggplot2::labs(
      x = "",
      y = "Contribution to Energy Yield (%)",
      fill = ""
    ) +
    theme_oxidizr() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  p
}

#' Plot CHO Partition
#'
#' Create a plot showing the partitioning of CHO oxidation sources.
#'
#' @param results An OxidationResults object
#' @param by Grouping variable
#' @param type Type of partition: "exo_endo" or "mus_liv"
#'
#' @return A ggplot2 object
#' @export
plot_cho_partition <- function(results,
                                by = "protocol",
                                type = c("exo_endo", "mus_liv")) {

  type <- match.arg(type)

  plot_energy_contribution(results, by = by, partition = TRUE)
}

#' Plot Isotope Enrichment
#'
#' Create a time course plot of 13C isotope enrichment.
#'
#' @param results An OxidationResults object with isotope data
#' @param type Type of enrichment: "expired" (Rexp), "plasma" (Rpla)
#' @param by Grouping variable
#' @param time_col Name of time column
#' @param show_error Show error bars
#'
#' @return A ggplot2 object
#' @export
plot_isotope_enrichment <- function(results,
                                     type = c("expired", "plasma"),
                                     by = NULL,
                                     time_col = "time",
                                     show_error = TRUE) {

  type <- match.arg(type)

  if (S7_inherits(results, OxidationResults)) {
    df <- results@oxidation_rates
    if (is.null(results@study) || is.null(results@study@isotopes)) {
      cli::cli_abort("Isotope data not available in results")
    }
    # Get isotope data from study
    if (type == "expired") {
      iso_df <- results@study@isotopes@rexp
      value_col <- results@study@isotopes@rexp_col
      ylabel <- expression(bold(paste(delta^{13}, "CO"[2], " in expired gases (\u2030)")))
    } else {
      if (is.null(results@study@isotopes@rpla)) {
        cli::cli_abort("Plasma enrichment data not available")
      }
      iso_df <- results@study@isotopes@rpla
      value_col <- results@study@isotopes@rpla_col
      ylabel <- expression(bold(paste("Plasma ", delta^{13}, "C-glucose (\u2030)")))
    }
    df <- iso_df
  } else {
    df <- results
    value_col <- if (type == "expired") "rexp" else "rpla"
    ylabel <- if (type == "expired") {
      expression(bold(paste(delta^{13}, "CO"[2], " (\u2030)")))
    } else {
      expression(bold(paste(delta^{13}, "C-glucose (\u2030)")))
    }
  }

  if (is.factor(df[[time_col]])) {
    df[[time_col]] <- as.numeric(as.character(df[[time_col]]))
  }

  group_cols <- time_col
  if (!is.null(by)) group_cols <- c(group_cols, by)

  plot_df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      value_mean = mean(.data[[value_col]], na.rm = TRUE),
      value_sd = sd(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    )

  if (!is.null(by)) {
    p <- ggplot2::ggplot(plot_df,
                          ggplot2::aes(x = .data[[time_col]], y = .data$value_mean,
                                       color = .data[[by]], group = .data[[by]]))
  } else {
    p <- ggplot2::ggplot(plot_df,
                          ggplot2::aes(x = .data[[time_col]], y = .data$value_mean))
  }

  pos <- ggplot2::position_dodge(width = 5)

  p <- p +
    ggplot2::geom_line(position = pos, linewidth = 0.8) +
    ggplot2::geom_point(position = pos, size = 3)

  if (show_error) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$value_mean - .data$value_sd,
                     ymax = .data$value_mean + .data$value_sd),
        position = pos, width = 5, linewidth = 0.6
      )
  }

  p <- p +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
    ggplot2::scale_x_continuous(breaks = seq(-30, 120, 30)) +
    ggplot2::labs(x = "Time (min)", y = ylabel, color = by) +
    theme_oxidizr()

  p
}

# -----------------------------------------------------------------------------
# Diagnostic / Quality Plots
# -----------------------------------------------------------------------------

#' Plot Data Quality Overview
#'
#' Creates a multi-panel dashboard showing validation results summary.
#'
#' @param validation A ValidationResult object from validate_study() or component validators.
#' @param show_categories Logical, whether to show issues by category (default: TRUE).
#' @param show_severity Logical, whether to show issues by severity (default: TRUE).
#' @param show_variables Logical, whether to show most affected variables (default: TRUE).
#' @param max_variables Maximum number of variables to show (default: 10).
#'
#' @return A patchwork-combined ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' validation <- validate_study(study)
#' plot_data_quality(validation)
#' }
plot_data_quality <- function(validation,
                               show_categories = TRUE,
                               show_severity = TRUE,
                               show_variables = TRUE,
                               max_variables = 10) {
  if (!S7_inherits(validation, ValidationResult)) {
    cli::cli_abort("validation must be a ValidationResult object")
  }

  issues <- validation@issues

  if (nrow(issues) == 0) {
    cli::cli_inform("No validation issues found - nothing to plot")
    return(invisible(NULL))
  }

  plots <- list()

  # Severity distribution
  if (show_severity) {
    severity_df <- issues |>
      dplyr::count(.data$severity) |>
      dplyr::mutate(
        severity = factor(.data$severity, levels = c("error", "warning", "info")),
        fill_color = dplyr::case_when(
          .data$severity == "error" ~ "#E41A1C",
          .data$severity == "warning" ~ "#FF7F00",
          .data$severity == "info" ~ "#377EB8"
        )
      )

    p_severity <- ggplot2::ggplot(severity_df, ggplot2::aes(x = .data$severity, y = .data$n, fill = .data$severity)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = c("error" = "#E41A1C", "warning" = "#FF7F00", "info" = "#377EB8")) +
      ggplot2::labs(x = "Severity", y = "Count", title = "Issues by Severity") +
      theme_oxidizr() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0))

    plots$severity <- p_severity
  }

  # Category distribution
  if (show_categories) {
    category_df <- issues |>
      dplyr::count(.data$category, .data$severity) |>
      dplyr::mutate(
        severity = factor(.data$severity, levels = c("error", "warning", "info"))
      )

    p_category <- ggplot2::ggplot(category_df, ggplot2::aes(x = .data$category, y = .data$n, fill = .data$severity)) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_fill_manual(values = c("error" = "#E41A1C", "warning" = "#FF7F00", "info" = "#377EB8")) +
      ggplot2::labs(x = "Category", y = "Count", title = "Issues by Category", fill = "Severity") +
      theme_oxidizr() +
      ggplot2::coord_flip()

    plots$category <- p_category
  }

  # Most affected variables
  if (show_variables) {
    variable_df <- issues |>
      dplyr::filter(!is.na(.data$variable) & .data$variable != "") |>
      dplyr::group_by(.data$variable) |>
      dplyr::summarise(
        total_affected = sum(.data$n_affected, na.rm = TRUE),
        n_issues = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$total_affected)) |>
      dplyr::slice_head(n = max_variables) |>
      dplyr::mutate(variable = forcats::fct_reorder(.data$variable, .data$total_affected))

    if (nrow(variable_df) > 0) {
      p_variables <- ggplot2::ggplot(variable_df, ggplot2::aes(x = .data$variable, y = .data$total_affected)) +
        ggplot2::geom_col(fill = "#4DAF4A") +
        ggplot2::labs(x = "Variable", y = "Affected Values", title = "Most Affected Variables") +
        theme_oxidizr() +
        ggplot2::coord_flip()

      plots$variables <- p_variables
    }
  }

  # Combine plots using patchwork if available
  if (length(plots) == 0) {
    return(invisible(NULL))
  }

  if (length(plots) == 1) {
    return(plots[[1]])
  }

  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- patchwork::wrap_plots(plots, ncol = 2)
    return(combined)
  } else {
    cli::cli_warn("Install patchwork for combined plot output; returning first plot")
    return(plots[[1]])
  }
}

#' Plot Steady-State Analysis
#'
#' Visualizes steady-state detection results showing the variable time course
#' with rolling statistics and highlighted steady-state periods.
#'
#' @param steady_state_result Result from detect_steady_state() functions.
#' @param data Optional original data frame for additional context.
#' @param variable Variable to plot (default: "vo2"). Only used if not present
#'   in steady_state_result.
#' @param show_cv Logical, show CV overlay (default: TRUE if available).
#' @param show_variance Logical, show variance overlay (default: TRUE if available).
#' @param highlight_periods Logical, highlight qualifying steady-state periods (default: TRUE).
#' @param id_col Subject ID column name (default: "id").
#' @param time_col Time column name (default: "time").
#' @param facet_by Character, facet by this column (default: id_col for multiple subjects).
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' ss_result <- detect_steady_state(calo_data)
#' plot_steady_state(ss_result)
#' }
plot_steady_state <- function(steady_state_result,
                               data = NULL,
                               variable = "vo2",
                               show_cv = TRUE,
                               show_variance = TRUE,
                               highlight_periods = TRUE,
                               id_col = "id",
                               time_col = "time",
                               facet_by = NULL) {
  df <- steady_state_result

  # Try to get variable from attributes
  attr_var <- attr(df, "variable")

  attr_vars <- attr(df, "variables")
  if (!is.null(attr_var) && length(attr_var) == 1) {
    variable <- as.character(attr_var)
  } else if (!is.null(attr_vars) && length(attr_vars) >= 1) {
    variable <- as.character(attr_vars)[[1]]
  }

  # Ensure we have a single variable
  variable <- variable[[1]]

  # Check if variable exists in data
  if (!variable %in% names(df)) {
    cli::cli_abort("Variable '{variable}' not found in steady_state_result")
  }

  n_subjects <- length(unique(df[[id_col]]))

  # Auto-facet for multiple subjects
  if (is.null(facet_by) && n_subjects > 1) {
    facet_by <- id_col
  }

  # Base plot - time series of variable
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[time_col]], y = .data[[variable]])) +
    ggplot2::geom_line(color = "grey50", linewidth = 0.5, alpha = 0.7) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$meets_duration),
      size = 2, alpha = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#4DAF4A", "FALSE" = "#E41A1C"),
      labels = c("TRUE" = "Steady-state", "FALSE" = "Non-steady"),
      name = "State"
    )

  # Highlight steady-state regions
  if (highlight_periods && "steady_period" %in% names(df)) {
    ss_periods <- df |>
      dplyr::filter(.data$steady_period > 0 & .data$meets_duration) |>
      dplyr::group_by(.data[[id_col]], .data$steady_period) |>
      dplyr::summarise(
        start = min(.data[[time_col]], na.rm = TRUE),
        end = max(.data[[time_col]], na.rm = TRUE),
        .groups = "drop"
      )

    if (nrow(ss_periods) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = ss_periods,
          ggplot2::aes(xmin = .data$start, xmax = .data$end, ymin = -Inf, ymax = Inf),
          fill = "#4DAF4A", alpha = 0.15, inherit.aes = FALSE
        )
    }
  }

  # Add threshold lines if available
  cv_threshold <- attr(df, "cv_threshold")

  # Facet if needed
  if (!is.null(facet_by) && facet_by %in% names(df)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_by)), scales = "free_y")
  }

  # Labels
  y_label <- switch(variable,
    "vo2" = expression(VO[2]~(L/min)),
    "vco2" = expression(VCO[2]~(L/min)),
    variable
  )

  p <- p +
    ggplot2::labs(
      x = "Time (min)",
      y = y_label,
      title = paste("Steady-State Detection:", variable),
      subtitle = if (!is.null(cv_threshold)) paste0("CV threshold: ", cv_threshold * 100, "%") else NULL
    ) +
    theme_oxidizr()

  p
}

#' Plot Outliers
#'
#' Creates a time series plot with outliers highlighted based on validation results.
#'
#' @param data A data frame or CalorimetryData object.
#' @param validation A ValidationResult object containing outlier information.
#' @param variables Character vector of variables to plot (default: c("vo2", "vco2", "rer")).
#' @param id_col Subject ID column name (default: "id").
#' @param time_col Time column name (default: "time").
#' @param facet_subjects Logical, facet by subject (default: TRUE if multiple subjects).
#' @param max_subjects Maximum subjects to show (default: 6).
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' validation <- validate_calorimetry(calo_data)
#' plot_outliers(calo_data, validation)
#' }
plot_outliers <- function(data,
                           validation,
                           variables = c("vo2", "vco2", "rer"),
                           id_col = "id",
                           time_col = "time",
                           facet_subjects = NULL,
                           max_subjects = 6) {
  # Extract data
  if (S7_inherits(data, CalorimetryData)) {
    id_col <- data@id_col
    time_col <- data@time_col
    df <- data@data
  } else {
    df <- data
  }

  if (!S7_inherits(validation, ValidationResult)) {
    cli::cli_abort("validation must be a ValidationResult object")
  }

  # Filter to available variables
  variables <- intersect(variables, names(df))
  if (length(variables) == 0) {
    cli::cli_abort("None of the specified variables found in data")
  }

  # Get outlier issues
  issues <- validation@issues
  outlier_issues <- issues |>
    dplyr::filter(grepl("outlier", .data$check_id, ignore.case = TRUE))

  # Build outlier indicator for each row
  # This is approximate - we mark rows based on subject_ids from issues
  df <- df |>
    dplyr::mutate(.row_id = dplyr::row_number())

  # Pivot to long format for plotting
  plot_df <- df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variable",
      values_to = "value"
    )

  # Mark outliers based on validation issues
  # Create an outlier flag based on subject and variable
  outlier_flags <- outlier_issues |>
    dplyr::select("variable", "subject_ids") |>
    tidyr::unnest("subject_ids") |>
    dplyr::rename(!!id_col := "subject_ids") |>
    dplyr::distinct() |>
    dplyr::mutate(is_outlier_subject = TRUE)

  plot_df <- plot_df |>
    dplyr::left_join(outlier_flags, by = c(id_col, "variable")) |>
    dplyr::mutate(is_outlier_subject = dplyr::coalesce(.data$is_outlier_subject, FALSE))

  # Limit subjects if needed
  n_subjects <- length(unique(df[[id_col]]))
  if (n_subjects > max_subjects) {
    subjects_to_show <- unique(df[[id_col]])[1:max_subjects]
    plot_df <- plot_df |>
      dplyr::filter(.data[[id_col]] %in% subjects_to_show)
    cli::cli_inform("Showing first {max_subjects} subjects (total: {n_subjects})")
  }

  # Auto facet decision
  if (is.null(facet_subjects)) {
    facet_subjects <- n_subjects > 1 && n_subjects <= max_subjects
  }

  # Create plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[time_col]], y = .data$value))

  if (facet_subjects && n_subjects > 1) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(group = .data[[id_col]]), color = "grey60", alpha = 0.5) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$is_outlier_subject),
        size = 1.5, alpha = 0.7
      ) +
      ggplot2::facet_grid(stats::as.formula(paste("variable ~", id_col)), scales = "free_y")
  } else {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(group = .data[[id_col]]), color = "grey60", alpha = 0.3) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$is_outlier_subject),
        size = 1.5, alpha = 0.7
      ) +
      ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 1)
  }

  p <- p +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#E41A1C", "FALSE" = "#377EB8"),
      labels = c("TRUE" = "Outlier", "FALSE" = "Normal"),
      name = ""
    ) +
    ggplot2::labs(
      x = "Time (min)",
      y = "Value",
      title = "Time Series with Outlier Detection"
    ) +
    theme_oxidizr()

  p
}

#' Plot Missing Data Patterns
#'
#' Creates a visualization of missing data patterns in the dataset.
#'
#' @param data A data frame or CalorimetryData object.
#' @param variables Character vector of variables to check (default: auto-detect).
#' @param type Type of plot: "heatmap" (default), "bar", or "upset".
#' @param id_col Subject ID column name (default: "id").
#' @param time_col Time column name (default: "time").
#' @param max_subjects Maximum subjects for heatmap (default: 20).
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_missing_data(calo_data)
#' plot_missing_data(calo_data, type = "bar")
#' }
plot_missing_data <- function(data,
                               variables = NULL,
                               type = c("heatmap", "bar", "upset"),
                               id_col = "id",
                               time_col = "time",
                               max_subjects = 20) {
  type <- match.arg(type)

  # Extract data
  if (S7_inherits(data, CalorimetryData)) {
    id_col <- data@id_col
    time_col <- data@time_col
    df <- data@data
  } else {
    df <- data
  }

  # Auto-detect numeric variables if not specified
  if (is.null(variables)) {
    variables <- names(df)[sapply(df, is.numeric)]
    variables <- setdiff(variables, c(id_col, time_col))
  }

  variables <- intersect(variables, names(df))
  if (length(variables) == 0) {
    cli::cli_abort("No numeric variables found to check for missingness")
  }

  # Calculate missingness
  missing_df <- df |>
    dplyr::select(dplyr::all_of(c(id_col, time_col, variables))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(variables), is.na))

  if (type == "bar") {
    # Bar plot of missingness by variable
    bar_df <- missing_df |>
      dplyr::summarise(dplyr::across(dplyr::all_of(variables), ~ sum(.x) / dplyr::n() * 100)) |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "variable",
        values_to = "pct_missing"
      ) |>
      dplyr::mutate(variable = forcats::fct_reorder(.data$variable, .data$pct_missing))

    p <- ggplot2::ggplot(bar_df, ggplot2::aes(x = .data$variable, y = .data$pct_missing)) +
      ggplot2::geom_col(fill = "#E41A1C", alpha = 0.8) +
      ggplot2::geom_hline(yintercept = 5, linetype = 2, color = "grey50") +
      ggplot2::labs(
        x = "Variable",
        y = "Missing (%)",
        title = "Missing Data by Variable",
        subtitle = "Dashed line indicates 5% threshold"
      ) +
      theme_oxidizr() +
      ggplot2::coord_flip()

  } else if (type == "heatmap") {
    # Heatmap by subject and variable
    heatmap_df <- missing_df |>
      dplyr::group_by(.data[[id_col]]) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(variables), ~ sum(.x) / dplyr::n() * 100),
        .groups = "drop"
      )

    # Limit subjects
    n_subjects <- nrow(heatmap_df)
    if (n_subjects > max_subjects) {
      heatmap_df <- heatmap_df |> dplyr::slice_head(n = max_subjects)
      cli::cli_inform("Showing first {max_subjects} subjects (total: {n_subjects})")
    }

    heatmap_long <- heatmap_df |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(variables),
        names_to = "variable",
        values_to = "pct_missing"
      )

    p <- ggplot2::ggplot(heatmap_long,
                          ggplot2::aes(x = .data$variable, y = factor(.data[[id_col]]), fill = .data$pct_missing)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_gradient2(
        low = "#4DAF4A",
        mid = "#FFFF33",
        high = "#E41A1C",
        midpoint = 25,
        limits = c(0, 100),
        name = "Missing (%)"
      ) +
      ggplot2::labs(
        x = "Variable",
        y = "Subject",
        title = "Missing Data Heatmap"
      ) +
      theme_oxidizr() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid = ggplot2::element_blank()
      )

  } else {
    # Upset-style plot (simplified version without UpSetR dependency)
    # Show co-occurrence of missingness patterns
    pattern_df <- missing_df |>
      dplyr::select(dplyr::all_of(variables)) |>
      dplyr::mutate(pattern = apply(dplyr::pick(dplyr::everything()), 1, function(x) paste(which(x), collapse = "-"))) |>
      dplyr::filter(.data$pattern != "") |>
      dplyr::count(.data$pattern, sort = TRUE) |>
      dplyr::slice_head(n = 15)

    if (nrow(pattern_df) == 0) {
      cli::cli_inform("No missing data patterns found")
      return(invisible(NULL))
    }

    pattern_df <- pattern_df |>
      dplyr::mutate(pattern = forcats::fct_reorder(.data$pattern, .data$n))

    p <- ggplot2::ggplot(pattern_df, ggplot2::aes(x = .data$pattern, y = .data$n)) +
      ggplot2::geom_col(fill = "#984EA3", alpha = 0.8) +
      ggplot2::labs(
        x = "Missing Variables (indices)",
        y = "Count",
        title = "Missing Data Co-occurrence Patterns",
        subtitle = "Pattern indices refer to variable positions"
      ) +
      theme_oxidizr() +
      ggplot2::coord_flip()
  }

  p
}

#' Plot Validation Time Series
#'
#' Creates a faceted time series plot showing variables with out-of-range
#' values highlighted based on validation thresholds.
#'
#' @param data A data frame or CalorimetryData object.
#' @param thresholds Named list of threshold lists (e.g., calorimetry_thresholds).
#' @param variables Variables to plot (default: auto-detect from thresholds).
#' @param id_col Subject ID column name (default: "id").
#' @param time_col Time column name (default: "time").
#' @param show_thresholds Logical, show threshold lines (default: TRUE).
#'
#' @return A ggplot2 object
#' @export
plot_validation_timeseries <- function(data,
                                        thresholds = NULL,
                                        variables = NULL,
                                        id_col = "id",
                                        time_col = "time",
                                        show_thresholds = TRUE) {
  # Extract data
  if (S7_inherits(data, CalorimetryData)) {
    id_col <- data@id_col
    time_col <- data@time_col
    df <- data@data
    if (is.null(thresholds)) thresholds <- calorimetry_thresholds
  } else {
    df <- data
    if (is.null(thresholds)) thresholds <- calorimetry_thresholds
  }

  # Get variables from thresholds if not specified
  if (is.null(variables)) {
    variables <- names(thresholds)
  }
  variables <- intersect(variables, names(df))

  if (length(variables) == 0) {
    cli::cli_abort("No matching variables found between data and thresholds")
  }

  # Pivot to long format
  plot_df <- df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variable",
      values_to = "value"
    )

  # Add out-of-range flag
  plot_df <- plot_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      out_of_range = {
        thresh <- thresholds[[.data$variable]]
        if (is.null(thresh) || is.na(.data$value)) {
          FALSE
        } else {
          .data$value < thresh$min || .data$value > thresh$max
        }
      }
    ) |>
    dplyr::ungroup()

  # Build threshold reference data for geom_hline
  if (show_thresholds) {
    thresh_lines <- purrr::map_dfr(variables, function(v) {
      th <- thresholds[[v]]
      if (!is.null(th)) {
        tibble::tibble(
          variable = v,
          yintercept = c(th$min, th$max),
          type = c("min", "max")
        )
      }
    })
  }

  # Plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[time_col]], y = .data$value)) +
    ggplot2::geom_line(ggplot2::aes(group = .data[[id_col]]), color = "grey60", alpha = 0.3) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$out_of_range),
      size = 1, alpha = 0.7
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#E41A1C", "FALSE" = "#377EB8"),
      labels = c("TRUE" = "Out of range", "FALSE" = "Normal"),
      name = ""
    ) +
    ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2)

  if (show_thresholds && exists("thresh_lines") && nrow(thresh_lines) > 0) {
    p <- p +
      ggplot2::geom_hline(
        data = thresh_lines,
        ggplot2::aes(yintercept = .data$yintercept),
        linetype = 2, color = "darkred", alpha = 0.5
      )
  }

  p <- p +
    ggplot2::labs(
      x = "Time (min)",
      y = "Value",
      title = "Variable Time Series with Threshold Validation"
    ) +
    theme_oxidizr()

  p
}
