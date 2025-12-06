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
    labels <- c("Exogenous CHO", "Endogenous CHO", "Fat", "Protein")
    colors <- c("#E41A1C", "#FF7F00", "#FFFF33", "#000000")
  } else {
    pct_cols <- c("pct_cho_total", "pct_fat", "pct_protein")
    labels <- c("Total CHO", "Fat", "Protein")
    colors <- c("#FFC0CB", "#FFFF33", "#000000")
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
