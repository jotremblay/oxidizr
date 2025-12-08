# Package index

## Data Classes

S7 classes for organizing calorimetry and isotope data

- [`CalorimetryData`](https://jotremblay.github.io/oxidizr/reference/CalorimetryData.md)
  : Calorimetry Data Class
- [`IsotopeData`](https://jotremblay.github.io/oxidizr/reference/IsotopeData.md)
  : Isotope Data Class
- [`UreaData`](https://jotremblay.github.io/oxidizr/reference/UreaData.md)
  : Urea Data Class
- [`EnvironmentData`](https://jotremblay.github.io/oxidizr/reference/EnvironmentData.md)
  : Environment Data Class
- [`SubjectData`](https://jotremblay.github.io/oxidizr/reference/SubjectData.md)
  : Subject Data Class
- [`OxidationStudy`](https://jotremblay.github.io/oxidizr/reference/OxidationStudy.md)
  : Oxidation Study Class
- [`OxidationResults`](https://jotremblay.github.io/oxidizr/reference/OxidationResults.md)
  : Oxidation Results Class
- [`ValidationResult`](https://jotremblay.github.io/oxidizr/reference/ValidationResult.md)
  : Validation Result Class

## Main Analysis Workflow

High-level functions for complete analysis

- [`oxidation_study()`](https://jotremblay.github.io/oxidizr/reference/oxidation_study.md)
  : Create Oxidation Study Object
- [`analyze_oxidation()`](https://jotremblay.github.io/oxidizr/reference/analyze_oxidation.md)
  : Analyze Oxidation (Main Workflow)

## Data Validation

Functions for validating data quality

- [`validate_study()`](https://jotremblay.github.io/oxidizr/reference/validate_study.md)
  : Validate Oxidation Study
- [`validate_calorimetry()`](https://jotremblay.github.io/oxidizr/reference/validate_calorimetry.md)
  : Validate Calorimetry Data
- [`validate_isotopes()`](https://jotremblay.github.io/oxidizr/reference/validate_isotopes.md)
  : Validate Isotope Data
- [`validate_environment()`](https://jotremblay.github.io/oxidizr/reference/validate_environment.md)
  : Validate Environment Data
- [`validate_urea()`](https://jotremblay.github.io/oxidizr/reference/validate_urea.md)
  : Validate Urea Data
- [`check_consistency()`](https://jotremblay.github.io/oxidizr/reference/check_consistency.md)
  : Check Cross-Dataset Consistency
- [`calorimetry_thresholds`](https://jotremblay.github.io/oxidizr/reference/calorimetry_thresholds.md)
  : Calorimetry Validation Thresholds
- [`isotope_thresholds`](https://jotremblay.github.io/oxidizr/reference/isotope_thresholds.md)
  : Isotope Validation Thresholds
- [`environment_thresholds`](https://jotremblay.github.io/oxidizr/reference/environment_thresholds.md)
  : Environment Validation Thresholds
- [`urea_thresholds`](https://jotremblay.github.io/oxidizr/reference/urea_thresholds.md)
  : Urea Validation Thresholds

## Steady-State Detection

Functions for identifying steady-state periods

- [`detect_steady_state()`](https://jotremblay.github.io/oxidizr/reference/detect_steady_state.md)
  : Detect Steady-State Using Combined Methods
- [`detect_steady_state_cv()`](https://jotremblay.github.io/oxidizr/reference/detect_steady_state_cv.md)
  : Detect Steady-State Using Coefficient of Variation
- [`detect_steady_state_variance()`](https://jotremblay.github.io/oxidizr/reference/detect_steady_state_variance.md)
  : Detect Steady-State Using Rolling Variance
- [`summarize_steady_state()`](https://jotremblay.github.io/oxidizr/reference/summarize_steady_state.md)
  : Summarize Steady-State Periods
- [`filter_steady_state()`](https://jotremblay.github.io/oxidizr/reference/filter_steady_state.md)
  : Filter Data to Steady-State Periods
- [`calc_steady_state_stats()`](https://jotremblay.github.io/oxidizr/reference/calc_steady_state_stats.md)
  : Calculate Steady-State Statistics

## Oxidation Calculations

Functions for calculating substrate oxidation rates

- [`calc_substrate_oxidation()`](https://jotremblay.github.io/oxidizr/reference/calc_substrate_oxidation.md)
  : Calculate Substrate Oxidation
- [`calc_cho_partition()`](https://jotremblay.github.io/oxidizr/reference/calc_cho_partition.md)
  : Calculate CHO Source Partitioning
- [`calc_exogenous_cho()`](https://jotremblay.github.io/oxidizr/reference/calc_exogenous_cho.md)
  : Calculate Exogenous CHO Oxidation
- [`calc_plasma_cho()`](https://jotremblay.github.io/oxidizr/reference/calc_plasma_cho.md)
  : Calculate Plasma CHO Oxidation
- [`calc_fexo()`](https://jotremblay.github.io/oxidizr/reference/calc_fexo.md)
  : Calculate Fraction Exogenous (Fexo)
- [`calc_rref()`](https://jotremblay.github.io/oxidizr/reference/calc_rref.md)
  : Calculate Reference Enrichment (Rref)
- [`stoich_coefficients`](https://jotremblay.github.io/oxidizr/reference/stoich_coefficients.md)
  : Stoichiometric coefficients

## Protein Calculations

Functions for protein oxidation from urea

- [`calc_protein_oxidation()`](https://jotremblay.github.io/oxidizr/reference/calc_protein_oxidation.md)
  : Calculate Protein Oxidation from Urea
- [`calc_protein_gas_exchange()`](https://jotremblay.github.io/oxidizr/reference/calc_protein_gas_exchange.md)
  : Calculate Protein Contribution to Gas Exchange
- [`calc_sweat_loss()`](https://jotremblay.github.io/oxidizr/reference/calc_sweat_loss.md)
  : Calculate Sweat Loss
- [`calc_urea_loss()`](https://jotremblay.github.io/oxidizr/reference/calc_urea_loss.md)
  : Calculate Urea Loss
- [`estimate_protein_simple()`](https://jotremblay.github.io/oxidizr/reference/estimate_protein_simple.md)
  : Estimate Protein Oxidation (Simple Method)

## Energy Calculations

Functions for energy yield and contributions

- [`calc_energy_yield()`](https://jotremblay.github.io/oxidizr/reference/calc_energy_yield.md)
  : Calculate Energy Yield
- [`calc_energy_percent()`](https://jotremblay.github.io/oxidizr/reference/calc_energy_percent.md)
  : Calculate Energy Percentages
- [`calc_total_energy()`](https://jotremblay.github.io/oxidizr/reference/calc_total_energy.md)
  : Calculate Total Energy Expenditure
- [`calc_cumulative_energy()`](https://jotremblay.github.io/oxidizr/reference/calc_cumulative_energy.md)
  : Calculate Cumulative Energy
- [`summarize_energy()`](https://jotremblay.github.io/oxidizr/reference/summarize_energy.md)
  : Summarize Energy Contributions
- [`energy_factors`](https://jotremblay.github.io/oxidizr/reference/energy_factors.md)
  : Energy conversion factors

## Data Import

Functions for reading calorimetry data from various formats

- [`read_calorimetry()`](https://jotremblay.github.io/oxidizr/reference/read_calorimetry.md)
  : Read Calorimetry Data

## Data Processing

Functions for filtering and aggregating data

- [`filter_time_range()`](https://jotremblay.github.io/oxidizr/reference/filter_time_range.md)
  : Filter Calorimetry Data by Time
- [`aggregate_calorimetry()`](https://jotremblay.github.io/oxidizr/reference/aggregate_calorimetry.md)
  : Aggregate Calorimetry Data
- [`normalize_gas_units()`](https://jotremblay.github.io/oxidizr/reference/normalize_gas_units.md)
  : Normalize VO2/VCO2 Units
- [`calc_rer()`](https://jotremblay.github.io/oxidizr/reference/calc_rer.md)
  : Calculate RER from Calorimetry Data
- [`as_numeric_time()`](https://jotremblay.github.io/oxidizr/reference/as_numeric_time.md)
  : Convert Time to Numeric

## Visualization

ggplot2-based plotting functions

- [`theme_oxidizr()`](https://jotremblay.github.io/oxidizr/reference/theme_oxidizr.md)
  : Publication Theme
- [`plot_oxidation_timecourse()`](https://jotremblay.github.io/oxidizr/reference/plot_oxidation_timecourse.md)
  : Plot Oxidation Time Course
- [`plot_rer_timecourse()`](https://jotremblay.github.io/oxidizr/reference/plot_rer_timecourse.md)
  : Plot RER Time Course
- [`plot_vo2_timecourse()`](https://jotremblay.github.io/oxidizr/reference/plot_vo2_timecourse.md)
  : Plot VO2 Time Course
- [`plot_energy_contribution()`](https://jotremblay.github.io/oxidizr/reference/plot_energy_contribution.md)
  : Plot Energy Contribution
- [`plot_cho_partition()`](https://jotremblay.github.io/oxidizr/reference/plot_cho_partition.md)
  : Plot CHO Partition
- [`plot_isotope_enrichment()`](https://jotremblay.github.io/oxidizr/reference/plot_isotope_enrichment.md)
  : Plot Isotope Enrichment
- [`plot_data_quality()`](https://jotremblay.github.io/oxidizr/reference/plot_data_quality.md)
  : Plot Data Quality Overview
- [`plot_steady_state()`](https://jotremblay.github.io/oxidizr/reference/plot_steady_state.md)
  : Plot Steady-State Analysis
- [`plot_outliers()`](https://jotremblay.github.io/oxidizr/reference/plot_outliers.md)
  : Plot Outliers
- [`plot_missing_data()`](https://jotremblay.github.io/oxidizr/reference/plot_missing_data.md)
  : Plot Missing Data Patterns
- [`plot_validation_timeseries()`](https://jotremblay.github.io/oxidizr/reference/plot_validation_timeseries.md)
  : Plot Validation Time Series

## Tables

Publication-ready table generation with gt

- [`tbl_subjects()`](https://jotremblay.github.io/oxidizr/reference/tbl_subjects.md)
  : Subject Characteristics Table
- [`tbl_oxidation_summary()`](https://jotremblay.github.io/oxidizr/reference/tbl_oxidation_summary.md)
  : Oxidation Summary Table
- [`tbl_energy_contribution()`](https://jotremblay.github.io/oxidizr/reference/tbl_energy_contribution.md)
  : Energy Contribution Table
- [`tbl_validation_summary()`](https://jotremblay.github.io/oxidizr/reference/tbl_validation_summary.md)
  : Validation Summary Table
- [`tbl_data_completeness()`](https://jotremblay.github.io/oxidizr/reference/tbl_data_completeness.md)
  : Data Completeness Table
- [`tbl_outliers()`](https://jotremblay.github.io/oxidizr/reference/tbl_outliers.md)
  : Outlier Summary Table
- [`tbl_steady_state()`](https://jotremblay.github.io/oxidizr/reference/tbl_steady_state.md)
  : Steady-State Summary Table
- [`tbl_stats()`](https://jotremblay.github.io/oxidizr/reference/tbl_stats.md)
  : Statistical Results Table
- [`export_table_xlsx()`](https://jotremblay.github.io/oxidizr/reference/export_table_xlsx.md)
  : Export Table to Excel

## Reports

Automated report generation

- [`render_oxidation_report()`](https://jotremblay.github.io/oxidizr/reference/render_oxidation_report.md)
  : Render Oxidation Report
- [`render_quality_report()`](https://jotremblay.github.io/oxidizr/reference/render_quality_report.md)
  : Render Quality Report
- [`create_report_template()`](https://jotremblay.github.io/oxidizr/reference/create_report_template.md)
  : Create Report Template
- [`create_quality_report_template()`](https://jotremblay.github.io/oxidizr/reference/create_quality_report_template.md)
  : Create Quality Report Template

## targets Integration

Functions for use with the targets package

- [`tar_generate_figures()`](https://jotremblay.github.io/oxidizr/reference/tar_generate_figures.md)
  : Generate Publication Figures
- [`tar_import_data()`](https://jotremblay.github.io/oxidizr/reference/tar_import_data.md)
  : Import and Clean Oxidation Study Data
- [`tar_oxidation_analysis()`](https://jotremblay.github.io/oxidizr/reference/tar_oxidation_analysis.md)
  : Calculate Substrate Oxidation and Energy Contributions
- [`tar_statistical_analysis()`](https://jotremblay.github.io/oxidizr/reference/tar_statistical_analysis.md)
  : Run Statistical Analysis on Oxidation Data

## Generics and Methods

S7 generic functions

- [`get_data()`](https://jotremblay.github.io/oxidizr/reference/get_data.md)
  : Extract data from oxidizr objects

## Utilities

Helper functions

- [`format_mean_sd()`](https://jotremblay.github.io/oxidizr/reference/format_mean_sd.md)
  : Format Mean +/- SD
- [`list_columns()`](https://jotremblay.github.io/oxidizr/reference/list_columns.md)
  : List Available Columns
- [`quick_summary()`](https://jotremblay.github.io/oxidizr/reference/quick_summary.md)
  : Quick Summary of Oxidation Results

## Package Documentation

Internal documentation pages

- [`oxidizr`](https://jotremblay.github.io/oxidizr/reference/oxidizr-package.md)
  [`oxidizr-package`](https://jotremblay.github.io/oxidizr/reference/oxidizr-package.md)
  : oxidizr: Substrate Oxidation Analysis During Exercise
- [`oxidizr-classes`](https://jotremblay.github.io/oxidizr/reference/oxidizr-classes.md)
  : S7 Classes for Oxidation Analysis
- [`oxidizr-generics`](https://jotremblay.github.io/oxidizr/reference/oxidizr-generics.md)
  : S7 Generic Functions for Oxidation Analysis
- [`calorimetry`](https://jotremblay.github.io/oxidizr/reference/calorimetry.md)
  : Calorimetry Data Functions
- [`energy`](https://jotremblay.github.io/oxidizr/reference/energy.md) :
  Energy Calculation Functions
- [`isotopes`](https://jotremblay.github.io/oxidizr/reference/isotopes.md)
  : Isotope Tracer Functions
- [`oxidation`](https://jotremblay.github.io/oxidizr/reference/oxidation.md)
  : Substrate Oxidation Functions
- [`plots`](https://jotremblay.github.io/oxidizr/reference/plots.md) :
  Visualization Functions
- [`protein`](https://jotremblay.github.io/oxidizr/reference/protein.md)
  : Protein Oxidation Functions
- [`reports`](https://jotremblay.github.io/oxidizr/reference/reports.md)
  : Report Generation Functions
- [`steady-state`](https://jotremblay.github.io/oxidizr/reference/steady-state.md)
  : Steady-State Detection Functions
- [`tables`](https://jotremblay.github.io/oxidizr/reference/tables.md) :
  Table Generation Functions
- [`utils`](https://jotremblay.github.io/oxidizr/reference/utils.md) :
  Utility Functions
- [`validation`](https://jotremblay.github.io/oxidizr/reference/validation.md)
  : Data Validation Functions
