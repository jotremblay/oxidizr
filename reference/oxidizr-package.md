# oxidizr: Substrate Oxidation Analysis During Exercise

The oxidizr package provides a complete workflow for analyzing substrate
oxidation during exercise using indirect calorimetry and stable isotope
tracers.

## Analysis Pipeline

The typical analysis workflow consists of:

1.  **Data Import**: Read data files or create S7 data objects

2.  **Data Validation**: Check data quality with
    [`validate_study()`](https://jotremblay.github.io/oxidizr/reference/validate_study.md)

3.  **Steady-State Detection**: Identify valid periods with
    [`detect_steady_state()`](https://jotremblay.github.io/oxidizr/reference/detect_steady_state.md)

4.  **Processing**: Filter and aggregate with
    [`filter_time_range()`](https://jotremblay.github.io/oxidizr/reference/filter_time_range.md),
    [`aggregate_calorimetry()`](https://jotremblay.github.io/oxidizr/reference/aggregate_calorimetry.md)

5.  **Computation**: Calculate oxidation with
    [`analyze_oxidation()`](https://jotremblay.github.io/oxidizr/reference/analyze_oxidation.md)
    or `calc_*()` functions

6.  **Reporting**: Generate outputs with `plot_*()`, `tbl_*()`, and
    `render_*_report()`

## Key Functions

- analyze_oxidation():

  Main analysis function with integrated validation

- validate_study():

  Comprehensive data quality validation

- detect_steady_state():

  Identify steady-state measurement periods

- calc_substrate_oxidation():

  Calculate CHO and fat oxidation rates

- render_quality_report():

  Generate data quality HTML report

## S7 Classes

- CalorimetryData:

  Indirect calorimetry measurements

- IsotopeData:

  13C isotope enrichment data

- OxidationStudy:

  Container for all study data

- OxidationResults:

  Analysis results

- ValidationResult:

  Validation findings

## Vignettes

- [`vignette("getting-started")`](https://jotremblay.github.io/oxidizr/articles/getting-started.md):
  Quick introduction

- [`vignette("analysis-pipeline")`](https://jotremblay.github.io/oxidizr/articles/analysis-pipeline.md):
  Complete workflow guide

- [`vignette("data-quality")`](https://jotremblay.github.io/oxidizr/articles/data-quality.md):
  Validation and quality control

## See also

Useful links:

- <https://jotremblay.github.io/oxidizr/>

- <https://github.com/jotremblay/oxidizr>

- Report bugs at <https://github.com/jotremblay/oxidizr/issues>

## Author

**Maintainer**: Jonathan Tremblay <jonathan.tremblay@umontreal.ca>
