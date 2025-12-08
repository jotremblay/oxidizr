# Analyze Oxidation (Main Workflow)

Main analysis function that performs complete oxidation analysis.

## Usage

``` r
analyze_oxidation(
  study,
  time_range = NULL,
  aggregate = FALSE,
  calc_energy = TRUE,
  control_protocol = NULL,
  validate = TRUE,
  strict = FALSE,
  validation_thresholds = NULL
)
```

## Arguments

- study:

  An OxidationStudy object

- time_range:

  Optional time range to filter (vector of 2)

- aggregate:

  Logical, aggregate by time point (default: FALSE)

- calc_energy:

  Logical, calculate energy contributions (default: TRUE)

- control_protocol:

  Name of control protocol for Rref (required if isotopes present)

- validate:

  Logical, whether to validate data before analysis (default: TRUE)

- strict:

  Logical, whether to stop on validation errors (default: FALSE). If
  FALSE (default), warnings are issued but analysis continues. If TRUE,
  analysis stops if validation errors are found.

- validation_thresholds:

  Optional custom thresholds list (see calorimetry_thresholds)

## Value

An OxidationResults S7 object

## Examples

``` r
if (FALSE) { # \dontrun{
study <- oxidation_study(
  calorimetry = calo_data,
  isotopes = iso_data,
  subjects = subject_data
)
results <- analyze_oxidation(study, time_range = c(30, 120))

# With strict validation
results <- analyze_oxidation(study, validate = TRUE, strict = TRUE)

# Skip validation
results <- analyze_oxidation(study, validate = FALSE)
} # }
```
