# Add Validation Issue

Add a validation issue to the issues tibble.

## Usage

``` r
add_issue(
  issues,
  check_id,
  category,
  variable,
  severity,
  message,
  n_affected = 0L,
  n_total = 0L,
  subject_ids = NULL,
  time_points = NULL,
  values = NULL,
  threshold = "",
  reference = ""
)
```

## Arguments

- issues:

  Existing issues tibble

- check_id:

  Unique identifier for this check

- category:

  Category of check (calorimetry, isotope, environment, urea,
  consistency)

- variable:

  Variable being checked

- severity:

  Severity level (error, warning, info)

- message:

  Human-readable message

- n_affected:

  Number of affected observations

- n_total:

  Total number of observations

- subject_ids:

  Vector of affected subject IDs

- time_points:

  Vector of affected time points

- values:

  Sample of problematic values

- threshold:

  Threshold that was violated

- reference:

  Literature reference

## Value

Updated issues tibble
