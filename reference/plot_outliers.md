# Plot Outliers

Creates a time series plot with outliers highlighted based on validation
results.

## Usage

``` r
plot_outliers(
  data,
  validation,
  variables = c("vo2", "vco2", "rer"),
  id_col = "id",
  time_col = "time",
  facet_subjects = NULL,
  max_subjects = 6
)
```

## Arguments

- data:

  A data frame or CalorimetryData object.

- validation:

  A ValidationResult object containing outlier information.

- variables:

  Character vector of variables to plot (default: c("vo2", "vco2",
  "rer")).

- id_col:

  Subject ID column name (default: "id").

- time_col:

  Time column name (default: "time").

- facet_subjects:

  Logical, facet by subject (default: TRUE if multiple subjects).

- max_subjects:

  Maximum subjects to show (default: 6).

## Value

A ggplot2 object

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- validate_calorimetry(calo_data)
plot_outliers(calo_data, validation)
} # }
```
