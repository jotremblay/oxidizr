# Plot Missing Data Patterns

Creates a visualization of missing data patterns in the dataset.

## Usage

``` r
plot_missing_data(
  data,
  variables = NULL,
  type = c("heatmap", "bar", "upset"),
  id_col = "id",
  time_col = "time",
  max_subjects = 20
)
```

## Arguments

- data:

  A data frame or CalorimetryData object.

- variables:

  Character vector of variables to check (default: auto-detect).

- type:

  Type of plot: "heatmap" (default), "bar", or "upset".

- id_col:

  Subject ID column name (default: "id").

- time_col:

  Time column name (default: "time").

- max_subjects:

  Maximum subjects for heatmap (default: 20).

## Value

A ggplot2 object

## Examples

``` r
if (FALSE) { # \dontrun{
plot_missing_data(calo_data)
plot_missing_data(calo_data, type = "bar")
} # }
```
