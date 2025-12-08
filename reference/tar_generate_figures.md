# Generate Publication Figures

Creates ggplot2 figures for oxidation study results.

## Usage

``` r
tar_generate_figures(cleaned_data, oxidation_results, output_dir = "plots")
```

## Arguments

- cleaned_data:

  Output from
  [`tar_import_data()`](https://jotremblay.github.io/oxidizr/reference/tar_import_data.md)

- oxidation_results:

  Output from
  [`tar_oxidation_analysis()`](https://jotremblay.github.io/oxidizr/reference/tar_oxidation_analysis.md)

- output_dir:

  Directory to save figure files (default: "plots")

## Value

A list containing ggplot objects and styling parameters
