# Run Statistical Analysis on Oxidation Data

Performs mixed ANOVAs on subject characteristics and oxidation outcomes.

## Usage

``` r
tar_statistical_analysis(cleaned_data, oxidation_results)
```

## Arguments

- cleaned_data:

  Output from
  [`tar_import_data()`](https://jotremblay.github.io/oxidizr/reference/tar_import_data.md)

- oxidation_results:

  Output from
  [`tar_oxidation_analysis()`](https://jotremblay.github.io/oxidizr/reference/tar_oxidation_analysis.md)

## Value

A list containing ANOVA results and summary tables
