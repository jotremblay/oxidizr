# Calculate Substrate Oxidation and Energy Contributions

Performs oxidation analysis including protein oxidation (mass balance),
substrate oxidation (Jequier equations), CHO partitioning (isotope
methods), and energy contributions.

## Usage

``` r
tar_oxidation_analysis(cleaned_data, exercise_duration = 120)
```

## Arguments

- cleaned_data:

  Output from
  [`tar_import_data()`](https://jotremblay.github.io/oxidizr/reference/tar_import_data.md)

- exercise_duration:

  Duration of exercise in minutes (default: 120)

## Value

A list containing:

- oxidation:

  Complete oxidation data with CHO partitioning

- oxidation_last_hour:

  Filtered to last hour (time \> 30)

- energy:

  Energy yield calculations

- protein_oxidation:

  Protein oxidation rates

- cho_exo:

  Exogenous CHO oxidation data

- cho_pla:

  Plasma glucose oxidation data

- oxidation_summary:

  Summary by protocol

- timecourse_summary:

  Time course summary

- oxidation_timecourse:

  Oxidation rates over time

- energy_contribution_summary:

  Energy percentages by protocol

- rexp_summary:

  Expired 13CO2 summary

- rpla_summary:

  Plasma 13C-glucose summary
