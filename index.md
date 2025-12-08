# oxidizr

Substrate Oxidation Analysis During Exercise

## Overview

`oxidizr` is an R package for analyzing substrate oxidation during
exercise using indirect calorimetry and stable isotope tracers. It
provides:

- **S7 classes** for structured data management
- **Oxidation calculations** for carbohydrate, fat, and protein
- **13C isotope analysis** for exogenous/endogenous CHO partitioning
- **Publication-ready plots** using ggplot2
- **Formatted tables** using gt
- **Quarto report templates** for reproducible analysis

## Installation

``` r
# Install from GitHub (once published)
# remotes::install_github("jtremblay/oxidizr")

# Or install locally
devtools::install("path/to/oxidizr")
```

## Quick Start

``` r
library(oxidizr)

# Create a study from your data
study <- oxidation_study(
  calorimetry = read_calorimetry("vo2_data.csv"),
  isotopes = IsotopeData(rexp = rexp_df, rexo = rexo_df, rref = rref_df),
  subjects = subject_info
)

# Analyze oxidation
results <- analyze_oxidation(study, time_range = c(30, 120))

# Visualize
plot_energy_contribution(results)
plot_cho_partition(results)

# Generate tables
tbl_oxidation_summary(results, by = "protocol")

# Render report
render_oxidation_report(results, output_file = "my_analysis.html")
```

## Key Functions

### Data Import

- [`read_calorimetry()`](https://jotremblay.github.io/oxidizr/reference/read_calorimetry.md) -
  Import calorimetry data from various formats
- [`oxidation_study()`](https://jotremblay.github.io/oxidizr/reference/oxidation_study.md) -
  Create an OxidationStudy object

### Calculations

- [`calc_substrate_oxidation()`](https://jotremblay.github.io/oxidizr/reference/calc_substrate_oxidation.md) -
  Calculate CHO and fat oxidation rates
- [`calc_protein_oxidation()`](https://jotremblay.github.io/oxidizr/reference/calc_protein_oxidation.md) -
  Calculate protein oxidation from urea losses
- [`calc_exogenous_cho()`](https://jotremblay.github.io/oxidizr/reference/calc_exogenous_cho.md) -
  Calculate exogenous CHO from 13C tracers
- [`calc_cho_partition()`](https://jotremblay.github.io/oxidizr/reference/calc_cho_partition.md) -
  Partition CHO sources (exo/endo, muscle/liver)
- [`analyze_oxidation()`](https://jotremblay.github.io/oxidizr/reference/analyze_oxidation.md) -
  Main analysis workflow

### Visualization

- [`plot_oxidation_timecourse()`](https://jotremblay.github.io/oxidizr/reference/plot_oxidation_timecourse.md) -
  Time course of oxidation rates
- [`plot_energy_contribution()`](https://jotremblay.github.io/oxidizr/reference/plot_energy_contribution.md) -
  Stacked bar chart of energy sources
- [`plot_cho_partition()`](https://jotremblay.github.io/oxidizr/reference/plot_cho_partition.md) -
  CHO source partitioning
- [`plot_isotope_enrichment()`](https://jotremblay.github.io/oxidizr/reference/plot_isotope_enrichment.md) -
  13C enrichment over time

### Tables

- [`tbl_subjects()`](https://jotremblay.github.io/oxidizr/reference/tbl_subjects.md) -
  Subject characteristics
- [`tbl_oxidation_summary()`](https://jotremblay.github.io/oxidizr/reference/tbl_oxidation_summary.md) -
  Summary statistics
- [`tbl_energy_contribution()`](https://jotremblay.github.io/oxidizr/reference/tbl_energy_contribution.md) -
  Energy contributions by substrate

## S7 Classes

- `CalorimetryData` - VO2, VCO2, RER measurements
- `IsotopeData` - 13C enrichment data
- `UreaData` - Protein oxidation inputs
- `EnvironmentData` - Temperature, humidity, pressure
- `OxidationStudy` - Main container for study data
- `OxidationResults` - Analysis output

## Key Formulas

### Stoichiometric equations

The stoichiometric equations for carbohydrate and fat oxidation are
based on general principles (Livesey & Elia, 1988) with coefficients
parameterized as presented in Telmosse (2022), derived from Institute of
Medicine (2005) data.

    CHO (g/min) = 4.618 * VCO2 - 3.279 * VO2
    Fat (g/min) = -1.712 * VCO2 + 1.712 * VO2

### Exogenous CHO from 13C

The formula for exogenous carbohydrate oxidation utilizes a CO2/CHO
conversion ratio of 0.747 L CO2 per gram CHO oxidized (Péronnet &
Massicotte, 1991; Telmosse, 2022).

    CHOexo = VCO2 * ((Rexp - Rref) / (Rexo - Rref)) / 0.747

## References

- **Institute of Medicine, Food and Nutrition Board.** (2005). *Dietary
  Reference Intakes for Energy, Carbohydrate, Fiber, Fat, Fatty Acids,
  Cholesterol, Protein, and Amino Acids*. National Academies Press.
- **Livesey, G., & Elia, M.** (1988). Estimation of energy expenditure,
  net carbohydrate utilization, and net fat oxidation and synthesis by
  indirect calorimetry: evaluation of errors with special reference to
  the detailed composition of fuels. *The American Journal of Clinical
  Nutrition*, *47*(4), 608–628.
- **Péronnet, F., & Massicotte, D.** (1991). Table of nonprotein
  respiratory quotient: an update. *Canadian Journal of Sport Sciences =
  Journal Canadien Des Sciences Du Sport*, *16*(1), 23–29.
- **Telmosse, E.** (2022). *Contribution des glucides exogènes à la
  fourniture d’énergie lors d’un effort prolongé : analyse par
  régression multiple*. Mémoire de maîtrise, Université de Montréal.

## License

MIT
