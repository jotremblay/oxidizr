# oxidizr

Substrate Oxidation Analysis During Exercise

## Overview

`oxidizr` is an R package for analyzing substrate oxidation during exercise using indirect calorimetry and stable isotope tracers. It provides:

- **S7 classes** for structured data management
- **Oxidation calculations** for carbohydrate, fat, and protein
- **13C isotope analysis** for exogenous/endogenous CHO partitioning
- **Publication-ready plots** using ggplot2
- **Formatted tables** using gt
- **Quarto report templates** for reproducible analysis

## Installation

```r
# Install from GitHub (once published)
# remotes::install_github("jtremblay/oxidizr")

# Or install locally
devtools::install("path/to/oxidizr")
```

## Quick Start

```r
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
- `read_calorimetry()` - Import calorimetry data from various formats
- `oxidation_study()` - Create an OxidationStudy object

### Calculations
- `calc_substrate_oxidation()` - Calculate CHO and fat oxidation rates
- `calc_protein_oxidation()` - Calculate protein oxidation from urea losses
- `calc_exogenous_cho()` - Calculate exogenous CHO from 13C tracers
- `calc_cho_partition()` - Partition CHO sources (exo/endo, muscle/liver)
- `analyze_oxidation()` - Main analysis workflow

### Visualization
- `plot_oxidation_timecourse()` - Time course of oxidation rates
- `plot_energy_contribution()` - Stacked bar chart of energy sources
- `plot_cho_partition()` - CHO source partitioning
- `plot_isotope_enrichment()` - 13C enrichment over time

### Tables
- `tbl_subjects()` - Subject characteristics
- `tbl_oxidation_summary()` - Summary statistics
- `tbl_energy_contribution()` - Energy contributions by substrate

## S7 Classes

- `CalorimetryData` - VO2, VCO2, RER measurements
- `IsotopeData` - 13C enrichment data
- `UreaData` - Protein oxidation inputs
- `EnvironmentData` - Temperature, humidity, pressure
- `OxidationStudy` - Main container for study data
- `OxidationResults` - Analysis output

## Key Formulas

### Stoichiometric equations
The package derives its coefficients from balanced oxidation reactions, following the step-by-step method of Telmosse (2022), with 22.4 L/mol replaced by 22.414 L/mol (Livesey & Elia, 1988):

- glucose: C6H12O6 + 6 O2 -> 6 CO2 + 6 H2O (0.7465 L O2 and CO2 per g)
- TAG-type (Telmosse, 2022, matched to Institute of Medicine, 2005): C53.4H99.2O6 + 75.2 O2 -> 53.4 CO2 + 49.6 H2O (2.0129 L O2 and 1.4294 L CO2 per g)
- protein: general values of Livesey & Elia (1988), 1.0103 L O2 and 0.8436 L CO2 per g; 2.9154 g protein per g urea

```
CHO (g/min) = 4.6211 * VCO2 - 3.2815 * VO2
Fat (g/min) = 1.7137 * VO2 - 1.7137 * VCO2
```

`gas_yield()` computes the gas volumes for any formula, and `vignette("stoichiometry")` gives the reasoning and sources for each choice. `oxidation_constants()` lets you choose another lipid, molar volume, protein factors or energy values, and `compare_constants()` shows how much the choice changes the results.

### Uncertainty
`analyze_oxidation()` propagates measurement uncertainty (`oxidation_uncertainty()`) and reports, for each rate, its standard uncertainty and its systematic part. `uncertainty_budget()` ranks the sources, and summaries report SD, SE or a t-based CI with the subject as the unit. See `vignette("uncertainty")`.

### Exogenous CHO from 13C
The Mosora equation divides by k, the CO2 produced per gram of glucose, from the same glucose reaction (6 x 22.414 / 180.16 = 0.7465 L/g).

```
CHOexo = VCO2 * ((Rexp - Rref) / (Rexo - Rref)) / 0.7465
```

## References

*   **Institute of Medicine, Food and Nutrition Board.** (2005). *Dietary Reference Intakes for Energy, Carbohydrate, Fiber, Fat, Fatty Acids, Cholesterol, Protein, and Amino Acids*. National Academies Press.
*   **Livesey, G., & Elia, M.** (1988). Estimation of energy expenditure, net carbohydrate utilization, and net fat oxidation and synthesis by indirect calorimetry: evaluation of errors with special reference to the detailed composition of fuels. *The American Journal of Clinical Nutrition*, *47*(4), 608–628.
*   **Péronnet, F., & Massicotte, D.** (1991). Table of nonprotein respiratory quotient: an update. *Canadian Journal of Sport Sciences = Journal Canadien Des Sciences Du Sport*, *16*(1), 23–29.
*   **Telmosse, E.** (2022). *Contribution des glucides exogènes à la fourniture d’énergie lors d’un effort prolongé : analyse par régression multiple*. Mémoire de maîtrise, Université de Montréal.

## License

MIT
