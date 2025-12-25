# Calculate Substrate Oxidation

Calculate carbohydrate and fat oxidation rates from indirect calorimetry
using stoichiometric equations.

## Usage

``` r
calc_substrate_oxidation(
  calo,
  protein_ox = NULL,
  coefficients = stoich_coefficients,
  na_protein_action = c("mean", "zero", "error")
)
```

## Arguments

- calo:

  A CalorimetryData object

- protein_ox:

  Optional protein oxidation data (from calc_protein_oxidation)

- coefficients:

  Stoichiometric coefficients (default: stoich_coefficients)

- na_protein_action:

  How to handle NA values in protein oxidation data. One of:

  - `"mean"` (default): Replace NA with group mean, then 0 if all NA

  - `"zero"`: Replace NA with 0 (assume negligible protein contribution)

  - `"error"`: Raise an error if any NA values are present

## Value

A tibble with CHO and fat oxidation rates (g/min)

## Details

Uses equations commonly attributed to Jequier et al. and Frayn, with
specific coefficients parameterized as presented in Telmosse (2022):

Non-protein calculations:

- VO2np = VO2 - (protein_ox \* 1.0075)

- VCO2np = VCO2 - (protein_ox \* 0.8443)

Substrate oxidation:

- CHO (g/min) = 4.618 \* VCO2np - 3.279 \* VO2np

- Fat (g/min) = -1.712 \* VCO2np + 1.712 \* VO2np

### NA Handling Strategies

When protein oxidation data contains NA values, the `na_protein_action`
parameter controls behavior:

- **"mean"**: Imputes missing values with the mean of available protein
  oxidation values. If all values are NA, uses 0. This preserves the
  group average contribution but may not be appropriate for studies with
  large inter-subject variability.

- **"zero"**: Treats missing protein oxidation as zero contribution.
  Appropriate when protein contribution is expected to be negligible (\<
  5% of total energy) or when conservative estimates are preferred.

- **"error"**: Raises an error if any NA values are present. Recommended
  for automated pipelines where missing data should halt processing.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic calculation (assuming negligible protein contribution)
ox <- calc_substrate_oxidation(calo_data)

# With protein correction
protein <- calc_protein_oxidation(urea_loss)
ox <- calc_substrate_oxidation(calo_data, protein_ox = protein)

# Strict mode for automated pipelines
ox <- calc_substrate_oxidation(calo_data, protein_ox = protein,
                               na_protein_action = "error")
} # }
```
