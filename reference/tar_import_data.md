# Import and Clean Oxidation Study Data

Loads raw data files and performs cleaning for oxidation analysis.
Returns a list containing S7 objects and cleaned data frames suitable
for use as a targets pipeline target.

## Usage

``` r
tar_import_data(
  data_dir = "data",
  excluded_subjects = NULL,
  excluded_protocols = NULL,
  protocol_labels = NULL
)
```

## Arguments

- data_dir:

  Path to directory containing raw .RData files

- excluded_subjects:

  Character vector of subject IDs to exclude

- excluded_protocols:

  Character vector of protocol names to exclude

- protocol_labels:

  Named character vector mapping protocol codes to labels

## Value

A list containing:

- calo_s7:

  CalorimetryData S7 object

- iso_s7:

  IsotopeData S7 object

- calo_df:

  Cleaned calorimetry data frame

- subjects_clean:

  Cleaned subject characteristics

- prot_data:

  Data for protein oxidation calculation

- Rexo_clean, Rref_clean, Rpla_clean, Rexp_clean:

  Cleaned isotope data

- subject_summary:

  Summary statistics by protocol

- config:

  Configuration parameters used
