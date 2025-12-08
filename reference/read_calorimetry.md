# Read Calorimetry Data

Import calorimetry data from various file formats.

## Usage

``` r
read_calorimetry(
  file,
  format = c("auto", "csv", "xlsx"),
  id_col = "id",
  time_col = "time",
  vo2_col = "vo2",
  vco2_col = "vco2",
  vo2_unit = "L/min",
  protocol_col = NULL,
  sheet = 1,
  ...
)
```

## Arguments

- file:

  Path to the data file

- format:

  File format: "csv", "xlsx", "auto" (default: "auto")

- id_col:

  Name of the subject ID column

- time_col:

  Name of the time column

- vo2_col:

  Name of the VO2 column

- vco2_col:

  Name of the VCO2 column

- vo2_unit:

  Unit for VO2/VCO2: "L/min" or "mL/min"

- protocol_col:

  Name of the protocol column (optional)

- sheet:

  Sheet name or number for Excel files (default: 1)

- ...:

  Additional arguments passed to read functions

## Value

A CalorimetryData S7 object

## Examples

``` r
if (FALSE) { # \dontrun{
calo <- read_calorimetry("vo2_data.csv",
                         id_col = "subject",
                         time_col = "time_min",
                         vo2_col = "VO2",
                         vco2_col = "VCO2")
} # }
```
