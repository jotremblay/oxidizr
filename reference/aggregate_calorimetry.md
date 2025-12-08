# Aggregate Calorimetry Data

Aggregate calorimetry measurements by subject, time, and optionally
protocol.

## Usage

``` r
aggregate_calorimetry(calo, by = NULL, fun = mean)
```

## Arguments

- calo:

  A CalorimetryData object

- by:

  Character vector of grouping columns (uses id_col, time_col,
  protocol_col by default)

- fun:

  Aggregation function (default: mean)

## Value

A CalorimetryData object with aggregated data
