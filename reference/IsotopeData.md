# Isotope Data Class

S7 class for storing 13C isotope enrichment data for tracer studies.

## Arguments

- rexp:

  Data frame with expired CO2 13C enrichment (delta per mil)

- rexo:

  Data frame with exogenous substrate 13C enrichment

- rref:

  Data frame with reference/baseline 13C enrichment

- rpla:

  Data frame with plasma glucose 13C enrichment (optional)

- id_col:

  Name of the subject ID column

- time_col:

  Name of the time column

- protocol_col:

  Name of the protocol column (optional)

- rexp_col:

  Name of the rexp column in rexp data frame (default: "rexp")

- rexo_col:

  Name of the rexo column in rexo data frame (default: "rexo")

- rref_col:

  Name of the rref column in rref data frame (default: "rref")

- rpla_col:

  Name of the rpla column in rpla data frame (default: "rpla")

## Value

An IsotopeData S7 object
