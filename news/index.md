# Changelog

## oxidizr 0.1.0

- Initial release

- S7 classes for calorimetry data management:

  - `CalorimetryData` for VO2, VCO2, RER measurements
  - `IsotopeData` for 13C enrichment data
  - `UreaData` for protein oxidation inputs
  - `EnvironmentData` for environmental conditions
  - `SubjectData` for subject characteristics
  - `OxidationStudy` as a container combining all data types
  - `OxidationResults` for analysis outputs

- Substrate oxidation calculations (CHO, FAT, Protein) using
  stoichiometric equations

- 13C isotope tracer analysis for exogenous CHO oxidation

- Publication-ready tables via gt and Excel export

- ggplot2-based visualization functions

- Quarto report template for automated analysis reports

- Targets pipeline integration helpers
