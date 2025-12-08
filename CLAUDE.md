# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`oxidizr` is an R package for analyzing substrate oxidation during exercise using indirect calorimetry and stable isotope tracers. It calculates carbohydrate, fat, and protein oxidation rates with optional partitioning of exogenous vs endogenous carbohydrate sources using 13C isotope methods.

## Development Commands

```r
# Load package for development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-oxidation.R")

# Check package
devtools::check()

# Document (regenerate NAMESPACE and .Rd files)
devtools::document()

# Build package
devtools::build()
```

## Architecture

### S7 Class Hierarchy

The package uses S7 classes (not S4 or R6) for data management. All classes are defined in `R/classes.R`:

- **CalorimetryData** - VO2, VCO2, RER measurements with configurable column mappings
- **IsotopeData** - 13C enrichment data (rexp, rexo, rref, rpla)
- **UreaData** - Sweat/urine urea concentrations for protein oxidation
- **EnvironmentData** - Temperature, humidity, pressure
- **SubjectData** - Subject characteristics and fluid balance
- **OxidationStudy** - Main container combining all data components
- **OxidationResults** - Analysis output (oxidation rates, energy contributions, partitioning)

### Core Calculation Pipeline

1. **Calorimetry** (`R/calorimetry.R`): `read_calorimetry()` → CalorimetryData
2. **Protein oxidation** (`R/protein.R`): `calc_sweat_loss()` → `calc_urea_loss()` → `calc_protein_oxidation()`
3. **Substrate oxidation** (`R/oxidation.R`): `calc_substrate_oxidation()` using stoichiometric equations
4. **Isotope analysis** (`R/isotopes.R`): `calc_rref()` → `calc_exogenous_cho()` → `calc_plasma_cho()`
5. **CHO partitioning** (`R/oxidation.R`): `calc_cho_partition()` for exo/endo/muscle/liver sources
6. **Energy yield** (`R/energy.R`): `calc_energy_yield()` → `calc_energy_percent()`

The main workflow function `analyze_oxidation()` orchestrates this pipeline and returns OxidationResults.

### Key Stoichiometric Constants

Defined in `R/oxidizr-package.R` via `stoich_coefficients`:
- CHO: 4.618 × VCO2 - 3.279 × VO2 (g/min)
- Fat: -1.712 × VCO2 + 1.712 × VO2 (g/min)
- CO2/CHO ratio: 0.747 L CO2 per gram CHO oxidized
- Protein-urea conversion: 2.915 g protein per g urea

### Generic Methods

S7 generics are defined in `R/generics.R`:
- `print()` methods for all classes
- `summary()` for OxidationResults
- `get_data()` to extract underlying tibbles
- `as.data.frame()` conversions

### targets Integration

`R/targets.R` provides helpers for reproducible pipelines:
- `tar_import_data()` - Data import and cleaning
- `tar_oxidation_analysis()` - Full analysis workflow
- `tar_statistical_analysis()` - Mixed ANOVAs (requires afex, emmeans)
- `tar_generate_figures()` - Publication figures

## Code Style

- Use tidyverse functions with native pipe `|>`
- Use `.data[[col]]` pronoun for column references in dplyr
- Validate S7 class inputs with `S7_inherits()`
- Use cli package for user messages (`cli::cli_abort()`, `cli::cli_alert_info()`)
- Return tibbles from calculation functions
- Ensure non-negative oxidation rates with `pmax()`

## Package Maintenance Agents

Custom slash commands for maintaining package quality. Run with `/command-name`.

### Quality Assurance
| Command | Purpose |
|---------|---------|
| `/pkg-check` | Run devtools::check() and fix issues |
| `/pkg-status` | Quick health check (load, tests, git status) |
| `/pkg-audit` | Full audit across all quality dimensions |

### Code Quality
| Command | Purpose |
|---------|---------|
| `/pkg-lint` | Code style and best practices review |
| `/pkg-api` | API design consistency review |
| `/pkg-perf` | Performance profiling and optimization |

### Safety & Dependencies
| Command | Purpose |
|---------|---------|
| `/pkg-security` | Security vulnerability audit |
| `/pkg-deps` | Dependency audit and optimization |

### Documentation & Testing
| Command | Purpose |
|---------|---------|
| `/pkg-docs` | Documentation completeness check |
| `/pkg-tests` | Test coverage and quality analysis |

### Release Management
| Command | Purpose |
|---------|---------|
| `/pkg-cran` | CRAN submission readiness check |
| `/pkg-release` | Prepare for version release |

### Development Workflows
| Command | Purpose |
|---------|---------|
| `/pkg-add-function` | Add new function with proper docs/tests |
| `/pkg-fix-bug` | Systematic bug diagnosis and fix |
| `/pkg-refactor` | Safe code refactoring protocol |
