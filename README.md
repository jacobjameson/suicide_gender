# Suicide Gender Analysis Project

Analysis of U.S. suicide rates by gender, age, and firearm usage.

## Structure

```
.
├── outputs/            # Processed data, figures, and tables
├── raw/                # Raw data files
│   ├── WISQARS/        # 2001-2022 suicide data
│   ├── WONDER/         # 2018-2022 state-level data
│   └── scorecards/     # Annual state policy data
└── src/                # R scripts for analysis
```

## Data Sources

- WISQARS: Web-based Injury Statistics Query and Reporting System (2001-2022)
- WONDER: Wide-ranging ONline Data for Epidemiologic Research (2018-2022)
- Scorecards: State-level policy data (2018-2022)

## Scripts

- `prepare_WISQARS.R`: Process WISQARS data
- `prepare_WONDER.R`: Process WONDER data
- `figures.R`: Generate figures
- `tables.R`: Produce tables

## Usage

1. Clone repository
2. Open `suicide_gender.Rproj` in RStudio
3. Run scripts in `src/` in order:
   - `prepare_WISQARS.R`
   - `prepare_WONDER.R`
   - `figures.R`
   - `tables.R`

## Requirements

- R (4.0.0+)
- RStudio
- Packages: tidyverse, ggplot2 

## Contact

jacobjameson@g.harvard.edu
