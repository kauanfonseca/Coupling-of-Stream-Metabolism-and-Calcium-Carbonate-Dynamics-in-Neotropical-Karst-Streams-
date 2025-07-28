## README

This repository contains the full reproducible workflow for the manuscript: “Coupling of Stream Metabolism and Calcium Carbonate Dynamics in Neotropical Karst Streams”.

## Analysis Workflow

The scripts are organized to reflect the structure of the manuscript, progressing from physical measurements to biogeochemical modeling.

### 1. Hydraulic Data

-   `R/01_hydraulic_analysis.R`: Processes and visualizes hydraulic metrics such as stream discharge and flow velocity.

### 2. Discrete Water Sampling

-   `R/02_discrete_sampling_analysis.R`: Cleans and analyzes discrete and probe-based water quality measurements.
-   `R/02b_calibration_conductivity_calcium.R`: Calibrates specific conductivity as a proxy for calcium concentration.
-   `R/02c_temporal_trends_streams.R`: Assesses seasonal and spatial trends in key water quality parameters.

### 3. Stream Metabolism

-   `R/03a_metabolism_modeling.R`: Estimates gross primary production (GPP), ecosystem respiration (ER), and net ecosystem production (NEP).
-   `R/03b_compare_stream_metabolism.R`: Compares metabolic rates across stream reaches and conditions.
-   `R/03c_scale_metabolism.R`: Upscales reach-level metabolism to broader ecosystem or network scales.

### 4. Travertine Formation Proxies and Rates

-   `R/04a_alkalinity_calcium_coupling.R`: Analyzes the coupling between alkalinity and calcium as indicators of carbonate precipitation.
-   `R/04b_dic_summary_analysis.R`: Summarizes patterns in dissolved inorganic carbon (DIC) across sites and time.
-   `R/04d_travertine_rate_model.R`: Calculates travertine formation rates by modeling changes in calcium stocks over time.
-   `R/04e_geochemical_lmer_models.R`: Applies linear mixed-effects models to evaluate the influence of deposition on SI~Calcite~, pCO~2~, and Ca^2+^ mass transfer.
-   `R/04c_coupled_c_org_c_inorg_production.R`: Final integrative analysis linking organic and inorganic carbon dynamics to travertine formation.

## R Packages Used

The analysis relies on the following R packages:

### Data Manipulation & Cleaning

-   `dplyr`
-   `tidyverse`
-   `lubridate`
-   `broom`

### Statistical Modeling

-   `lme4`
-   `lmerTest`
-   `performance`
-   `car`
-   `R2jags`
-   `BASEmetab`

### Visualization

-   `ggplot2`
-   `ggpubr`
-   `cowplot`
-   `patchwork`
-   `scales`
-   `gridExtra`

### Reporting & Output

-   `openxlsx`
-   `writexl`
-   `officer`
-   `flextable`

All packages can be installed via CRAN using:

``` r
install.packages(c(
  "dplyr", "tidyverse", "lubridate", "broom", "lme4", "lmerTest", "performance",
  "car", "R2jags", "BASEmetab", "ggplot2", "ggpubr", "cowplot", "patchwork", 
  "scales", "gridExtra", "openxlsx", "writexl", "officer", "flextable"
))
```

### Reproducibility

To rerun the full analysis:

``` r
source("R/01_hydraulic_analysis.R")
source("R/02_discrete_sampling_analysis.R")
source("R/02b_calibration_conductivity_calcium.R")
source("R/02c_temporal_trends_streams.R")
source("R/03a_metabolism_modeling.R")
source("R/03b_compare_stream_metabolism.R")
source("R/03c_scale_metabolism.R")
source("R/04a_alkalinity_calcium_coupling.R")
source("R/04b_dic_summary_analysis.R")
source("R/04d_travertine_rate_model.R")
source("R/04e_geochemical_lmer_models.R")
source("R/04c_coupled_c_org_c_inorg_production.R")  # Final step
```

## Data Availability

All clean datasets and metadata used in this project are publicly available via the Mendeley Data Repository at:

DOI: <https://doi.org/10.17632/xmn2xpyzd6.1>

## Project Structure

├── data/ \# Raw and processed data (or linked externally)

├── figs/ \# Figures generated from analysis

├── results/ \# Analysis outputs (tables, model results, etc.)

├── scripts/ \# Additional R scripts or functions (optional)

├── README.Rmd \# This README file

└── LICENSE \# Project license
