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
-   `R/04b_dic_summary_analysis.R`: Summarizes patterns in dissolved inorganic carbon (DIC) across sites.
-   `R/04d_travertine_rate_model.R`: Calculates travertine formation rates by modeling changes in calcium stocks over time.
-   `R/04e_geochemical_lmer_models.R`: Applies linear mixed-effects models to evaluate the influence of deposition on SI~Calcite~, pCO~2~, and Ca^2+^ mass transfer.
-   `R/04c_coupled_c_org_c_inorg_production.R`: Final integrative analysis linking organic (stream metabolism) and inorganic carbon dynamics (travertine formation).

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
source("R/04c_geochemical_lmer_models.R")
source("R/04d_travertine_rate_model.R")
source("R/04e_coupled_c_org_c_inorg_production.R")  
```

## License

The code and scripts in this repository are licensed under the [MIT License](LICENSE).

The data files are not included in this repository and are available via Zenodo under a separate license (see below).

## Data Availability

The data are licensed under a Creative Commons Attribution 4.0 International (CC BY 4.0) license.

The data required to run the analyses in this project are not included in this GitHub repository due to access restrictions. However, the dataset is archived and preserved in a private Zenodo repository under the following DOI:

Zenodo record (private): https://doi.org/10.5281/zenodo.16584698
(Note: The DOI exists but is not publicly accessible until publication.)

Access to the dataset is provided exclusively for peer review purposes via a private Zenodo link. Reviewers will receive this link directly through the journal’s submission system.

If you are not a reviewer but wish to access the dataset (e.g., for research replication or academic use), please contact the author at n.kauan@gmail.com.

Access requests will be considered on a case-by-case basis to ensure responsible and appropriate use. If approved, a private access link will be provided.

### How to Access the Data

- Request access to the dataset 
- Once approved, download the full dataset (includes both `.csv` and `.xlsx` files).
- Place all downloaded files into the project's `data/` directory.

> The `.csv` files are used directly in the analysis scripts. The corresponding `.xlsx` files contain the same data along with structured metadata (e.g., units, variable descriptions, sources), and are provided for documentation and transparency.

Your `data/` folder should contain the following files:

```         
project/
└── data/
├── stream_metabolism_travertine_formation.csv
├── stream_metabolism_travertine_formation.xlsx
├── probe_data_discrete_sampling.csv
├── probe_data_discrete_sampling.xlsx
├── metabolism_data.csv
├── metabolism_data.xlsx
├── hydraulic_data.csv
├── hydraulic_data.xlsx
├── high_frequency_data.csv
├── high_frequency_data.xlsx
├── geochemical_data.csv
├── geochemical_data.xlsx
├── calcium_stock_variation_linear_segments.csv
├── calcium_stock_variation_linear_segments.xlsx
├── calcium_specific_conductivity_data.csv
├── calcium_specific_conductivity_data.xlsx
├── calcium_mass_transfer_reactions.csv
├── calcium_mass_transfers_reactions.xlsx
├── alkalinity.csv
├── alkalinity.xlsx
├── alkalinity_calcium_coupling.csv
└── alkalinity_calcium_coupling.xlsx
```
## Citation

If you use the code or data from this repository in your research, please cite it appropriately.

This repository includes a [`CITATION.cff`](CITATION.cff) file with full citation metadata. You can use it to generate citations in BibTeX, APA, or other formats using GitHub’s **“Cite this repository”** button.

For convenience, you may cite this repository as:

> Fonseca, K. N., Corman, J. R., dos Santos, R. C. L., Neres-Lima, V., Thomas, S. A., Moulton, T. P., & Zandonà, E. (2025). *Coupling of Stream Metabolism and Calcium Carbonate Dynamics in Neotropical Karst Streams* [Dataset and analysis scripts]. GitHub. https://github.com/kauanfonseca/Coupling-of-Stream-Metabolism-and-Calcium-Carbonate-Dynamics-in-Neotropical-Karst-Streams-

## Project Structure

```         
project/
├── data/           # Raw, unaltered input data (not included; download from Zenodo)
├── incremental/    # Intermediate or processed datasets
├── figs/           # Figures generated from the analysis
├── results/        # Tables, model outputs, etc.
├── R/              # Core or auxiliary R scripts/functions
├── README.Rmd      # Detailed project description (compiled to README.md)
├── LICENSE         # MIT license for code reuse
└── CITATION.cff    # Citation file for properly citing this repository
```
