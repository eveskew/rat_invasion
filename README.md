# Impacts of black rat invasion on the primary rodent host of Lassa virus, _Mastomys natalensis_

This repository contains code, data, and figures that support:

Eskew, E.A., B.H. Bird, B.M. Ghersi, J. Bangura, A.J. Basinski, E. Amara, M.A. Bah, M.C. Kanu, O.T. Kanu, E.G. Lavalie, V. Lungay, W. Robert, M.A. Vandi, E. Fichet-Calvet, and S.L. Nuismer. 2024. [Reservoir displacement by an invasive rodent reduces Lassa virus zoonotic spillover risk](https://doi.org/10.1038/s41467-024-47991-1). Nature Communications 15: 3589.

--- 

### Repository Structure

- [`/R`](/R) contains code for custom functions that are sourced in the analysis scripts

- [`/data`](/data) contains data files necessary for the analysis, including:

  - A clean data subdirectory ([`/clean`](/data/clean)) with:
  
    - Cleaned site- and visit-level rodent trapping data for Sierra Leone and Guinea ([`/combined`](/data/clean/combined))
    
    - Cleaned house-level rodent trapping data for Sierra Leone ([`/house`](/data/clean/house))
    
    - Cleaned rodent trapping data for occupancy modeling of house-level data from Sierra Leone ([`/occupancy`](/data/clean/occupancy))

  - Data used for data cleaning ([`/cleaning_tables`](/data/cleaning_tables))
  
  - A raw data subdirectory ([`/raw`](/data/raw)) with:
    
    - Data on _Mastomys natalensis_ catch per trap from prior studies that were used to formulate an informative prior for Bayesian modeling ([`/catch_per_trap`](/data/raw/catch_per_trap))
    
    - Data manually extracted from the [McCormick et al. 1987](https://doi.org/10.1093/infdis/155.3.437) manuscript ([`/McCormick`](/data/raw/McCormick))
    
    - GenBank accession codes for Lassa virus sequence data from _Mastomys natalensis_ collected in both Sierra Leone and Guinea ([`/sequencing_tables`](/data/raw/sequencing_tables))
  
  - Source data that can be used to recreate the main text figures ([`/source_data`](/data/source_data))

- [`/outputs`](/outputs) contains all main text figures (and a handful of others) output from the analysis scripts

  - The [`/supplementary`](/outputs/supplementary) subdirectory contains figures that appear in the manuscript's Supplementary Information file

- [`/scripts`](/scripts) contains all analysis scripts, ordered sequentially by number

- [`/stan_models`](/stan_models) contains the Stan code sourced in the analysis scripts, including:

  - A visit-level model of _Mastomys natalensis_ count with a site-level rodent presence predictor ([`visit_model.stan`](/stan_models/visit_model.stan))
  
  - A house-level model of _Mastomys natalensis_ count with a site-level rodent presence predictor ([`house_model_site_predictor.stan`](/stan_models/house_model_site_predictor.stan))
  
  - An alternative house-level model of _Mastomys natalensis_ count with a house-level rodent presence predictor ([`house_model_house_predictor.stan`](/stan_models/house_model_house_predictor.stan))
  
  - A visit-level zoonotic spillover risk model of Lassa-positive _Mastomys natalensis_ count with a site-level rodent presence predictor ([`spillover_risk_visit_level.stan`](/stan_models/spillover_risk_visit_level.stan))
  
  
---

### Notes on Reproducibility

Site-, visit-, and house-level data analyses and statistical modeling for this project (using the respective analysis scripts) are reproducible given the cleaned data in the [`data/clean`](/data/clean) subdirectory. Large raster data files necessary to reproduce the supplementary environmental analyses ([`07_environmental_analyses.R`](scripts/07_environmental_analyses.R) script) have not been uploaded to GitHub because of file sizes but are available on [the project's Zenodo repository](https://doi.org/10.5281/zenodo.10946459).