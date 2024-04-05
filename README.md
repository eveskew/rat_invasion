# Impacts of black rat invasion on the primary rodent host of Lassa virus, _Mastomys natalensis_

This repository contains code, data, and figures that support:

Eskew, E.A., B.H. Bird, B.M. Ghersi, J. Bangura, A.J. Basinski, E. Amara, M.A. Bah, M.C. Kanu, O.T. Kanu, E.G. Lavalie, V. Lungay, W. Robert, M.A. Vandi, E. Fichet-Calvet, and S.L. Nuismer. 2024. Reservoir displacement by an invasive rodent reduces Lassa virus zoonotic spillover risk.

--- 

### Repository Structure

- [`/R`](/R) contains code for custom functions that are sourced in the analysis scripts

- [`/data`](/data) contains data files necessary for the analysis, including:

  - Data used for data cleaning ([`/cleaning_tables`](/data/cleaning_tables))
  
  - A raw data subdirectory ([`/raw`](/data/raw)) with:
    
    - Data on _Mastomys natalensis_ catch per trap from prior studies that was used to formulate an informative prior for Bayesian modeling ([`/catch_per_trap`](/data/raw/catch_per_trap))
    
    - Data extracted from the [McCormick et al. 1987](https://doi.org/10.1093/infdis/155.3.437) manuscript ([`/McCormick`](/data/raw/McCormick))
    
    - GenBank accession codes associated with Lassa virus sequence data from _Mastomys natalensis_ collected in both Sierra Leone and Guinea ([`/sequencing_tables`](/data/raw/sequencing_tables))
  
  - Source data that can be used to recreate the main text figures ([`/source_data](/data/source_data))

- [`/outputs`](/outputs) contains all main text figures (and a handful of others) output from the analysis scripts

  - The [`/supplementary`](/outputs/supplementary) subdirectory contains figures that appear in the manuscript's Supplementary Information file

- [`/scripts`](/scripts) contains all analysis scripts, ordered sequentially by number

- [`/stan_models`](/stan_models) contains the Stan code sourced in the analysis scripts, including:

  - A visit-level model of _Mastomys natalensis_ count with a site-level rodent presence predictor ([`visit_model.stan`](/stan_models/visit_model.stan))
  
  - A house-level model of _Mastomys natalensis_ count with a site-level rodent presence predictor ([`house_model_site_predictor.stan`](/stan_models/house_model_site_predictor.stan))
  
  - An alternative house-level model of _Mastomys natalensis_ count with a house-level rodent presence predictor ([`house_model_house_predictor.stan`](/stan_models/house_model_house_predictor.stan))
  
  - A visit-level zoonotic spillover risk model of Lassa-positive _Mastomys natalensis_ count with a site-level rodent presence predictor ([`spillover_risk_visit_level.stan`](/stan_models/spillover_risk_visit_level.stan))