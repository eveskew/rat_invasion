# Impacts of black rat invasion on the primary rodent host of Lassa virus, _Mastomys natalensis_

This repository contains code, data, and figures that support:

Eskew, E.A., B.H. Bird, B.M. Ghersi, J. Bangura, A.J. Basinski, E. Amara, M.A. Bah, M.C. Kanu, O.T. Kanu, E.G. Lavalie, V. Lungay, W. Robert, M.A. Vandi, E. Fichet-Calvet, and S.L. Nuismer. 2023. Reservoir displacement by an invasive rodent reduces Lassa virus spillover risk.

--- 

### Repository Structure

- [`/R`](/R) contains code for functions that are sourced in the analysis scripts

- [`/data`](/data) contains data files necessary for the analysis

- [`/outputs`](/outputs) contains all main text figures output from the analysis scripts

- [`/scripts`](/scripts) contains the analysis scripts, ordered sequentially by number

- [`/stan_models`](/stan_models) contains the Stan code sourced in the analysis scripts
  - A visit-level model of _Mastomys natalensis_ count with a site-level rodent presence predictor: [`visit_model.stan`](/stan_models/visit_model.stan)
  - A house-level model of _Mastomys natalensis_ count with a site-level rodent presence predictor: [`house_model_site_predictor.stan`](/stan_models/house_model_site_predictor.stan)
  - An alternative house-level model of _Mastomys natalensis_ count with a house-level rodent presence predictor: [`house_model_house_predictor.stan`](/stan_models/house_model_house_predictor.stan)
  - A visit-level spillover risk model of Lassa-positive _Mastomys natalensis_ count with a site-level rodent presence predictor: [`spillover_risk_visit_level.stan`](/stan_models/spillover_risk_visit_level.stan)