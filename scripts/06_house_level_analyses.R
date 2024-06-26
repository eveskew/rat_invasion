library(tidyverse)
library(rethinking)
library(assertthat)

source("R/functions.R")

#==============================================================================


# House-level analyses

# Import house-level trapping data frame from Sierra Leone
house.level.captures <- read_csv("data/clean/house/house_level_data.csv")

# What is average house-level catch per trap?
house.level.captures %>%
  group_by(Rra_at_site) %>%
  summarize(
    Mna_per_trap = mean(Mna_per_trap)
  )

# What is average site-level occupancy?
house.level.captures %>%
  mutate(occupied = n_Mna > 0) %>%
  group_by(Rra_at_site) %>%
  summarize(
    n = n(),
    occupancy = sum(occupied)/n()
  ) %>%
  pull(occupancy) %>%
  mean()

house.level.captures %>%
  mutate(occupied = n_Mna > 0) %>%
  group_by(site, Rra_at_site) %>%
  summarize(
    n = n(),
    occupancy = sum(occupied)/n()
  ) %>%
  group_by(Rra_at_site) %>%
  summarize(
    occupancy = mean(occupancy)
  )

# Plot
set.seed(24)

house.yes.no <- house.level.captures %>%
  mutate(Rra_at_site_character = ifelse(Rra_at_site == 1, "Present", "Absent")) %>%
  ggplot(aes(x = Rra_at_site_character, y = Mna_per_trap)) +
  geom_violin(fill = alpha("lightgrey", 0.3), linewidth = 0.2) +
  geom_jitter(aes(color = Rra_at_site_character), height = 0, width = 0.25, size = 5) +
  xlab(expression(paste(italic("Rattus rattus"), " status at site"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap"))) +
  scale_color_manual(values = c(alpha("black", 0.15), alpha("darkred", 0.15))) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "none"
  )

house.yes.no

# Save source data for Figure 2a
house.level.captures %>%
  select(
    date, site, visit, track_id, habitat_code, house_id,
    tot_traps, n_Mna, Mna_per_trap, Rra_at_site
  ) %>%
  write_excel_csv("data/source_data/Figure2a.csv")

# Generate a plot that breaks out the data by season
house.level.captures.summary <- house.level.captures %>%
  group_by(Rra_at_site, wet_season) %>%
  summarize(
    tot_traps = sum(tot_traps),
    n_Mna = sum(n_Mna),
    Mna_per_trap = n_Mna/tot_traps
  ) %>%
  ungroup() %>%
  mutate(
    x = case_when(
      Rra_at_site == 0 & wet_season == 0 ~ -0.1,
      Rra_at_site == 0 & wet_season == 1 ~ 0.1,
      Rra_at_site == 1 & wet_season == 0 ~ 0.9,
      Rra_at_site == 1 & wet_season == 1 ~ 1.1
    )
  )

house.level.captures %>%
  mutate(
    Rra_at_site = as.factor(Rra_at_site),
    wet_season = as.factor(wet_season),
    x = case_when(
      Rra_at_site == 0 & wet_season == 0 ~ -0.1,
      Rra_at_site == 0 & wet_season == 1 ~ 0.1,
      Rra_at_site == 1 & wet_season == 0 ~ 0.9,
      Rra_at_site == 1 & wet_season == 1 ~ 1.1
    )
  ) %>%
  ggplot(aes(x = x, y = Mna_per_trap, color = wet_season, group = wet_season)) +
  geom_jitter(size = 5, height = 0, width = 0.05) +
  geom_segment(
    x = -0.15, xend = -0.05,
    y = house.level.captures.summary %>%
      filter(Rra_at_site == 0, wet_season == 0) %>%
      pull(Mna_per_trap),
    yend = house.level.captures.summary %>%
      filter(Rra_at_site == 0, wet_season == 0) %>%
      pull(Mna_per_trap),
    color = "wheat3",
    linewidth = 2
  ) +
  geom_segment(
    x = 0.05, xend = 0.15,
    y = house.level.captures.summary %>%
      filter(Rra_at_site == 0, wet_season == 1) %>%
      pull(Mna_per_trap),
    yend = house.level.captures.summary %>%
      filter(Rra_at_site == 0, wet_season == 1) %>%
      pull(Mna_per_trap),
    color = "steelblue",
    linewidth = 2
  ) +
  geom_segment(
    x = 0.85, xend = 0.95,
    y = house.level.captures.summary %>%
      filter(Rra_at_site == 1, wet_season == 0) %>%
      pull(Mna_per_trap),
    yend = house.level.captures.summary %>%
      filter(Rra_at_site == 1, wet_season == 0) %>%
      pull(Mna_per_trap),
    color = "wheat3",
    linewidth = 2
  ) +
  geom_segment(
    x = 1.05, xend = 1.15,
    y = house.level.captures.summary %>%
      filter(Rra_at_site == 1, wet_season == 1) %>%
      pull(Mna_per_trap),
    yend = house.level.captures.summary %>%
      filter(Rra_at_site == 1, wet_season == 1) %>%
      pull(Mna_per_trap),
    color = "steelblue",
    linewidth = 2
  ) +
  xlab(expression(paste(italic("Rattus rattus"), " status at site"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap"))) +
  theme_minimal() +
  scale_color_manual(
    values = c(
    alpha("wheat3", 0.05), 
    alpha("steelblue", 0.05)
    )
  ) +
  xlim(-0.3, 1.3) +
  theme(
    text = element_text(size = 21),
    legend.position = "none"
  )

#==============================================================================


# Statistical analyses at house level

# Package data for Stan models with Rra effect
stan.dat.house.Rra <- list(
  N = nrow(house.level.captures),
  n_Mna = house.level.captures$n_Mna,
  n_Rra = house.level.captures$n_Rra,
  n_Rra_s = standardize(house.level.captures$n_Rra),
  rodent_at_site = house.level.captures$Rra_at_site,
  rodent_at_house = house.level.captures$Rra_at_house,
  wet_season = house.level.captures$wet_season,
  tot_traps = house.level.captures$tot_traps,
  log_tot_traps = log(house.level.captures$tot_traps),
  N_site = n_distinct(house.level.captures$site),
  site = as.numeric(as.factor(house.level.captures$site)),
  N_visit = paste0(house.level.captures$site, "-", house.level.captures$visit) %>%
    n_distinct(),
  visit = paste0(house.level.captures$site, "-", house.level.captures$visit) %>%
    as.factor() %>%
    as.numeric(),
  N_house = n_distinct(house.level.captures$house_id),
  house = as.numeric(as.factor(house.level.captures$house_id))
)
assert_that(nrow(house.level.captures) == stan.dat.house.Rra$N_house)

# Load model
house.mod.site.predictor <- cmdstan_model("stan_models/house_model_site_predictor.stan")

# Fit Rra model
fit.m1 <- house.mod.site.predictor$sample(
  data = stan.dat.house.Rra, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m1$save_object("saved_models/house_mod_house_traps_Rra_at_site.RDS")
fit.m1 <- readRDS("saved_models/house_mod_house_traps_Rra_at_site.RDS")

fit.m1$diagnostic_summary()
fit.m1$print(max_rows = 100)

draws.m1 <- fit.m1$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site, sigma_visit, sigma_house)

jpeg("outputs/misc/model_out_house_level_Rra_at_site.jpeg",
     width = 1000, height = 500, units = "px")

parameter_summary(draws.m1, prob = 0.99)
plot(precis(draws.m1, prob = 0.99), col = "gold2")
plot(precis(draws.m1, prob = 0.9), add = TRUE)

dev.off()


# What portion of the posterior probability mass for bR is < 0?
sum(draws.m1$bR < 0)/length(draws.m1$bR)


# Generate parameter trace plots

# Set palette for plotting
palette <- wesanderson::wes_palette("Darjeeling1") %>%
  adjustcolor(alpha.f = 0.3)

# Base figure
p <- bayesplot::mcmc_trace(
  fit.m1$draws(format = "matrix"), 
  pars = c("a", "bR", "bW", "sigma_site", "sigma_visit", "sigma_house"),
  size = 0.8,
  facet_args = list(ncol = 2)
) 

# Relabel strip text
levels(p$data$parameter) <- c(
  "grand mean", "*Rattus rattus* effect (present vs. absent)",
  "season effect (rainy vs. dry)", "σ (for site-level varying intercepts)",
  "σ (for visit-level varying intercepts)", "σ (for house-level varying intercepts)"
)

# Plot
p + 
  scale_color_manual(values = palette) + 
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    strip.text.x = ggtext::element_markdown(face = "bold")
  )

ggsave(
  "outputs/supplementary/house_level_house_traps_Rra_at_site_trace_plots.jpeg",
  width = 3500, height = 4000, units = "px"
)


# Generate figure of the Rra_at_site posterior (have to do some of this 
# manually)
cutoff <- 0
hist <- density(draws.m1$bR, from = -10, to = 10)
hist <- data.frame(x = hist$x, y = hist$y) %>%
  mutate(area = x >= cutoff)

hist %>%
  ggplot(aes(x = x, ymin = 0, ymax = y, fill = area)) +
  geom_ribbon() +
  geom_line(aes(y = y), linewidth = 1) +
  geom_vline(xintercept = 0, lty = 2, linewidth = 2) +
  xlab(expression(paste("Coefficient for ", italic("Rattus rattus"), " presence"))) +
  ylab("Density") +
  xlim(-4, 2) +
  ylim(0, 1) +
  scale_fill_manual(values = c(alpha("darkred", 0.3), "white")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "none"
  )

ggsave("outputs/misc/house_level_house_traps_Rra_at_site_coefficient.jpeg",
       width = 2000, height = 1600, units = "px")


# Fit the same model but with the house-level Rattus rattus predictor 

# Load model
house.mod.house.predictor <- cmdstan_model("stan_models/house_model_house_predictor.stan")

# Fit Rra model
fit.m2 <- house.mod.house.predictor$sample(
  data = stan.dat.house.Rra, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m2$save_object("saved_models/house_mod_house_traps_Rra_at_house.RDS")
fit.m2 <- readRDS("saved_models/house_mod_house_traps_Rra_at_house.RDS")

fit.m2$diagnostic_summary()
fit.m2$print(max_rows = 100)

draws.m2 <- fit.m2$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site, sigma_visit, sigma_house)

jpeg("outputs/misc/model_out_house_level_Rra_at_house.jpeg",
     width = 1000, height = 500, units = "px")

parameter_summary(draws.m2, prob = 0.99)
plot(precis(draws.m2, prob = 0.99), col = "gold2")
plot(precis(draws.m2, prob = 0.9), add = TRUE)

dev.off()


# What portion of the posterior probability mass for bR is < 0?
sum(draws.m2$bR < 0)/length(draws.m2$bR)


# Generate parameter trace plots

# Set palette for plotting
palette <- wesanderson::wes_palette("Darjeeling1") %>%
  adjustcolor(alpha.f = 0.3)

# Base figure
p <- bayesplot::mcmc_trace(
  fit.m2$draws(format = "matrix"), 
  pars = c("a", "bR", "bW", "sigma_site", "sigma_visit", "sigma_house"),
  size = 0.8,
  facet_args = list(ncol = 2)
) 

# Relabel strip text
levels(p$data$parameter) <- c(
  "grand mean", "*Rattus rattus* effect (present vs. absent)",
  "season effect (rainy vs. dry)", "σ (for site-level varying intercepts)",
  "σ (for visit-level varying intercepts)", "σ (for house-level varying intercepts)"
)

# Plot
p + 
  scale_color_manual(values = palette) + 
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    strip.text.x = ggtext::element_markdown(face = "bold")
  )

ggsave(
  "outputs/supplementary/house_level_house_traps_Rra_at_house_trace_plots.jpeg",
  width = 3500, height = 4000, units = "px"
)


# Generate figure of the Rra_at_house posterior (have to do some of this 
# manually)
cutoff <- 0
hist <- density(draws.m2$bR, from = -10, to = 10)
hist <- data.frame(x = hist$x, y = hist$y) %>%
  mutate(area = x >= cutoff)

hist %>%
  ggplot(aes(x = x, ymin = 0, ymax = y, fill = area)) +
  geom_ribbon() +
  geom_line(aes(y = y), linewidth = 1) +
  geom_vline(xintercept = 0, lty = 2, linewidth = 2) +
  xlab(expression(paste("Coefficient for ", italic("Rattus rattus"), " presence"))) +
  ylab("Density") +
  xlim(-4, 2) +
  ylim(0, 1) +
  scale_fill_manual(values = c(alpha("darkred", 0.3), "white")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "none"
  )

ggsave("outputs/misc/house_level_house_traps_Rra_at_house_coefficient.jpeg",
       width = 2000, height = 1600, units = "px")

#==============================================================================


# Compute house-level predictions from model output

preds <- house.level.captures %>% 
  select(site, house_id, tot_traps, n_Mna) %>%
  uncount(nrow(draws.m1)) %>%
  mutate(
    lambda = fit.m1$draws(format = "matrix") %>%
      data.frame() %>%
      select(contains("lambda")) %>%
      unlist(),
    lambda_inv_link = exp(lambda)
  )

set.seed(1)
preds$pred <- sapply(1:nrow(preds), function(x)
  sum(rpois(preds$tot_traps[x], preds$lambda_inv_link[x]))
)

preds.summary <- preds %>%
  group_by(site, house_id, n_Mna) %>%
  summarize(
    mean_lambda = mean(lambda_inv_link),
    lower90 = HPDI(pred, 0.9)[1],
    upper90 = HPDI(pred, 0.9)[2],
    lower99 = HPDI(pred, 0.99)[1],
    upper99 = HPDI(pred, 0.99)[2],
  ) %>%
  ungroup() %>%
  mutate(
    in_interval = ifelse(n_Mna <= upper99 & n_Mna >= lower99, TRUE, FALSE)
  )

sum(preds.summary$in_interval)

ggplot() +
  geom_linerange(
    aes(x = 1:nrow(house.level.captures), ymin = lower99, ymax = upper99, color = site),
    data = preds.summary, linewidth = 1
  ) +
  geom_linerange(
    aes(x = 1:nrow(house.level.captures), ymin = lower90, ymax = upper90, color = site),
    data = preds.summary, linewidth = 2
  ) +
  geom_point(
    aes(x = 1:nrow(house.level.captures), y = n_Mna, col = as.factor(Rra_at_site)), 
    data = house.level.captures, size = 2
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    text = element_text(size = 20),
    legend.position = "none"
  )

#==============================================================================


# Statistical analyses of other rodent species at house level, with site-level
# predictors

# Package Mer data
stan.dat.house.Mer <- stan.dat.house.Rra
stan.dat.house.Mer$rodent_at_site <- house.level.captures$Mer_at_site
stan.dat.house.Mer$rodent_at_house <- house.level.captures$Mer_at_house

# Fit Mer model
fit.m3 <- house.mod.site.predictor$sample(
  data = stan.dat.house.Mer, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m3$save_object("saved_models/house_mod_house_traps_Mer_at_site.RDS")
fit.m3 <- readRDS("saved_models/house_mod_house_traps_Mer_at_site.RDS")

fit.m3$diagnostic_summary()
fit.m3$print(max_rows = 100)

draws.m3 <- fit.m3$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site, sigma_visit, sigma_house)

parameter_summary(draws.m3, prob = 0.8)

# Package Pda data
stan.dat.house.Pda <- stan.dat.house.Rra
stan.dat.house.Pda$rodent_at_site <- house.level.captures$Pda_at_site
stan.dat.house.Pda$rodent_at_house <- house.level.captures$Pda_at_house

# Fit Pda model
fit.m4 <- house.mod.site.predictor$sample(
  data = stan.dat.house.Pda, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m4$save_object("saved_models/house_mod_house_traps_Pda_at_site.RDS")
fit.m4 <- readRDS("saved_models/house_mod_house_traps_Pda_at_site.RDS")

fit.m4$diagnostic_summary()
fit.m4$print(max_rows = 100)

draws.m4 <- fit.m4$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site, sigma_visit, sigma_house)

parameter_summary(draws.m4, prob = 0.8)

# Package Pro data
stan.dat.house.Pro <- stan.dat.house.Rra
stan.dat.house.Pro$rodent_at_site <- house.level.captures$Pro_at_site
stan.dat.house.Pro$rodent_at_house <- house.level.captures$Pro_at_house

# Fit Pro model
fit.m5 <- house.mod.site.predictor$sample(
  data = stan.dat.house.Pro, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m5$save_object("saved_models/house_mod_house_traps_Pro_at_site.RDS")
fit.m5 <- readRDS("saved_models/house_mod_house_traps_Pro_at_site.RDS")

fit.m5$diagnostic_summary()
fit.m5$print(max_rows = 100)

draws.m5 <- fit.m5$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site, sigma_visit, sigma_house)

parameter_summary(draws.m5, prob = 0.8)


# Generate a figure of all rodent presence/absence effect posteriors

# Package data from all species-specific models
dat.ridges <- data.frame(
  species = rep(
    c("Rattus rattus", "Mastomys erythroleucus",
      "Praomys daltoni", "Praomys rostratus"),
    each = length(draws.m1$bR)
  ),
  value = c(
    draws.m1$bR,
    draws.m3$bR,
    draws.m4$bR,
    draws.m5$bR
  )
) %>%
  arrange(species)

# Plot
palette <- wesanderson::wes_palette("Darjeeling2")
palette <- palette[c(1, 3:5)]

dat.ridges %>%
  mutate(species = forcats::fct_rev(species)) %>%
  ggplot(aes(x = value, y = species, fill = rev(species))) +
  geom_vline(xintercept = 0, linewidth = 2, lty = 2) +
  ggridges::stat_density_ridges(
    quantile_lines = TRUE, 
    quantiles = c(0.10, 0.90), 
    alpha = 0.6
  ) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  xlab("Parameter Value") +
  ylab("") +
  xlim(-5, 5) +
  theme(
    text = element_text(size = 21),
    axis.text.y = element_text(face = "italic"),
    legend.position = "none"
  )

ggsave(
  "outputs/supplementary/species_presence_effect_posteriors_house_model_site_predictors.jpeg", 
  width = 3000, 
  height = 2000, 
  unit = "px"
)

#==============================================================================


# Statistical analyses of other rodent species at house level, with house-level
# predictors

# Fit Mer model
fit.m6 <- house.mod.house.predictor$sample(
  data = stan.dat.house.Mer, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m6$save_object("saved_models/house_mod_house_traps_Mer_at_house.RDS")
fit.m6 <- readRDS("saved_models/house_mod_house_traps_Mer_at_house.RDS")

fit.m6$diagnostic_summary()
fit.m6$print(max_rows = 100)

draws.m6 <- fit.m6$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site, sigma_visit, sigma_house)

parameter_summary(draws.m6, prob = 0.8)

# Fit Pda model
fit.m7 <- house.mod.house.predictor$sample(
  data = stan.dat.house.Pda, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m7$save_object("saved_models/house_mod_house_traps_Pda_at_house.RDS")
fit.m7 <- readRDS("saved_models/house_mod_house_traps_Pda_at_house.RDS")

fit.m7$diagnostic_summary()
fit.m7$print(max_rows = 100)

draws.m7 <- fit.m7$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site, sigma_visit, sigma_house)

parameter_summary(draws.m7, prob = 0.8)

# Fit Pro model
fit.m8 <- house.mod.house.predictor$sample(
  data = stan.dat.house.Pro, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m8$save_object("saved_models/house_mod_house_traps_Pro_at_house.RDS")
fit.m8 <- readRDS("saved_models/house_mod_house_traps_Pro_at_house.RDS")

fit.m8$diagnostic_summary()
fit.m8$print(max_rows = 100)

draws.m8 <- fit.m8$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site, sigma_visit, sigma_house)

parameter_summary(draws.m8, prob = 0.8)


# Generate a figure of all rodent presence/absence effect posteriors

# Package data from all species-specific models
dat.ridges <- data.frame(
  species = rep(
    c("Rattus rattus", "Mastomys erythroleucus",
      "Praomys daltoni", "Praomys rostratus"),
    each = length(draws.m2$bR)
  ),
  value = c(
    draws.m2$bR,
    draws.m6$bR,
    draws.m7$bR,
    draws.m8$bR
  )
) %>%
  arrange(species)

# Plot
palette <- wesanderson::wes_palette("Darjeeling2")
palette <- palette[c(1, 3:5)]

dat.ridges %>%
  mutate(species = forcats::fct_rev(species)) %>%
  ggplot(aes(x = value, y = species, fill = rev(species))) +
  geom_vline(xintercept = 0, linewidth = 2, lty = 2) +
  ggridges::stat_density_ridges(
    quantile_lines = TRUE, 
    quantiles = c(0.10, 0.90), 
    alpha = 0.6
  ) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  xlab("Parameter Value") +
  ylab("") +
  xlim(-5, 5) +
  theme(
    text = element_text(size = 21),
    axis.text.y = element_text(face = "italic"),
    legend.position = "none"
  )

ggsave(
  "outputs/supplementary/species_presence_effect_posteriors_house_model_house_predictors.jpeg", 
  width = 3000, 
  height = 2000, 
  unit = "px"
)

#==============================================================================


# Occupancy analyses - data importation

# Import Mastomys natalensis detection data
o <- read_csv("data/clean/occupancy/Mna_detection.csv")

# Import occupancy and detection covariate data
occ.covs <- read_csv("data/clean/occupancy/occupancy_covariates.csv")
det.cov.trap.count <- read_csv("data/clean/occupancy/detection_trap_count_covariate.csv")
det.cov.cumulative.Mna.count <- read_csv("data/clean/occupancy/detection_cumulative_Mna_count_covariate.csv")

# Generate detection covariate list
det.covs <- list(
  trap_count = det.cov.trap.count,
  cumulative_Mna_count = det.cov.cumulative.Mna.count
)

# Generate data list for model fitting
data.list <- list(
  y = o[, 7:10],
  occ.covs = occ.covs,
  det.covs = det.covs
)

#==============================================================================


# Occupancy analyses - model fitting and interpretation

# What is average site-level occupancy?
o %>%
  rowwise() %>%
  mutate(
    sum = sum(`1`, `2`, `3`, `4`, na.rm = T),
    occupied = sum > 0
  ) %>%
  group_by(site) %>%
  summarize(
    n = n(),
    occupancy = sum(occupied)/n()
  ) %>%
  pull(occupancy) %>%
  mean()

# What is occupancy conditional on R. rattus presence and season
# (complete pooling by site)
o %>%
  rowwise() %>%
  mutate(
    sum = sum(`1`, `2`, `3`, `4`, na.rm = T),
    occupied = sum > 0
  ) %>%
  group_by(site, Rra_at_site, wet_season) %>%
  summarize(
    n = n(),
    occupied = sum(occupied),
    occupancy = occupied/n()
  ) %>%
  group_by(Rra_at_site, wet_season) %>%
  summarize(
    occupancy = mean(occupancy)
  )

# What is occupancy conditional on R. rattus presence and season
# (no pooling by site)
o %>%
  rowwise() %>%
  mutate(
    sum = sum(`1`, `2`, `3`, `4`, na.rm = T),
    occupied = sum > 0
  ) %>%
  group_by(Rra_at_site, wet_season) %>%
  summarize(
    n = n(),
    n_occupied = sum(occupied),
    occupancy = n_occupied/n()
  )

# Set priors for the occupancy model fit
priors.list <- list(
  beta.normal = list(mean = 0, var = 1),
  alpha.normal = list(mean = 0, var = 1),
  sigma.sq.psi.ig = list(shape = 3, scale = 1)
)

# Fit an occupancy model
out <- spOccupancy::PGOcc(
  occ.formula = ~ Rra_at_site + wet_season + (1|site_numeric) + (1|visit_numeric), 
  det.formula = ~ trap_count + cumulative_Mna_count, 
  data = data.list, 
  priors = priors.list,
  n.burn = 15000,
  n.samples = 40000,
  n.chains = 4,
  verbose = TRUE
)

# Save and load fit model
saveRDS(out, file = "saved_models/occupancy_model.RDS")
out <- readRDS("saved_models/occupancy_model.RDS")

summary(out)

# Effect of Rra_at_site
mean(out$beta.samples[,2])
HPDI(out$beta.samples[,2], prob = 0.99)
dens(out$beta.samples[,2], show.HPDI = 0.9)

# What portion of the posterior probability mass for bR is < 0?
sum(out$beta.samples[,2] < 0)/length(out$beta.samples[,2])

# Wet season sampling parameter
mean(out$beta.samples[,3])
HPDI(out$beta.samples[,3], prob = 0.99)
dens(out$beta.samples[,3], show.HPDI = 0.9)

# Baseline detection parameter
mean(out$alpha.samples[,1])
HPDI(out$alpha.samples[,1], prob = 0.99)
dens(out$alpha.samples[,1], show.HPDI = 0.9)

# What's the expected detection probability for a single trap-night
# Need to take baseline detection parameter, add the trap count parameter, and
# transform to the outcome scale
mean(logistic(out$alpha.samples[,1] + out$alpha.samples[,2]))
HPDI(logistic(out$alpha.samples[,1] + out$alpha.samples[,2]), prob = 0.99)
dens(logistic(out$alpha.samples[,1] + out$alpha.samples[,2]), show.HPDI = 0.9)

# 1 - failure_rate^x = probability
# -failure_rate^x = probability - 1
# failure_rate^x = 1 - probability
# x = log_base_failure_rate(1 - probability)
success <- mean(logistic(out$alpha.samples[,1] + out$alpha.samples[,2]))
failure <- 1 - success

probability <- 0.9
logb(1 - probability, failure)

probability <- 0.99
logb(1 - probability, failure)

# Another way to do the calculation is to use the trap effort estimate from the
# model directly

# 3 traps
mean(logistic(out$alpha.samples[,1] + (out$alpha.samples[,2]*3)))
HPDI(logistic(out$alpha.samples[,1] + (out$alpha.samples[,2]*3)), prob = 0.99)
dens(logistic(out$alpha.samples[,1] + (out$alpha.samples[,2]*3)), show.HPDI = 0.9)

# 7 traps
mean(logistic(out$alpha.samples[,1] + (out$alpha.samples[,2]*7)))
HPDI(logistic(out$alpha.samples[,1] + (out$alpha.samples[,2]*7)), prob = 0.99)
dens(logistic(out$alpha.samples[,1] + (out$alpha.samples[,2]*7)), show.HPDI = 0.9)

# 13 traps
mean(logistic(out$alpha.samples[,1] + (out$alpha.samples[,2]*13)))
HPDI(logistic(out$alpha.samples[,1] + (out$alpha.samples[,2]*13)), prob = 0.99)
dens(logistic(out$alpha.samples[,1] + (out$alpha.samples[,2]*13)), show.HPDI = 0.9)

# Effect of trap_count
mean(out$alpha.samples[,2])
HPDI(out$alpha.samples[,2], prob = 0.99)
dens(out$alpha.samples[,2], show.HPDI = 0.9)

# Effect of cumulative_Mna_count
mean(out$alpha.samples[,3])
HPDI(out$alpha.samples[,3], prob = 0.99)
dens(out$alpha.samples[,3], show.HPDI = 0.9)


# Generate figure of the Rra_at_site posterior (have to do some of this 
# manually)
cutoff <- 0
hist <- density(out$beta.samples[,2], from = -10, to = 10)
hist <- data.frame(x = hist$x, y = hist$y) %>%
  mutate(area = x >= cutoff)

hist %>%
  ggplot(aes(x = x, ymin = 0, ymax = y, fill = area)) +
  geom_ribbon() +
  geom_line(aes(y = y), linewidth = 1) +
  geom_vline(xintercept = 0, lty = 2, linewidth = 2) +
  xlab(expression(paste("Occupancy coefficient for ", italic("Rattus rattus"), " presence"))) +
  ylab("Density") +
  xlim(-6, 2) +
  ylim(0, 0.7) +
  scale_fill_manual(values = c(alpha("darkred", 0.3), "white")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "none"
  )

ggsave("outputs/misc/occupancy_model_Rra_at_site_coefficient.jpeg",
       width = 2000, height = 1600, units = "px")


# Generate "site.preds" using the "z.samples" out from the model, matching them
# up with their appropriate sites
site.preds <- data.frame(
  site = character(),
  occ_prob = double()
)

for(site in unique(o$site)) {
  
  occ_prob <- apply(out$z.samples[ , which(o$site == site)], 1, sum)/length(which(o$site == site))
  temp <- data.frame(
    site = rep(as.character(site), length(occ_prob)),
    occ_prob
  )
  
  site.preds <- bind_rows(site.preds, temp)
}

site.preds <- site.preds %>%
  left_join(
    .,
    o %>%
      select(site, Rra_at_site) %>%
      distinct(site, Rra_at_site),
    by = "site"
  )

site.preds %>%
  group_by(site) %>%
  summarize(
    mean_occ_prob = mean(occ_prob),
    median_occ_prob = median(occ_prob),
    lower = HPDI(occ_prob, 0.99)[1],
    upper = HPDI(occ_prob, 0.99)[2]
  )

naive.occ <- o %>%
  mutate(
    house_w_Mna = ifelse(rowSums(.[,7:10], na.rm = TRUE) > 0, 1, 0)
  ) %>%
  group_by(site, Rra_at_site) %>%
  summarize(
    n_houses_sampled = n(),
    n_houses_w_Mna = sum(house_w_Mna),
    naive_occ = n_houses_w_Mna/n_houses_sampled
  )

site.preds %>%
  group_by(site, Rra_at_site) %>%
  summarize(
    mean_occ_prob = mean(occ_prob),
    median_occ_prob = median(occ_prob),
    lower = HPDI(occ_prob, 0.99)[1],
    upper = HPDI(occ_prob, 0.99)[2]
  ) %>%
  ungroup() %>%
  ggplot(aes(x = site, y = median_occ_prob, color = as.factor(Rra_at_site))) +
  geom_point(size = 6) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept = mean(logistic(out$beta.samples[,1])), lty = 2) +
  geom_hline(yintercept = mean(logistic(out$beta.samples[,1] + out$beta.samples[,2])), lty = 2, color = "darkred") +
  geom_point(
    data = naive.occ, 
    aes(x = site, y = naive_occ, color = as.factor(Rra_at_site)), 
    pch = 1, size = 6) +
  scale_color_manual(values = c("black", "darkred")) +
  xlab("") +
  ylab("Proportion of houses occupied by *Mastomys natalensis*") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = ggtext::element_markdown(),
    legend.position = "none"
  )

ggsave("outputs/misc/observed_predicted_Mna_site_level_occupancy.jpeg",
       width = 3500, height = 1500, units = "px")


preds <- data.frame(
  season = rep(
    c("rainy season", "dry season"), 
    each = length(out$beta.samples[,1])*2
  ),
  site_status = rep(
    c("Absent", "Present",
      "Absent", "Present"),
    each = length(out$beta.samples[,1])
  ),
  beta = c(
    out$beta.samples[,1] + out$beta.samples[,3], 
    out$beta.samples[,1] + out$beta.samples[,3] + out$beta.samples[,2],
    out$beta.samples[,1], 
    out$beta.samples[,1] + out$beta.samples[,2]
  )
) %>%
  mutate(
    occ_prob = logistic(beta)
  )

preds %>%
  group_by(site_status, season) %>%
  summarize(
    mean_occ_prob = mean(occ_prob),
    median_occ_prob = median(occ_prob),
    lower_90 = HPDI(occ_prob, 0.9)[1],
    upper_90 = HPDI(occ_prob, 0.9)[2],
    lower_99 = HPDI(occ_prob, 0.99)[1],
    upper_99 = HPDI(occ_prob, 0.99)[2]
  )

# Save source data for Figure 2b
preds %>%
  write_excel_csv("data/source_data/Figure2b.csv")

#==============================================================================


occ.plot <- preds %>%
  ggplot(aes(x = occ_prob, color = site_status)) +
  geom_density(aes(fill = site_status), linewidth = 1) +
  xlab(expression(paste("House-level occupancy of ", italic("Mastomys natalensis")))) +
  ylab("Density") +
  scale_color_manual(
    values = c("black", "darkred"),
    labels = c(
      expression(paste(italic("Rattus rattus"), " absent")),
      expression(paste(italic("Rattus rattus"), " present"))
    )
  ) +
  scale_fill_manual(
    values = c(alpha("black", 0.3), alpha("darkred", 0.3)),
    labels = c(
      expression(paste(italic("Rattus rattus"), " absent")),
      expression(paste(italic("Rattus rattus"), " present"))
    )
  ) +
  theme_minimal() +
  xlim(0, 1) +
  geom_vline(
    xintercept = preds %>%
      filter(site_status == "Absent") %>%
      pull(occ_prob) %>%
      median(),
    color = "black",
    lty = 2, linewidth = 2
  ) +
  geom_vline(
    xintercept = preds %>%
      filter(site_status == "Present") %>%
      pull(occ_prob) %>%
      median(),
    color = "darkred",
    lty = 2, linewidth = 2
  ) +
  theme(
    text = element_text(size = 20),
    legend.position = "bottom",
    legend.title = element_blank()
  )

val.dodge <- 0.25

occ.plot <- preds %>%
  group_by(season, site_status) %>%
  summarize(
    mean = mean(occ_prob),
    lower99 = HPDI(occ_prob, 0.99)[1],
    upper99 = HPDI(occ_prob, 0.99)[2],
    lower90 = HPDI(occ_prob, 0.9)[1],
    upper90 = HPDI(occ_prob, 0.9)[2]
  ) %>%
  ungroup() %>%
  ggplot(aes(x = site_status, y = mean, color = season, group = season)) +
  geom_linerange(
    aes(ymin = lower99, ymax = upper99), 
    position = position_dodge2(width = val.dodge),
    linewidth = 1,
    key_glyph = "rect"
  ) +
  geom_linerange(
    aes(ymin = lower90, ymax = upper90), 
    position = position_dodge2(width = val.dodge),
    linewidth = 3
  ) +
  geom_point(position = position_dodge2(width = val.dodge), size = 5) +
  xlab(expression(paste(italic("Rattus rattus"), " status at site"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "house-level occupancy"))) +
  scale_y_continuous(limits = c(0, 0.7)) +
  theme_minimal() +
  scale_color_manual(values = c("wheat3", "steelblue")) +
  theme(
    text = element_text(size = 21),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.82),
    legend.background = element_rect(fill = "white", colour = 0)
  )

cowplot::plot_grid(
  house.yes.no, occ.plot, 
  nrow = 1,
  labels = "auto",
  label_size = 22
)

ggsave("outputs/house_level_analyses.jpeg", 
       width = 3000, height = 1500, unit = "px")

# Make pdf version of the same plot

set.seed(24)

house.yes.no <- house.level.captures %>%
  mutate(Rra_at_site_character = ifelse(Rra_at_site == 1, "Present", "Absent")) %>%
  ggplot(aes(x = Rra_at_site_character, y = Mna_per_trap)) +
  geom_violin(fill = alpha("lightgrey", 0.3), linewidth = 0.2/2) +
  geom_jitter(aes(fill = Rra_at_site_character), stroke = NA, height = 0, width = 0.25, shape = 21, size = 5/2) +
  xlab(expression(paste(italic("Rattus rattus"), " status at site"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap"))) +
  scale_fill_manual(values = c(alpha("black", 0.2), alpha("darkred", 0.2))) +
  theme_minimal() +
  theme(
    text = element_text(size = 20/2),
    legend.position = "none"
  )

occ.plot <- preds %>%
  group_by(season, site_status) %>%
  summarize(
    mean = mean(occ_prob),
    lower99 = HPDI(occ_prob, 0.99)[1],
    upper99 = HPDI(occ_prob, 0.99)[2],
    lower90 = HPDI(occ_prob, 0.9)[1],
    upper90 = HPDI(occ_prob, 0.9)[2]
  ) %>%
  ungroup() %>%
  ggplot(aes(x = site_status, y = mean, color = season, group = season)) +
  geom_linerange(
    aes(ymin = lower99, ymax = upper99), 
    position = position_dodge2(width = val.dodge),
    linewidth = 1/2,
    key_glyph = "rect"
  ) +
  geom_linerange(
    aes(ymin = lower90, ymax = upper90), 
    position = position_dodge2(width = val.dodge),
    linewidth = 3/2
  ) +
  geom_point(position = position_dodge2(width = val.dodge), size = 5/2) +
  xlab(expression(paste(italic("Rattus rattus"), " status at site"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "house-level occupancy"))) +
  scale_y_continuous(limits = c(0, 0.7)) +
  theme_minimal() +
  scale_color_manual(values = c("wheat3", "steelblue")) +
  theme(
    text = element_text(size = 20/2),
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.key.height = unit(4, "mm"),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.83),
    legend.background = element_rect(fill = "white", colour = 0)
  )

cowplot::plot_grid(
  house.yes.no, occ.plot, 
  nrow = 1,
  labels = "auto",
  label_size = 22/2
)

ggsave("outputs/Figure2.pdf", 
       width = 180, height = 90, unit = "mm")
