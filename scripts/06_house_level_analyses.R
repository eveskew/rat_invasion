library(tidyverse)
library(rethinking)
library(lubridate)

#==============================================================================


# Import cleaned trapping and capture data files from Sierra Leone (the 
# PREEMPT project)

# Data on track-level trapping effort across all sites
summary.trap.dat <- read_csv("data/clean/PREEMPT/trap_metadata.csv") %>%
  mutate(
    # visit 5 at Petema is coded with two dates two days apart, recode to have
    # the same visit date
    date_mod = ifelse(
      site == "Petema" & as.character(date) == "2021-01-28",
      "2021-01-26",
      as.character(date)
    ),
    # visit 2 at Badala is coded in the incorrect year, recode
    date_mod = ifelse(
      site == "Badala" & visit == 2,
      "2019-11-09",
      date_mod
    ),
    date_mod = as.Date(date_mod)
  ) %>%
  select(-mastomys, -house_type, -date) %>%
  rename(date = date_mod) %>%
  relocate(date, everything())

# Data on rodent captures across all sites
capture.dat <- read_csv("data/clean/PREEMPT/PREEMPT_capture_data.csv")


# Generate track-level data tables for Sierra Leone

# Summarize capture data by site, visit, and track ID
capture.dat.by.track <- capture.dat %>%
  group_by(site, visit, track_id) %>%
  summarize(
    n_catch = n(),
    n_Mna = sum(sp == "Mna"),
    n_Rra = sum(sp == "Rra"),
    n_Mer = sum(sp == "Mer"),
    n_Pda = sum(sp == "Pda"),
    n_Pro = sum(sp == "Pro")
  ) %>%
  ungroup()

sum(capture.dat.by.track$n_Mna)
sum(capture.dat.by.track$n_Rra)
sum(capture.dat.by.track$n_Mer)
sum(capture.dat.by.track$n_Pda)
sum(capture.dat.by.track$n_Pro)

# Join summarized capture data in with track-level trap data
track.level.captures <- summary.trap.dat %>%
  left_join(
    ., capture.dat.by.track,
    by = c("site", "visit", "track_id")
  ) %>%
  mutate(
    n_Mna = ifelse(is.na(n_Mna), 0, n_Mna),
    n_Rra = ifelse(is.na(n_Rra), 0, n_Rra),
    n_Mer = ifelse(is.na(n_Mer), 0, n_Mer),
    n_Pda = ifelse(is.na(n_Pda), 0, n_Pda),
    n_Pro = ifelse(is.na(n_Pro), 0, n_Pro)
  )

sum(track.level.captures$n_Mna)
sum(track.level.captures$n_Rra)
sum(track.level.captures$n_Mer)
sum(track.level.captures$n_Pda)
sum(track.level.captures$n_Pro)


# Import combined visit-level data
visit.dat <- read_csv(
  "data/clean/combined/visit_level_data.csv"
)

#==============================================================================


# House-level analyses

# Generate house-level data frame
house.level.captures <- track.level.captures %>%
  filter(habitat_code == "H") %>%
  # Remove zero trapping effort houses!
  filter(tot_traps != 0) %>%
  mutate(
    house_id = paste(site, visit, track_id, sep = "-"),
    house_w_only_Mna = n_Mna > 0 & n_Rra == 0,
    house_w_only_Rra = n_Mna == 0 & n_Rra > 0,
    house_w_both = n_Mna > 0 & n_Rra > 0,
    house_w_neither = n_Mna == 0 & n_Rra == 0,
    Mna_per_trap = n_Mna/tot_traps,
    Rra_at_house = ifelse(n_Rra > 0, 1, 0)
  ) %>%
  left_join(
    .,
    visit.dat %>%
      filter(data_source == "PREEMPT") %>%
      select(site, visit, date, Rra_at_site, wet_season) %>%
      mutate(visit = as.numeric(visit)),
    by = c("site", "visit", "date")
  )

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
  geom_violin(fill = alpha("lightgrey", 0.5)) +
  geom_jitter(aes(color = Rra_at_site_character), height = 0, width = 0.25, size = 7) +
  xlab(expression(paste(italic("Rattus rattus"), " status at site"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap"))) +
  scale_color_manual(values = c(alpha("black", 0.2), alpha("darkred", 0.2))) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "none"
  )

house.yes.no

#==============================================================================


# Statistical analyses at house level

# Package data for Stan models
stan.dat <- list(
  N = nrow(house.level.captures),
  n_Mna = house.level.captures$n_Mna,
  n_Rra = house.level.captures$n_Rra,
  n_Rra_s = standardize(house.level.captures$n_Rra),
  Rra_at_site = house.level.captures$Rra_at_site,
  Rra_at_house = house.level.captures$Rra_at_house,
  wet_season = house.level.captures$wet_season,
  tot_traps = house.level.captures$tot_traps,
  log_tot_traps = log(house.level.captures$tot_traps),
  N_site = n_distinct(house.level.captures$site),
  site = as.numeric(as.factor(house.level.captures$site)),
  N_house = n_distinct(house.level.captures$house_id),
  house = as.numeric(as.factor(house.level.captures$house_id))
)

# Load model
house.mod.Rra.at.site <- cmdstan_model("stan_models/house_model_Rra_at_site.stan")

# Fit model
fit.m1 <- house.mod.Rra.at.site$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m1$save_object("saved_models/house_mod_Rra_at_site.RDS")
fit.m1 <- readRDS("saved_models/house_mod_Rra_at_site.RDS")

fit.m1$diagnostic_summary()
fit.m1$print(max_rows = 100)

draws.m1 <- fit.m1$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site, sigma_house)

jpeg("outputs/misc/model_out_house_level_Rra_at_site.jpeg",
     width = 1000, height = 500, units = "px")

precis(draws.m1, prob = 0.99)
plot(precis(draws.m1, prob = 0.99), col = "gold2")
plot(precis(draws.m1, prob = 0.9), add = TRUE)

dev.off()


# What portion of the posterior probability mass for bR is < 0?
sum(draws.m1$bR < 0)/length(draws.m1$bR)


# Generate parameter trace plots

# Set palette for plotting
palette <- adjustcolor(wesanderson::wes_palette("Darjeeling1"), alpha.f = 0.3)

# Base figure
p <- bayesplot::mcmc_trace(
  fit.m1$draws(format = "matrix"), 
  pars = c("a", "bR", "bW", "sigma_site", "sigma_house"),
  size = 0.8,
  facet_args = list(ncol = 2)
) 

# Relabel strip text
levels(p$data$parameter) <- c(
  "grand mean", "*Rattus rattus* effect (present vs. absent)",
  "season effect (wet vs. dry)", "σ (for site-level random effects)",
  "σ (for house-level random effects)"
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
  "outputs/misc/house_level_house_traps_Rra_at_site_trace_plots.jpeg",
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
house.mod.Rra.at.house <- cmdstan_model("stan_models/house_model_Rra_at_house.stan")

# Fit model
fit.m2 <- house.mod.Rra.at.house$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m2$save_object("saved_models/house_mod_Rra_at_house.RDS")
fit.m2 <- readRDS("saved_models/house_mod_Rra_at_house.RDS")

fit.m2$diagnostic_summary()
fit.m2$print(max_rows = 100)

draws.m2 <- fit.m2$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site, sigma_house)

jpeg("outputs/misc/model_out_house_level_Rra_at_house.jpeg",
     width = 1000, height = 500, units = "px")

precis(draws.m2, prob = 0.99)
plot(precis(draws.m2, prob = 0.99), col = "gold2")
plot(precis(draws.m2, prob = 0.9), add = TRUE)

dev.off()


# What portion of the posterior probability mass for bR is < 0?
sum(draws.m2$bR < 0)/length(draws.m2$bR)


# Generate parameter trace plots

# Set palette for plotting
palette <- adjustcolor(wesanderson::wes_palette("Darjeeling1"), alpha.f = 0.3)

# Base figure
p <- bayesplot::mcmc_trace(
  fit.m2$draws(format = "matrix"), 
  pars = c("a", "bR", "bW", "sigma_site", "sigma_house"),
  size = 0.8,
  facet_args = list(ncol = 2)
) 

# Relabel strip text
levels(p$data$parameter) <- c(
  "grand mean", "*Rattus rattus* effect (present vs. absent)",
  "season effect (wet vs. dry)", "σ (for site-level random effects)",
  "σ (for house-level random effects)"
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
  "outputs/misc/house_level_house_traps_Rra_at_house_trace_plots.jpeg",
  width = 3500, height = 4000, units = "px"
)


# Generate figure of the Rra_at_site posterior (have to do some of this 
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


# Occupancy analyses - data preparation

o <- summary.trap.dat %>%
  # get rid of no-house information
  filter(habitat_code == "H") %>%
  # create total trap counts by night and house IDs
  mutate(
    date = as.Date(date),
    n1_traps = n1_traps - n1_ce - n1_miss,
    n2_traps = n2_traps - n2_ce - n2_miss,
    n3_traps = n3_traps - n3_ce - n3_miss,
    n4_traps = n4_traps - n4_ce - n4_miss,
    house_id = paste(site, visit, track_id, sep = "-")
  ) %>%
  # select only a subset of relevant columns
  select(
    date, site, visit, track_id, house_id,
    n1_traps, n2_traps, n3_traps, n4_traps
  ) %>%
  # pivot longer to get each row as a night of trapping at a given house
  pivot_longer(
    cols = starts_with("n"),
    names_to = "night",
    values_to = "trap_count"
  ) %>%
  # eliminate any nights without traps out
  filter(
    trap_count > 0,
    !is.na(trap_count)
  ) %>%
  # clean up the "night" variable and "date" column to actually correspond 
  # with each night of trapping
  mutate(
    night = as.numeric(as.vector(str_match(night, "[0-9]"))),
    date = case_when(
      night == 1 ~ date,
      night == 2 ~ date + days(1),
      night == 3 ~ date + days(2),
      night == 4 ~ date + days(3)
    )
  )

# Add on info about rodent catch for each of these trapping occasions
o2 <- o %>%
  left_join(
    .,
    capture.dat %>%
      filter(habitat_code == "H") %>%
      select(site, visit, track_id, night, species),
    by = c("site", "visit", "track_id", "night")
  ) %>%
  group_by(date, site, visit, track_id, house_id, night, trap_count) %>%
  summarize(
    n_rodents = sum(!is.na(species)),
    n_Mna = sum(species == "Mastomys natalensis"),
    n_Rra = sum(species == "Rattus rattus")
  ) %>%
  ungroup() %>%
  mutate(
    n_Mna = ifelse(is.na(n_Mna), 0, n_Mna),
    n_Rra = ifelse(is.na(n_Rra), 0, n_Rra),
    Mna_detected = ifelse(n_Mna > 0, 1, 0),
    Rra_detected = ifelse(n_Rra > 0, 1, 0)
  ) %>%
  arrange(site, visit, house_id, night) %>%
  left_join(
    .,
    house.level.captures %>%
      select(house_id, Rra_at_site, wet_season),
    by = "house_id"
  )

# Pivot longer to get all trapping occasions from a single house on one row
o3 <- o2 %>%
  pivot_wider(
    id_cols = c(site, visit, track_id, house_id, Rra_at_site, wet_season),
    names_from = "night",
    values_from = "Mna_detected"
  )

# If there are 3 NAs in a given row it means we only have one observation: drop
good.indices <- which(rowSums(is.na(o3[, 7:10])) < 3)
o3 <- slice(o3, good.indices)

# Generate occupancy covariates
occ.covs <- data.frame(
  site = o3$site,
  site_numeric = as.numeric(as.factor(o3$site)),
  house_numeric = as.numeric(as.factor(o3$house_id)),
  Rra_at_site = o3$Rra_at_site,
  wet_season = o3$wet_season
)

# Generate detection covariate: trap count
det.covs.trap.count <- o2 %>%
  pivot_wider(
    id_cols = house_id,
    names_from = "night",
    values_from = "trap_count"
  ) %>%
  slice(good.indices) %>%
  select(-house_id)

# Generate detection covariate: cumulative Mna catch
temp.cumulative.counts <- o2 %>%
  group_by(house_id) %>%
  mutate(cumulative_Mna_count = cumsum(n_Mna)) %>%
  select(site, visit, house_id, night, cumulative_Mna_count)

det.covs.cumulative.Mna.count <- o2 %>%
  left_join(
    .,
    temp.cumulative.counts %>%
      mutate(night = night + 1),
    by = c("site", "visit", "house_id", "night")
  ) %>%
  group_by(house_id) %>%
  # for every house, the first trapping occasion has 0 cumulative catch
  mutate(cumulative_Mna_count = replace(cumulative_Mna_count, 1, 0)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = house_id,
    names_from = "night",
    values_from = "cumulative_Mna_count"
  ) %>%
  ungroup() %>%
  slice(good.indices) %>%
  select(-house_id)

# Generate detection covariate list
det.covs <- list(
  trap_count = det.covs.trap.count,
  cumulative_Mna_count = det.covs.cumulative.Mna.count
)

# Generate data list for model fitting
data.list <- list(
  y = o3[, 7:10],
  occ.covs = occ.covs,
  det.covs = det.covs
)

#==============================================================================


# Occupancy analyses - model fitting and interpretation

# What is average site-level occupancy?
o3 %>%
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
o3 %>%
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

# What is occupancy condition on R. rattus presence and season
# (no pooling by site)
o3 %>%
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
  sigma.sq.psi.ig = list(shape = 2, scale = 1)
)

# Fit an occupancy model
out <- spOccupancy::PGOcc(
  occ.formula = ~ Rra_at_site + wet_season + (1|site_numeric) + (1|house_numeric), 
  det.formula = ~ trap_count + cumulative_Mna_count, 
  data = data.list, 
  priors = priors.list,
  n.burn = 10000,
  n.samples = 35000,
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
  geom_vline(xintercept = 0, lty = 2, size = 2) +
  xlab(expression(paste("Occupancy coefficient for ", italic("Rattus rattus"), " presence"))) +
  ylab("Density") +
  xlim(-6, 2) +
  ylim(0, 0.6) +
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

for(site in unique(o3$site)) {
  
  occ_prob <- apply(out$z.samples[ , which(o3$site == site)], 1, sum)/length(which(o3$site == site))
  temp <- data.frame(
    site = rep(as.character(site), length(occ_prob)),
    occ_prob
  )
  
  site.preds <- bind_rows(site.preds, temp)
}

site.preds <- site.preds %>%
  left_join(
    .,
    o3 %>%
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

naive.occ <- o2 %>%
  group_by(site, house_id, Rra_at_site) %>%
  summarize(
    house_w_Mna = ifelse(sum(n_Mna) > 0, 1, 0)
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
    c("Wet season", "Dry season"), 
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

#==============================================================================


occ.plot <- preds %>%
  ggplot(aes(x = occ_prob, color = site_status)) +
  geom_density(aes(fill = site_status), size = 1) +
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
    lty = 2, size = 2
  ) +
  geom_vline(
    xintercept = preds %>%
      filter(site_status == "Present") %>%
      pull(occ_prob) %>%
      median(),
    color = "darkred",
    lty = 2, size = 2
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
    linewidth = 1
  ) +
  geom_linerange(
    aes(ymin = lower90, ymax = upper90), 
    position = position_dodge2(width = val.dodge),
    linewidth = 3
  ) +
  geom_point(position = position_dodge2(width = val.dodge), size = 5) +
  xlab(expression(paste(italic("Rattus rattus"), " status at site"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "house-level occupancy"))) +
  scale_y_continuous(limits = c(0, 0.6)) +
  theme_minimal() +
  scale_color_manual(values = c("wheat3", "steelblue")) +
  theme(
    text = element_text(size = 21),
    legend.position = "none"
  )

cowplot::plot_grid(
  house.yes.no, occ.plot, 
  nrow = 1,
  labels = "auto",
  label_size = 22
)

ggsave("outputs/house_level_analyses.jpeg", 
       width = 3000, height = 1500, unit = "px")
