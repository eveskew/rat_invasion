library(tidyverse)
library(rethinking)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

source("R/functions.R")

#==============================================================================


# Import combined site-level datasets
site.dat <- read_csv("data/clean/combined/site_level_data.csv")
site.dat.only.houses <- read_csv(
  "data/clean/combined/site_level_data_only_houses.csv"
)

#==============================================================================


# Make a paneled plot of trapping success across Sierra Leone and Guinea, 
# using all site-level data

sl <- ne_countries(
  country = c("Guinea", "Sierra Leone"),
  scale = "medium",
  returnclass = "sf"
)
site.points <- site.dat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(sl))

breaks.all <- c(0, 0.02, 0.04, 0.06, 0.08, 0.1)

a.all <- ggplot() +
  geom_sf(data = sl, fill = alpha("darkgreen", 0.9), color = "black", size = 1) +
  geom_sf(data = site.points, aes(color = Mna_per_trap), size = 5) +
  scale_color_gradient(
    low = "white",
    high = "darkred",
    breaks = breaks.all,
    limits = c(0, 0.1)
  ) +
  guides(
    color = guide_legend(
      title = expression(paste(italic("Mastomys natalensis"), " catch per trap")),
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.text = element_blank(),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

b.all <- ggplot() +
  geom_sf(data = sl, fill = alpha("darkgreen", 0.9), color = "black", size = 1) +
  geom_sf(data = site.points, aes(color = Rra_per_trap), size = 5) +
  scale_color_gradient(
    low = "white",
    high = "darkred",
    breaks = breaks.all,
    limits = c(0, 0.1)
  ) +
  guides(
    color = guide_legend(
      title = expression(paste(italic("Rattus rattus"), " catch per trap")),
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.text = element_blank(),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

top.row.all <- cowplot::plot_grid(
  a.all, b.all,
  labels = "auto",
  label_size = 22
)

c.all <- ggplot(
  data = site.dat,
  aes(x = Rra_per_trap, y = Mna_per_trap)) +
  geom_point(size = 6) +
  xlab(expression(paste(italic("Rattus rattus"), " catch per trap"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap"))) +
  scale_x_continuous(breaks = breaks.all, limits = c(0, 0.1)) +
  scale_y_continuous(breaks = breaks.all, limits = c(0, 0.1)) +
  theme_minimal() +
  theme(
    text = element_text(size = 20)
  )

cowplot::plot_grid(
  top.row.all, c.all,
  nrow = 2,
  rel_heights = c(0.55, 0.45),
  labels = c("", "c"),
  label_size = 22
)

#ggsave("outputs/sites_all_traps.jpeg", width = 4000, height = 3000, unit = "px")


# Make a paneled plot of trapping success across Sierra Leone and Guinea, 
# using only house trapping data

sl <- ne_countries(
  country = c("Guinea", "Sierra Leone"),
  scale = "medium",
  returnclass = "sf"
) 
site.points <- site.dat.only.houses %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(sl))

breaks.house <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25)

a.house <- ggplot() +
  geom_sf(data = sl, fill = alpha("darkgreen", 0.9), color = "black", size = 1) +
  geom_sf(data = site.points, aes(color = Mna_per_trap), size = 5) +
  scale_color_gradient(
    low = "white",
    high = "darkred",
    breaks = breaks.house,
    limits = c(0, 0.25)
  ) +
  guides(
    color = guide_legend(
      title = expression(paste(italic("Mastomys natalensis"), " catch per trap")),
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.text = element_blank(),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

b.house <- ggplot() +
  geom_sf(data = sl, fill = alpha("darkgreen", 0.9), color = "black", size = 1) +
  geom_sf(data = site.points, aes(color = Rra_per_trap), size = 5) +
  scale_color_gradient(
    low = "white",
    high = "darkred",
    breaks = breaks.house,
    limits = c(0, 0.25)
  ) +
  guides(
    color = guide_legend(
      title = expression(paste(italic("Rattus rattus"), " catch per trap")),
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.text = element_blank(),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

top.row.house <- cowplot::plot_grid(
  a.house, b.house,
  labels = "auto",
  label_size = 22
)

c.house <- ggplot(
  data = site.dat.only.houses,
  aes(x = Rra_per_trap, y = Mna_per_trap)) +
  geom_point(size = 6) +
  xlab(expression(paste(italic("Rattus rattus"), " catch per trap"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap"))) +
  scale_x_continuous(breaks = breaks.house, limits = c(0, 0.25)) +
  scale_y_continuous(breaks = breaks.house, limits = c(0, 0.25)) +
  theme_minimal() +
  theme(
    text = element_text(size = 20)
  )

cowplot::plot_grid(
  top.row.house, c.house,
  nrow = 2,
  rel_heights = c(0.55, 0.45),
  labels = c("", "c"),
  label_size = 22
)

#ggsave("outputs/sites_house_traps.jpeg", width = 4000, height = 3000, unit = "px")

#==============================================================================


# Statistical analyses at site level, all traps


# Package data for Stan models, with all traps included
stan.dat <- list(
  N = nrow(site.dat),
  n_Mna = site.dat$n_Mna,
  n_Rra = site.dat$n_Rra,
  n_Rra_s = standardize(site.dat$n_Rra),
  Rra_at_site = site.dat$Rra_at_site,
  tot_traps = site.dat$tot_traps,
  log_tot_traps = log(site.dat$tot_traps),
  N_site = n_distinct(site.dat$site),
  site = as.numeric(as.factor(site.dat$site))
)

# Load model
site.mod.Rra.at.site <- cmdstan_model("stan_models/site_model_Rra_at_site.stan")

# Fit model
fit.m1 <- site.mod.Rra.at.site$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m1$save_object("saved_models/site_mod_all_traps_Rra_at_site.RDS")
fit.m1 <- readRDS("saved_models/site_mod_all_traps_Rra_at_site.RDS")

fit.m1$diagnostic_summary()
fit.m1$print(max_rows = 100)

draws.m1 <- fit.m1$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, sigma_site)

jpeg("outputs/misc/model_out_site_level_all_traps_Rra_at_site.jpeg",
     width = 1000, height = 500, units = "px")

parameter_summary(draws.m1, prob = 0.99)
plot(precis(draws.m1, prob = 0.99), col = "gold2")
plot(precis(draws.m1, prob = 0.9), add = TRUE)

dev.off()


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
  xlim(-4, 1) +
  ylim(0, 1) +
  scale_fill_manual(values = c(alpha("darkred", 0.3), "white")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "none"
  )

ggsave("outputs/misc/site_level_all_traps_Rra_at_site_coefficient.jpeg",
       width = 2000, height = 1600, units = "px")


# Compute site-level predictions from model output

preds <- site.dat %>% 
  select(site, tot_traps) %>%
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

# Plot demonstrating shrinkage: sites with less trapping effort show greater
# shrinkage (black points) towards the group-level means
preds %>%
  group_by(site) %>%
  summarize(
    mean = mean(lambda_inv_link)
  ) %>%
  ungroup() %>%
  left_join(
    .,
    site.dat %>%
      select(site, Mna_per_trap, Rra_at_site, tot_traps),
    by = "site"
  ) %>%
  ggplot() +
  geom_point(aes(x = tot_traps, y = Mna_per_trap, col = as.factor(Rra_at_site))) +
  geom_point(aes(x = tot_traps, y = mean)) +
  geom_hline(yintercept = mean(exp(draws.m1$a)), lty = 2, color = "darkgrey") +
  geom_hline(yintercept = mean(exp(draws.m1$a + draws.m1$bR)), lty = 2, color = "darkred") +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme_minimal()

# Generate posterior predictive check plot
preds.summary <- preds %>%
  group_by(site) %>%
  summarize(
    lower90 = HPDI(pred, 0.9)[1],
    upper90 = HPDI(pred, 0.9)[2],
    lower99 = HPDI(pred, 0.99)[1],
    upper99 = HPDI(pred, 0.99)[2],
  ) %>%
  ungroup() %>%
  left_join(
    .,
    site.dat %>%
      select(site, n_Mna),
    by = "site"
  ) %>%
  mutate(
    in_interval = ifelse(n_Mna <= upper99 & n_Mna >= lower99, TRUE, FALSE)
  )

sum(preds.summary$in_interval)

ggplot() +
  geom_linerange(
    aes(x = site, ymin = lower99, ymax = upper99),
    data = preds.summary, linewidth = 0.5
  ) +
  geom_linerange(
    aes(x = site, ymin = lower90, ymax = upper90),
    data = preds.summary, linewidth = 1.5
  ) +
  geom_point(
    aes(x = site, y = n_Mna, color = as.factor(Rra_at_site)), 
    data = site.dat, size = 2
  ) +
  theme_minimal() +
  ylab(expression(paste("Number of ", italic("Mastomys natalensis"), " captured"))) +
  ylim(0, 400) +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    text = element_text(size = 20),
    legend.position = "none",
    axis.title.x = element_blank()
  )

ggsave("outputs/misc/predictive_check_site_level_all_traps_Rra_at_site.jpeg", 
       width = 3000, height = 2000, unit = "px")

 
# Compare predictions to an intercept-only model

site.mod.int <- cmdstan_model("stan_models/site_model_int_only.stan")

fit.int <- site.mod.int$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99
)

fit.int$diagnostic_summary()
fit.int$print(max_rows = 100)

draws.int <- fit.int$draws(format = "matrix") %>%
  data.frame() %>%
  select(-lp__)

preds <- data.frame(
  site = rep(site.dat$site, each = nrow(draws.int)),
  tot_traps = rep(site.dat$tot_traps, each = nrow(draws.int)),
  lambda = rep(draws.int$a, times = n_distinct(site.dat$site))
) %>%
  mutate(
    lambda_inv_link = exp(lambda)
  )

set.seed(1)
preds$pred <- sapply(1:nrow(preds), function(x)
  sum(rpois(preds$tot_traps[x], preds$lambda_inv_link[x]))
)

preds.summary <- preds %>%
  group_by(site) %>%
  summarize(
    lower90 = HPDI(pred, 0.9)[1],
    upper90 = HPDI(pred, 0.9)[2],
    lower99 = HPDI(pred, 0.99)[1],
    upper99 = HPDI(pred, 0.99)[2],
  ) %>%
  ungroup() %>%
  left_join(
    .,
    site.dat %>%
      select(site, n_Mna),
    by = "site"
  ) %>%
  mutate(
    in_interval = ifelse(n_Mna <= upper99 & n_Mna >= lower99, TRUE, FALSE)
  )

sum(preds.summary$in_interval)

ggplot() +
  geom_linerange(
    aes(x = site, ymin = lower99, ymax = upper99),
    data = preds.summary, linewidth = 0.5
  ) +
  geom_linerange(
    aes(x = site, ymin = lower90, ymax = upper90),
    data = preds.summary, linewidth = 1.5
  ) +
  geom_point(
    aes(x = site, y = n_Mna, col = as.factor(Rra_at_site)), 
    data = site.dat, size = 2
  ) +
  theme_minimal() +
  ylab(expression(paste("Number of ", italic("Mastomys natalensis"), " captured"))) +
  ylim(0, 400) +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    text = element_text(size = 20),
    legend.position = "none",
    axis.title.x = element_blank()
  )

ggsave("outputs/misc/predictive_check_site_level_all_traps_int_only.jpeg", 
       width = 3000, height = 2000, unit = "px")

#==============================================================================


# Statistical analyses at site level, only house traps

# Package data for Stan models, with only houses included
stan.dat <- list(
  N = nrow(site.dat.only.houses),
  n_Mna = site.dat.only.houses$n_Mna,
  n_Rra = site.dat.only.houses$n_Rra,
  n_Rra_s = standardize(site.dat.only.houses$n_Rra),
  Rra_at_site = site.dat.only.houses$Rra_at_site,
  tot_traps = site.dat.only.houses$tot_traps,
  log_tot_traps = log(site.dat.only.houses$tot_traps),
  N_site = n_distinct(site.dat.only.houses$site),
  site = as.numeric(as.factor(site.dat.only.houses$site))
)

# Fit model
fit.m2 <- site.mod.Rra.at.site$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m2$save_object("saved_models/site_mod_house_traps_Rra_at_site.RDS")
fit.m2 <- readRDS("saved_models/site_mod_house_traps_Rra_at_site.RDS")

fit.m2$diagnostic_summary()
fit.m2$print(max_rows = 100)

draws.m2 <- fit.m2$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, sigma_site)

jpeg("outputs/misc/model_out_site_level_house_traps_Rra_at_site.jpeg",
     width = 1000, height = 500, units = "px")

parameter_summary(draws.m2, prob = 0.99)
plot(precis(draws.m2, prob = 0.99), col = "gold2")
plot(precis(draws.m2, prob = 0.9), add = TRUE)

dev.off()


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
  xlim(-5, 1) +
  ylim(0, 1) +
  scale_fill_manual(values = c(alpha("darkred", 0.3), "white")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "none"
  )

ggsave("outputs/misc/site_level_house_traps_Rra_at_site_coefficient.jpeg",
       width = 2000, height = 1600, units = "px")


# Compute site-level predictions from model output

preds <- site.dat.only.houses %>% 
  select(site, tot_traps) %>%
  uncount(nrow(draws.m2)) %>%
  mutate(
    lambda = fit.m2$draws(format = "matrix") %>%
      data.frame() %>%
      select(contains("lambda")) %>%
      unlist(),
    lambda_inv_link = exp(lambda)
  )

set.seed(1)
preds$pred <- sapply(1:nrow(preds), function(x)
  sum(rpois(preds$tot_traps[x], preds$lambda_inv_link[x]))
)

# Generate posterior predictive check plot
preds.summary <- preds %>%
  group_by(site) %>%
  summarize(
    lower90 = HPDI(pred, 0.9)[1],
    upper90 = HPDI(pred, 0.9)[2],
    lower99 = HPDI(pred, 0.99)[1],
    upper99 = HPDI(pred, 0.99)[2],
  ) %>%
  ungroup() %>%
  left_join(
    .,
    site.dat.only.houses %>%
      select(site, n_Mna),
    by = "site"
  ) %>%
  mutate(
    in_interval = ifelse(n_Mna <= upper99 & n_Mna >= lower99, TRUE, FALSE)
  )

sum(preds.summary$in_interval)

ggplot() +
  geom_linerange(
    aes(x = site, ymin = lower99, ymax = upper99),
    data = preds.summary, linewidth = 0.5
  ) +
  geom_linerange(
    aes(x = site, ymin = lower90, ymax = upper90),
    data = preds.summary, linewidth = 1.5
  ) +
  geom_point(
    aes(x = site, y = n_Mna, color = as.factor(Rra_at_site)), 
    data = site.dat.only.houses, size = 2
  ) +
  theme_minimal() +
  ylab(expression(paste("Number of ", italic("Mastomys natalensis"), " captured"))) +
  ylim(0, 400) +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    text = element_text(size = 20),
    legend.position = "none",
    axis.title.x = element_blank()
  )

ggsave("outputs/misc/predictive_check_site_level_house_traps_Rra_at_site.jpeg", 
       width = 3000, height = 2000, unit = "px")

#==============================================================================


# Plot implications of the two models with Rra_at_site predictors

preds <- data.frame(
  data_type = rep(
    c("All traps", "House traps only"), 
    each = length(draws.m1$a)*2
  ),
  site_status = rep(
    c("Absent", "Present", 
      "Absent", "Present"),
    each = length(draws.m1$a)
  ),
  catch_per_trap = c(
    exp(draws.m1$a),
    exp(draws.m1$a + draws.m1$bR),
    exp(draws.m2$a),
    exp(draws.m2$a + draws.m2$bR)
  )
)

# Initial plot with all four combinations
preds %>%
  group_by(data_type, site_status) %>%
  summarize(
    mean = mean(catch_per_trap),
    lower99 = HPDI(catch_per_trap, 0.99)[1],
    upper99 = HPDI(catch_per_trap, 0.99)[2]
  ) %>%
  ungroup() %>%
  ggplot(aes(x = data_type, y = mean, ymin = lower99, ymax = upper99, color = site_status)) +
  geom_pointrange(size = 1.2) +
  scale_y_continuous(limits = c(0, 0.1)) +
  xlab("") +
  ylab(expression(paste("Average ", italic("Mastomys natalensis"), " catch per trap"))) +
  theme_minimal() +
  scale_color_manual(values = c("black", "darkred")) +
  guides(
    color = guide_legend(
      title = expression(paste(italic("Rattus rattus"))),
      title.position = "top"
    )
  ) +
  theme(
    text = element_text(size = 21)
  )

# Table giving observed catch per trap in both datasets (i.e., complete pooling 
# estimates): this is what you'd get in a model without varying effects
site.dat %>% 
  group_by(Rra_at_site) %>% 
  summarize(
    n_Mna = sum(n_Mna),
    tot_traps = sum(tot_traps),
    catch_per_trap = n_Mna/tot_traps
  )

site.dat.only.houses %>% 
  group_by(Rra_at_site) %>% 
  summarize(
    n_Mna = sum(n_Mna),
    tot_traps = sum(tot_traps),
    catch_per_trap = n_Mna/tot_traps
  )


# Integrate information with other panels to make alternative plots

d.all <- preds %>%
  filter(data_type == "All traps") %>%
  group_by(site_status) %>%
  summarize(
    mean = mean(catch_per_trap),
    lower99 = HPDI(catch_per_trap, 0.99)[1],
    upper99 = HPDI(catch_per_trap, 0.99)[2],
    lower90 = HPDI(catch_per_trap, 0.9)[1],
    upper90 = HPDI(catch_per_trap, 0.9)[2]
  ) %>%
  ungroup() %>%
  ggplot(aes(x = site_status, y = mean, color = site_status)) +
  geom_linerange(aes(ymin = lower99, ymax = upper99), linewidth = 1) +
  geom_linerange(aes(ymin = lower90, ymax = upper90), linewidth = 3) +
  geom_point(size = 5) +
  xlab(expression(paste(italic("Rattus rattus"), " status at site"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap (average site)"))) +
  scale_y_continuous(breaks = breaks.all, limits = c(0, 0.05)) +
  theme_minimal() +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(
    text = element_text(size = 21),
    legend.position = "none"
  )

cowplot::plot_grid(
  a.all, b.all, c.all, d.all,
  nrow = 2,
  rel_heights = c(0.55, 0.45),
  labels = "auto",
  label_size = 22
)

ggsave(
  "outputs/sites_all_traps.jpeg", 
  width = 4000, 
  height = 3500, 
  unit = "px"
)

d.house <- preds %>%
  filter(data_type == "House traps only") %>%
  group_by(site_status) %>%
  summarize(
    mean = mean(catch_per_trap),
    lower99 = HPDI(catch_per_trap, 0.99)[1],
    upper99 = HPDI(catch_per_trap, 0.99)[2],
    lower90 = HPDI(catch_per_trap, 0.9)[1],
    upper90 = HPDI(catch_per_trap, 0.9)[2]
  ) %>%
  ungroup() %>%
  ggplot(aes(x = site_status, y = mean, color = site_status)) +
  geom_linerange(aes(ymin = lower99, ymax = upper99), linewidth = 1) +
  geom_linerange(aes(ymin = lower90, ymax = upper90), linewidth = 3) +
  geom_point(size = 5) +
  xlab(expression(paste(italic("Rattus rattus"), " status at site"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap (average site)"))) +
  scale_y_continuous(breaks = breaks.house, limits = c(0, 0.1)) +
  theme_minimal() +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(
    text = element_text(size = 21),
    legend.position = "none"
  )

cowplot::plot_grid(
  a.house, b.house, c.house, d.house,
  nrow = 2,
  rel_heights = c(0.55, 0.45),
  labels = "auto",
  label_size = 22
)

ggsave(
  "outputs/sites_house_traps.jpeg", 
  width = 4000, 
  height = 3500, 
  unit = "px"
)

#==============================================================================


# Import McCormick trapping data for comparison

# McCormick raw data
m <- read_csv("data/raw/McCormick/McCormick_etal_1987.csv") %>%
  mutate(
    # how many houses are implied by the stated data?
    n_houses_infer = ceiling(n_Mna/avg_Mna_per_house),
    # for all villages except Niahun and Konia, the data seem consistent
    # with the stated sampling of 40 houses in each village
    n_houses = ifelse(village %in% c("Niahun", "Konia"), n_houses_infer, 40),
    # at each site, 10 traps were set in each house for 5 to 7 nights
    low_trap_night_estimate = n_houses*50,
    med_trap_night_estimate = n_houses*60,
    high_trap_night_estimate = n_houses*70,
    # calculate trap success
    low_trap_success_estimate = n_Mna/high_trap_night_estimate,
    med_trap_success_estimate = n_Mna/med_trap_night_estimate,
    high_trap_success_estimate = n_Mna/low_trap_night_estimate,
    # state data source
    data_source = rep("McCormick", n())
  )

# Combine our house-only data with McCormick data
m2 <- site.dat.only.houses %>%
  mutate(
    data_timing = rep("Contemporary", n())
  ) %>%
  bind_rows(
    .,
    m %>%
      select(
        village, n_rodent_total, n_Mna, med_trap_night_estimate,
        low_trap_success_estimate, med_trap_success_estimate,
        high_trap_success_estimate, longitude, latitude, data_source
      ) %>%
      rename(
        site = village,
        tot_traps = med_trap_night_estimate,
        n_catch = n_rodent_total,
        Mna_per_trap = med_trap_success_estimate
      ) %>%
      mutate(
        data_timing = rep("Historical", n())
      )
  )


# Plot geography of historical and contemporary sites

sl <- ne_states(
  country = c("Sierra Leone", "Guinea"),
  returnclass = "sf"
)
site.points <- m2 %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(sl))

ggplot() +
  geom_sf(data = sl, fill = alpha("darkgreen", 0.8)) +
  geom_sf(data = site.points, aes(color = Mna_per_trap), size = 4) +
  scale_color_gradientn(colors = c("white", "darkred")) +
  guides(
    color = guide_legend(
      title = expression(atop(italic("Mastomys natalensis"), "catch per trap")),
      reverse = T
    )
  ) +
  facet_grid(~data_timing) +
  theme_minimal()

# ggsave("outputs/Mna_catch_per_trap_map_hist_vs_contemp.jpeg",
#        width = 2500, height = 1500, units = "px")


# Compare McCormick trapping success versus nearby contemporary sites

# Filter to only sites that lie in the eastern province of Sierra Leone,
# where the original McCormick sampling happened
sl <- ne_states(
  country = "Sierra Leone",
  returnclass = "sf"
)
site.points.mod <- site.points %>%
  filter(st_intersects(site.points, sl) %in% c(2))

ggplot() +
  geom_sf(data = sl) +
  geom_sf(data = site.points.mod, aes(color = data_source)) +
  theme_minimal()

# Violin plot of contemporary and historical catch per trap
set.seed(2)
mc.a <- site.points.mod %>%
  mutate(data_timing = forcats::fct_rev(data_timing)) %>%
  ggplot(aes(x = data_timing, y = Mna_per_trap)) +
  geom_violin(fill = alpha("lightgrey", 0.4)) +
  geom_jitter(aes(color = data_timing), height = 0, width = 0.15, size = 7) +
  xlab("") +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 0.1, by = .02), limits = c(0, 0.1)) +
  scale_color_manual(values = c(alpha("forestgreen", 0.7), alpha("dodgerblue3", 0.7))) +
  theme_minimal() +
  theme(
    text = element_text(size = 22),
    legend.position = "none"
  )

mc.a


# Package Stan data
stan.dat <- list(
  N = nrow(site.points.mod),
  n_catch = site.points.mod$n_catch,
  n_Mna = site.points.mod$n_Mna,
  tot_traps = site.points.mod$tot_traps,
  log_tot_traps = log(site.points.mod$tot_traps),
  data_timing = site.points.mod %>%
    mutate(data_timing_binary = ifelse(data_timing == "Contemporary", 1, 0)) %>%
    pull(data_timing_binary),
  N_site = n_distinct(site.points.mod$site),
  site = as.numeric(as.factor(site.points.mod$site))
)

# Summarize data
site.points.mod %>%
  group_by(data_timing) %>%
  summarize(
    n_Mna = sum(n_Mna),
    tot_traps = sum(tot_traps),
    Mna_per_trap = n_Mna/tot_traps
  )

# Load model
McCormick.mod <- cmdstan_model("stan_models/site_model_McCormick.stan")

# Fit model
fit.mc <- McCormick.mod$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.mc$save_object("saved_models/site_mod_McCormick.RDS")
fit.mc <- readRDS("saved_models/site_mod_McCormick.RDS")

fit.mc$diagnostic_summary()
fit.mc$print(max_rows = 100)

draws.mc <- fit.mc$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bT, sigma_site)

jpeg("outputs/misc/model_out_McCormick.jpeg",
     width = 1000, height = 500, units = "px")

parameter_summary(draws.mc, prob = 0.99)
plot(precis(draws.mc, prob = 0.99), col = "gold2")
plot(precis(draws.mc, prob = 0.9), add = TRUE)

dev.off()

# What posterior probability mass supports a negative effect of contemporary
# sampling (relative to historical sampling)?
sum(draws.mc$bT < 0)/nrow(draws.mc)


# Plot implications of the model

preds <- data.frame(
  data_timing = rep(
    c("Historical", "Contemporary"),
    each = nrow(draws.mc)
  ),
  catch_per_trap = c(
    exp(draws.mc$a),
    exp(draws.mc$a + draws.mc$bT)
  )
)

mc.b <- preds %>%
  group_by(data_timing) %>%
  summarize(
    mean = mean(catch_per_trap),
    lower99 = HPDI(catch_per_trap, 0.99)[1],
    upper99 = HPDI(catch_per_trap, 0.99)[2],
    lower90 = HPDI(catch_per_trap, 0.9)[1],
    upper90 = HPDI(catch_per_trap, 0.9)[2]
  ) %>%
  ungroup() %>%
  mutate(data_timing = forcats::fct_rev(data_timing)) %>%
  ggplot(aes(x = data_timing, y = mean, color = data_timing)) +
  geom_linerange(aes(ymin = lower99, ymax = upper99), linewidth = 1) +
  geom_linerange(aes(ymin = lower90, ymax = upper90), linewidth = 3) +
  geom_point(size = 5) +
  xlab("") +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap (average site)"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 0.1, by = .02), limits = c(0, 0.1)) +
  scale_color_manual(values = c("forestgreen", "dodgerblue3")) +
  theme_minimal() +
  theme(
    text = element_text(size = 21),
    legend.position = "none"
  )

cowplot::plot_grid(
  mc.a, mc.b,
  nrow = 1,
  labels = "auto",
  label_size = 22
)

ggsave("outputs/Mna_catch_per_trap_hist_vs_contemp.jpeg",
       width = 3000, height = 1500, units = "px")

#==============================================================================


# Spillover risk analyses

# What is the range of catch per trap of Lassa-positive Mastomys natalensis?
summary(site.dat.only.houses$n_Mna_pos_lassa/site.dat.only.houses$tot_traps)

# Package data for Stan models, with only houses included
stan.dat <- list(
  N = nrow(site.dat.only.houses),
  n_Mna = site.dat.only.houses$n_Mna,
  n_Mna_pos_lassa = site.dat.only.houses$n_Mna_pos_lassa,
  n_Rra = site.dat.only.houses$n_Rra,
  n_Rra_s = standardize(site.dat.only.houses$n_Rra),
  Rra_at_site = site.dat.only.houses$Rra_at_site,
  tot_traps = site.dat.only.houses$tot_traps,
  log_tot_traps = log(site.dat.only.houses$tot_traps),
  N_site = n_distinct(site.dat.only.houses$site),
  site = as.numeric(as.factor(site.dat.only.houses$site))
)

# Load model
spill.mod <- cmdstan_model("stan_models/spillover_risk_site_level.stan")

# Fit model
fit.spill <- spill.mod$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.spill$save_object("saved_models/spillover_risk_site_level.RDS")
fit.spill <- readRDS("saved_models/spillover_risk_site_level.RDS")

fit.spill$diagnostic_summary()
fit.spill$print(max_rows = 100)

draws.spill <- fit.spill$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, sigma_site)

jpeg("outputs/misc/model_out_spillover_risk_site_level.jpeg",
     width = 1000, height = 500, units = "px")

parameter_summary(draws.spill, prob = 0.99)
plot(precis(draws.spill, prob = 0.99), col = "gold2")
plot(precis(draws.spill, prob = 0.9), add = TRUE)

dev.off()


# Generate figure of the Rra_at_site posterior (have to do some of this 
# manually)
cutoff <- 0
hist <- density(draws.spill$bR, from = -10, to = 10)
hist <- data.frame(x = hist$x, y = hist$y) %>%
  mutate(area = x >= cutoff)

hist %>%
  ggplot(aes(x = x, ymin = 0, ymax = y, fill = area)) +
  geom_ribbon() +
  geom_line(aes(y = y), linewidth = 1) +
  geom_vline(xintercept = 0, lty = 2, linewidth = 2) +
  xlab(expression(paste("Coefficient for ", italic("Rattus rattus"), " presence"))) +
  ylab("Density") +
  xlim(-5, 1) +
  ylim(0, 1) +
  scale_fill_manual(values = c(alpha("darkred", 0.3), "white")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "none"
  )

ggsave("outputs/misc/spillover_risk_model_site_level_Rra_at_site_coefficient.jpeg",
       width = 2000, height = 1600, units = "px")


# Compute site-level predictions from model output

preds <- site.dat.only.houses %>% 
  select(site, tot_traps) %>%
  uncount(nrow(draws.spill)) %>%
  mutate(
    lambda = fit.spill$draws(format = "matrix") %>%
      data.frame() %>%
      select(contains("lambda")) %>%
      unlist(),
    lambda_inv_link = exp(lambda)
  )

set.seed(1)
preds$pred <- sapply(1:nrow(preds), function(x)
  sum(rpois(preds$tot_traps[x], preds$lambda_inv_link[x]))
)

# Generate posterior predictive check plot
preds.summary <- preds %>%
  group_by(site) %>%
  summarize(
    lower90 = HPDI(pred, 0.9)[1],
    upper90 = HPDI(pred, 0.9)[2],
    lower99 = HPDI(pred, 0.99)[1],
    upper99 = HPDI(pred, 0.99)[2],
  ) %>%
  ungroup() %>%
  left_join(
    .,
    site.dat.only.houses %>%
      select(site, n_Mna_pos_lassa),
    by = "site"
  ) %>%
  mutate(
    in_interval = ifelse(n_Mna_pos_lassa <= upper99 & n_Mna_pos_lassa >= lower99, TRUE, FALSE)
  )

sum(preds.summary$in_interval)

ggplot() +
  geom_linerange(
    aes(x = site, ymin = lower99, ymax = upper99),
    data = preds.summary, linewidth = 0.5
  ) +
  geom_linerange(
    aes(x = site, ymin = lower90, ymax = upper90),
    data = preds.summary, linewidth = 1.5
  ) +
  geom_point(
    aes(x = site, y = n_Mna_pos_lassa, color = as.factor(Rra_at_site)), 
    data = site.dat.only.houses, size = 2
  ) +
  theme_minimal() +
  ylab(expression(paste("Number of ", italic("Mastomys natalensis"), " captured"))) +
  ylim(0, 50) +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    text = element_text(size = 20),
    legend.position = "none",
    axis.title.x = element_blank()
  )

ggsave("outputs/misc/predictive_check_spillover_risk_site_level.jpeg", 
       width = 3000, height = 2000, unit = "px")


# Generate figure

sl <- ne_countries(
  country = c("Guinea", "Sierra Leone"),
  scale = "medium", 
  returnclass = "sf"
)
site.points <- site.dat.only.houses %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(sl))

breaks.spill <- seq(from = 0, to = 0.016, by = 0.004)

spill.map.house <- ggplot() +
  geom_sf(data = sl, fill = alpha("darkgreen", 0.9), color = "black", size = 1) +
  geom_sf(data = site.points, aes(color = n_Mna_pos_lassa/tot_traps), size = 5) +
  scale_color_gradient(
    low = "white",
    high = "darkred",
    breaks = breaks.spill,
    limits = c(0, 0.02)
  ) +
  guides(
    color = guide_legend(
      title = expression(
        atop(
          paste("Lassa-positive ", italic("Mastomys natalensis")), 
          "catch per trap"
        )
      ),
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.text = element_blank(),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

preds <- data.frame(
  site_status = rep(
    c("Absent", "Present"),
    each = length(draws.spill$a)
  ),
  catch_per_trap = c(
    exp(draws.spill$a), 
    exp(draws.spill$a + draws.spill$bR)
  )
)

spill.results.house <- preds %>%
  group_by(site_status) %>%
  summarize(
    mean = mean(catch_per_trap),
    lower99 = HPDI(catch_per_trap, 0.99)[1],
    upper99 = HPDI(catch_per_trap, 0.99)[2],
    lower90 = HPDI(catch_per_trap, 0.9)[1],
    upper90 = HPDI(catch_per_trap, 0.9)[2]
  ) %>%
  ungroup() %>%
  ggplot(aes(x = site_status, y = mean, color = site_status)) +
  geom_linerange(aes(ymin = lower99, ymax = upper99), linewidth = 1) +
  geom_linerange(aes(ymin = lower90, ymax = upper90), linewidth = 3) +
  geom_point(size = 5) +
  xlab(expression(paste(italic("Rattus rattus"), " status at site"))) +
  ylab(expression(atop(paste("Lassa-positive ", italic("Mastomys natalensis")), "catch per trap (average site)"))) +
  scale_y_continuous(breaks = breaks.spill, limits = c(0, 0.012)) +
  theme_minimal() +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(
    text = element_text(size = 21),
    legend.position = "none"
  )

cowplot::plot_grid(
  spill.map.house, spill.results.house,
  labels = "auto",
  label_size = 22
)

ggsave(
  "outputs/spillover_risk_index_site_level_house_traps.jpeg", 
  width = 4000, 
  height = 2000, 
  unit = "px"
)
