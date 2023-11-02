library(tidyverse)
library(rethinking)
library(rnaturalearth)
library(sf)

source("R/functions.R")

#==============================================================================


# Import combined visit-level datasets
visit.dat <- read_csv("data/clean/combined/visit_level_data.csv")
visit.dat.only.houses <- read_csv(
  "data/clean/combined/visit_level_data_only_houses.csv"
)

#==============================================================================


# Make a paneled plot of trapping success across Sierra Leone and Guinea, 
# using all visit-level data

breaks.all <- c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14)

ggplot(
  data = visit.dat,
  aes(x = Rra_per_trap, y = Mna_per_trap)) +
  geom_point(size = 6) +
  xlab(expression(paste(italic("Rattus rattus"), " catch per trap"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap"))) +
  scale_x_continuous(breaks = breaks.all, limits = c(0, 0.1)) +
  scale_y_continuous(breaks = breaks.all, limits = c(0, 0.16)) +
  theme_minimal() +
  theme(
    text = element_text(size = 20)
  ) +
  facet_wrap(~wet_season)


# Make a paneled plot of trapping success across Sierra Leone and Guinea, 
# using only visit-level house trapping data

breaks.house <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)

ggplot(
  data = visit.dat.only.houses,
  aes(x = Rra_per_trap, y = Mna_per_trap)) +
  geom_point(size = 6) +
  xlab(expression(paste(italic("Rattus rattus"), " catch per trap"))) +
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap"))) +
  scale_x_continuous(breaks = breaks.house, limits = c(0, 0.3)) +
  scale_y_continuous(breaks = breaks.house, limits = c(0, 0.3)) +
  theme_minimal() +
  theme(
    text = element_text(size = 20)
  ) +
  facet_wrap(~wet_season)

#==============================================================================


# Generate figure illustrating informative prior vs. uninformative prior

# Number of samples to generate from each distribution plus seed for
# reproducibility
n <- 1e6
set.seed(8)

# Generate samples from the informative prior and transform them into lambda
informative.prior <- rnorm(n, mean = -3.1, sd = 1.1)
informative.lambda <- exp(informative.prior)

# Generate samples from the uninformative prior and transform them into lambda
uninformative.prior <- rnorm(n, mean = 0, sd = 1)
uninformative.lambda <- exp(uninformative.prior)

# Package the samples together
d <- data.frame(
  value = c(
    informative.prior, uninformative.prior,
    informative.lambda, uninformative.lambda
  ),
  data_type = rep(
    c("raw", "lambda"),
    each = 2 * n
  ),
  prior_type = rep(
    c("Normal(-3.1, 1.1)", "Normal(0, 1)",
      "exp(Normal(-3.1, 1.1))", "exp(Normal(0, 1))"),
    each = n
  )
)

# Plot
palette <- wesanderson::wes_palette("Moonrise2") %>%
  adjustcolor(alpha.f = 0.5)
palette.line <- wesanderson::wes_palette("Moonrise2") %>%
  adjustcolor(alpha.f = 0.9)

panel.a <- d %>%
  filter(data_type == "raw") %>%
  ggplot(aes(x = value)) +
  geom_density(aes(fill = prior_type)) +
  geom_vline(
    xintercept = d %>%
      filter(data_type == "raw", prior_type == "Normal(-3.1, 1.1)") %>%
      pull(value) %>%
      median(),
    lty = 2,
    linewidth = 1.5,
    color = palette.line[1]
  ) +
  geom_vline(
    xintercept = d %>%
      filter(data_type == "raw", prior_type == "Normal(0, 1)") %>%
      pull(value) %>%
      median(),
    lty = 2,
    linewidth = 1.5,
    color = palette.line[2]
  ) +
  xlab("Raw parameter value") +
  ylab("Density") +
  xlim(-10, 10) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(
    text = element_text(size = 24),
    legend.title = element_blank(),
    legend.position = c(0.8, 0.85)
  )

panel.b <- d %>%
  filter(data_type == "lambda") %>%
  ggplot(aes(x = value)) +
  geom_density(aes(fill = prior_type)) +
  geom_vline(
    xintercept = d %>%
      filter(data_type == "lambda", prior_type == "exp(Normal(-3.1, 1.1))") %>%
      pull(value) %>%
      median(),
    lty = 2,
    linewidth = 1.5,
    color = palette.line[1]
  ) +
  geom_vline(
    xintercept = d %>%
      filter(data_type == "lambda", prior_type == "exp(Normal(0, 1))") %>%
      pull(value) %>%
      median(),
    lty = 2,
    linewidth = 1.5,
    color = palette.line[2]
  ) +
  xlab("Implied value of λ") +
  ylab("Density") +
  xlim(0, 2) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(
    text = element_text(size = 24),
    legend.title = element_blank(),
    legend.position = c(0.8, 0.85)
  )

cowplot::plot_grid(
  panel.a, panel.b,
  nrow = 2,
  labels = "auto",
  label_size = 22
)

ggsave("outputs/misc/prior_comparison.jpeg", 
       width = 3500, height = 4000, units = "px")

#==============================================================================


# Statistical analyses at visit level, all traps


# Package data for Stan models, with all traps included
stan.dat <- list(
  N = nrow(visit.dat),
  n_Mna = visit.dat$n_Mna,
  n_Rra = visit.dat$n_Rra,
  n_Rra_s = standardize(visit.dat$n_Rra),
  Rra_at_site = visit.dat$Rra_at_site,
  wet_season = visit.dat$wet_season,
  tot_traps = visit.dat$tot_traps,
  log_tot_traps = log(visit.dat$tot_traps),
  N_site = n_distinct(visit.dat$site),
  site = as.numeric(as.factor(visit.dat$site))
)

# Load model
visit.mod.Rra.at.site <- cmdstan_model("stan_models/visit_model_Rra_at_site.stan")

# Fit model
fit.m1 <- visit.mod.Rra.at.site$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m1$save_object("saved_models/visit_mod_all_traps_Rra_at_site.RDS")
fit.m1 <- readRDS("saved_models/visit_mod_all_traps_Rra_at_site.RDS")

fit.m1$diagnostic_summary()
fit.m1$print(max_rows = 100)

draws.m1 <- fit.m1$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site)

jpeg("outputs/misc/model_out_visit_level_all_traps_Rra_at_site.jpeg",
     width = 1000, height = 500, units = "px")

precis(draws.m1, prob = 0.99)
plot(precis(draws.m1, prob = 0.99), col = "gold2")
plot(precis(draws.m1, prob = 0.9), add = TRUE)

dev.off()


# Generate parameter trace plots

# Set palette for plotting
palette <- wesanderson::wes_palette("Darjeeling1") %>%
  adjustcolor(alpha.f = 0.3)

# Base figure
p <- bayesplot::mcmc_trace(
  fit.m1$draws(format = "matrix"), 
  pars = c("a", "bR", "bW", "sigma_site"),
  size = 0.8
) 

# Relabel strip text
levels(p$data$parameter) <- c(
  "grand mean", "*Rattus rattus* effect (present vs. absent)",
  "season effect (wet vs. dry)", "σ (for site-level random effects)"
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
  "outputs/misc/visit_level_all_traps_trace_plots.jpeg",
  width = 3500, height = 3000, units = "px"
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
  xlim(-4, 1) +
  ylim(0, 1) +
  scale_fill_manual(values = c(alpha("darkred", 0.3), "white")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "none"
  )

ggsave("outputs/misc/visit_level_all_traps_Rra_at_site_coefficient.jpeg",
       width = 2000, height = 1600, units = "px")


# Compute visit-level predictions from model output

preds <- visit.dat %>% 
  select(data_source, site, visit_mod, tot_traps) %>%
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

# Generate posterior predictive check plot
preds.summary <- preds %>%
  group_by(data_source, site, visit_mod) %>%
  summarize(
    lower90 = HPDI(pred, 0.9)[1],
    upper90 = HPDI(pred, 0.9)[2],
    lower99 = HPDI(pred, 0.99)[1],
    upper99 = HPDI(pred, 0.99)[2],
  ) %>%
  ungroup() %>%
  left_join(
    .,
    visit.dat %>%
      select(site, visit_mod, wet_season, n_Mna),
    by = c("site", "visit_mod")
  ) %>%
  mutate(
    in_interval = ifelse(n_Mna <= upper99 & n_Mna >= lower99, TRUE, FALSE)
  ) %>%
  arrange(data_source, site)

sum(preds.summary$in_interval)

ggplot() +
  geom_linerange(
    aes(x = 1:nrow(visit.dat), ymin = lower99, ymax = upper99),
    data = preds.summary, linewidth = 0.5
  ) +
  geom_linerange(
    aes(x = 1:nrow(visit.dat), ymin = lower90, ymax = upper90),
    data = preds.summary, linewidth = 1.5
  ) +
  geom_point(
    aes(x = 1:nrow(visit.dat), y = n_Mna, color = as.factor(Rra_at_site)), 
    data = visit.dat, size = 2
  ) +
  theme_minimal() +
  ylab(expression(paste("Number of ", italic("Mastomys natalensis"), " captured"))) +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    text = element_text(size = 20),
    legend.position = "none",
    axis.title.x = element_blank()
  )

ggsave("outputs/misc/predictive_check_visit_level_all_traps_Rra_at_site.jpeg", 
       width = 3000, height = 2000, unit = "px")

#==============================================================================


# Statistical analyses at visit level, only house traps

# Package data for Stan models, with only houses included
stan.dat <- list(
  N = nrow(visit.dat.only.houses),
  n_Mna = visit.dat.only.houses$n_Mna,
  n_Rra = visit.dat.only.houses$n_Rra,
  n_Rra_s = standardize(visit.dat.only.houses$n_Rra),
  Rra_at_site = visit.dat.only.houses$Rra_at_site,
  wet_season = visit.dat.only.houses$wet_season,
  tot_traps = visit.dat.only.houses$tot_traps,
  log_tot_traps = log(visit.dat.only.houses$tot_traps),
  N_site = n_distinct(visit.dat.only.houses$site),
  site = as.numeric(as.factor(visit.dat.only.houses$site))
)

# Fit model
fit.m2 <- visit.mod.Rra.at.site$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m2$save_object("saved_models/visit_mod_house_traps_Rra_at_site.RDS")
fit.m2 <- readRDS("saved_models/visit_mod_house_traps_Rra_at_site.RDS")

fit.m2$diagnostic_summary()
fit.m2$print(max_rows = 100)

draws.m2 <- fit.m2$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site)

jpeg("outputs/misc/model_out_visit_level_house_traps_Rra_at_site.jpeg",
     width = 1000, height = 500, units = "px")

precis(draws.m2, prob = 0.99)
plot(precis(draws.m2, prob = 0.99), col = "gold2")
plot(precis(draws.m2, prob = 0.9), add = TRUE)

dev.off()


# Generate parameter trace plots

# Set palette for plotting
palette <- wesanderson::wes_palette("Darjeeling1") %>%
  adjustcolor(alpha.f = 0.3)

# Base figure
p <- bayesplot::mcmc_trace(
  fit.m2$draws(format = "matrix"), 
  pars = c("a", "bR", "bW", "sigma_site"),
  size = 0.8
) 

# Relabel strip text
levels(p$data$parameter) <- c(
  "grand mean", "*Rattus rattus* effect (present vs. absent)",
  "season effect (wet vs. dry)", "σ (for site-level random effects)"
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
  "outputs/misc/visit_level_house_traps_trace_plots.jpeg",
  width = 3500, height = 3000, units = "px"
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
  xlim(-5, 1) +
  ylim(0, 1) +
  scale_fill_manual(values = c(alpha("darkred", 0.3), "white")) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "none"
  )

ggsave("outputs/misc/visit_level_house_traps_Rra_at_site_coefficient.jpeg",
       width = 2000, height = 1600, units = "px")


# Compute visit-level predictions from model output

preds <- visit.dat.only.houses %>% 
  select(data_source, site, visit_mod, tot_traps) %>%
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
  group_by(data_source, site, visit_mod) %>%
  summarize(
    lower90 = HPDI(pred, 0.9)[1],
    upper90 = HPDI(pred, 0.9)[2],
    lower99 = HPDI(pred, 0.99)[1],
    upper99 = HPDI(pred, 0.99)[2],
  ) %>%
  ungroup() %>%
  left_join(
    .,
    visit.dat.only.houses %>%
      select(site, visit_mod, wet_season, n_Mna),
    by = c("site", "visit_mod")
  ) %>%
  mutate(
    in_interval = ifelse(n_Mna <= upper99 & n_Mna >= lower99, TRUE, FALSE)
  ) %>%
  arrange(data_source, site)

sum(preds.summary$in_interval)

ggplot() +
  geom_linerange(
    aes(x = 1:nrow(visit.dat.only.houses), ymin = lower99, ymax = upper99),
    data = preds.summary, linewidth = 0.5
  ) +
  geom_linerange(
    aes(x = 1:nrow(visit.dat.only.houses), ymin = lower90, ymax = upper90),
    data = preds.summary, linewidth = 1.5
  ) +
  geom_point(
    aes(x = 1:nrow(visit.dat.only.houses), y = n_Mna, color = as.factor(Rra_at_site)), 
    data = visit.dat.only.houses, size = 2
  ) +
  theme_minimal() +
  ylab(expression(paste("Number of ", italic("Mastomys natalensis"), " captured"))) +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    text = element_text(size = 20),
    legend.position = "none",
    axis.title.x = element_blank()
  )

ggsave("outputs/misc/predictive_check_visit_level_house_traps_Rra_at_site.jpeg", 
       width = 3000, height = 2000, unit = "px")


#==============================================================================


# Make a paneled plot of trapping success across Sierra Leone and Guinea, 
# using all site-level data

site.dat <- read_csv("data/clean/combined/site_level_data.csv")
site.dat.only.houses <- read_csv(
  "data/clean/combined/site_level_data_only_houses.csv"
)

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

#==============================================================================


# Plot implications of the two models with Rra_at_site predictors

# Generate predictions for the all traps model, for all combinations of 
# wet/dry seasons, Rattus rattus present/absent
preds.all <- data.frame(
  season = rep(
    c("Wet season", "Dry season"), 
    each = length(draws.m1$a)*2
  ),
  site_status = rep(
    c("Absent", "Present", 
      "Absent", "Present"),
    each = length(draws.m1$a)
  ),
  catch_per_trap = c(
    exp(draws.m1$a + draws.m1$bW),
    exp(draws.m1$a + draws.m1$bW + draws.m1$bR),
    exp(draws.m1$a),
    exp(draws.m1$a + draws.m1$bR)
  )
)

# Generate predictions for the house traps model, for all combinations of 
# wet/dry seasons, Rattus rattus present/absent
preds.house <- data.frame(
  season = rep(
    c("Wet season", "Dry season"), 
    each = length(draws.m2$a)*2
  ),
  site_status = rep(
    c("Absent", "Present", 
      "Absent", "Present"),
    each = length(draws.m2$a)
  ),
  catch_per_trap = c(
    exp(draws.m2$a + draws.m2$bW),
    exp(draws.m2$a + draws.m2$bW + draws.m2$bR),
    exp(draws.m2$a),
    exp(draws.m2$a + draws.m2$bR)
  )
)

# Table giving observed catch per trap in both datasets (i.e., complete pooling 
# estimates): this is what you'd get in a model without varying effects
visit.dat %>% 
  group_by(Rra_at_site) %>% 
  summarize(
    n_Mna = sum(n_Mna),
    tot_traps = sum(tot_traps),
    catch_per_trap = n_Mna/tot_traps
  )

visit.dat.only.houses %>% 
  group_by(Rra_at_site) %>% 
  summarize(
    n_Mna = sum(n_Mna),
    tot_traps = sum(tot_traps),
    catch_per_trap = n_Mna/tot_traps
  )


# Integrate information with other panels to make alternative plots

val.dodge <- 0.25

d.all <- preds.all %>%
  group_by(season, site_status) %>%
  summarize(
    mean = mean(catch_per_trap),
    lower99 = HPDI(catch_per_trap, 0.99)[1],
    upper99 = HPDI(catch_per_trap, 0.99)[2],
    lower90 = HPDI(catch_per_trap, 0.9)[1],
    upper90 = HPDI(catch_per_trap, 0.9)[2]
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
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap (average site)"))) +
  scale_y_continuous(breaks = breaks.all, limits = c(0, 0.05)) +
  theme_minimal() +
  scale_color_manual(values = c("wheat3", "steelblue")) +
  theme(
    text = element_text(size = 21),
    legend.position = "none"
    # legend.title = element_blank(),
    # legend.position = c(0.75, 0.80),
    # legend.background = element_rect(fill = "white", colour = 0)
  )

cowplot::plot_grid(
  a.all, b.all, c.all, d.all,
  nrow = 2,
  rel_heights = c(0.55, 0.45),
  labels = "auto",
  label_size = 22
)

ggsave(
  "outputs/visits_all_traps.jpeg", 
  width = 4000, 
  height = 3500, 
  unit = "px"
)

d.house <- preds.house %>%
  group_by(season, site_status) %>%
  summarize(
    mean = mean(catch_per_trap),
    lower99 = HPDI(catch_per_trap, 0.99)[1],
    upper99 = HPDI(catch_per_trap, 0.99)[2],
    lower90 = HPDI(catch_per_trap, 0.9)[1],
    upper90 = HPDI(catch_per_trap, 0.9)[2]
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
  ylab(expression(atop(italic("Mastomys natalensis"), "catch per trap (average site)"))) +
  scale_y_continuous(breaks = breaks.house, limits = c(0, 0.1)) +
  theme_minimal() +
  scale_color_manual(values = c("wheat3", "steelblue")) +
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
  "outputs/visits_house_traps.jpeg", 
  width = 4000, 
  height = 3500, 
  unit = "px"
)

#==============================================================================


# Spillover risk analyses

# What is the range of catch per trap of Lassa-positive Mastomys natalensis?
summary(visit.dat.only.houses$n_Mna_pos_lassa/visit.dat.only.houses$tot_traps)

# Package data for Stan models, with only houses included
stan.dat <- list(
  N = nrow(visit.dat.only.houses),
  n_Mna = visit.dat.only.houses$n_Mna,
  n_Mna_pos_lassa = visit.dat.only.houses$n_Mna_pos_lassa,
  n_Rra = visit.dat.only.houses$n_Rra,
  n_Rra_s = standardize(visit.dat.only.houses$n_Rra),
  Rra_at_site = visit.dat.only.houses$Rra_at_site,
  wet_season = visit.dat.only.houses$wet_season,
  tot_traps = visit.dat.only.houses$tot_traps,
  log_tot_traps = log(visit.dat.only.houses$tot_traps),
  N_site = n_distinct(visit.dat.only.houses$site),
  site = as.numeric(as.factor(visit.dat.only.houses$site))
)

# Load model
spill.mod <- cmdstan_model("stan_models/spillover_risk_visit_level.stan")

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
fit.spill$save_object("saved_models/spillover_risk_visit_level.RDS")
fit.spill <- readRDS("saved_models/spillover_risk_visit_level.RDS")

fit.spill$diagnostic_summary()
fit.spill$print(max_rows = 100)

draws.spill <- fit.spill$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bR, bW, sigma_site)

jpeg("outputs/misc/model_out_spillover_risk_visit_level.jpeg",
     width = 1000, height = 500, units = "px")

precis(draws.spill, prob = 0.99)
plot(precis(draws.spill, prob = 0.99), col = "gold2")
plot(precis(draws.spill, prob = 0.9), add = TRUE)

dev.off()


# Generate parameter trace plots

# Set palette for plotting
palette <- wesanderson::wes_palette("Darjeeling1") %>%
  adjustcolor(alpha.f = 0.3)

# Base figure
p <- bayesplot::mcmc_trace(
  fit.spill$draws(format = "matrix"), 
  pars = c("a", "bR", "bW", "sigma_site"),
  size = 0.8
) 

# Relabel strip text
levels(p$data$parameter) <- c(
  "grand mean", "*Rattus rattus* effect (present vs. absent)",
  "season effect (wet vs. dry)", "σ (for site-level random effects)"
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
  "outputs/misc/spillover_risk_model_visit_level_house_traps_trace_plots.jpeg",
  width = 3500, height = 3000, units = "px"
)


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

ggsave("outputs/misc/spillover_risk_model_visit_level_Rra_at_site_coefficient.jpeg",
       width = 2000, height = 1600, units = "px")


# Compute visit-level predictions from model output

preds <- visit.dat.only.houses %>% 
  select(data_source, site, visit_mod, tot_traps) %>%
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
  group_by(data_source, site, visit_mod) %>%
  summarize(
    lower90 = HPDI(pred, 0.9)[1],
    upper90 = HPDI(pred, 0.9)[2],
    lower99 = HPDI(pred, 0.99)[1],
    upper99 = HPDI(pred, 0.99)[2],
  ) %>%
  ungroup() %>%
  left_join(
    .,
    visit.dat.only.houses %>%
      select(site, visit_mod, wet_season, n_Mna_pos_lassa),
    by = c("site", "visit_mod")
  ) %>%
  mutate(
    in_interval = ifelse(n_Mna_pos_lassa <= upper99 & n_Mna_pos_lassa >= lower99, TRUE, FALSE)
  )

sum(preds.summary$in_interval)

ggplot() +
  geom_linerange(
    aes(x = 1:nrow(visit.dat.only.houses), ymin = lower99, ymax = upper99),
    data = preds.summary, linewidth = 0.5
  ) +
  geom_linerange(
    aes(x = 1:nrow(visit.dat.only.houses), ymin = lower90, ymax = upper90),
    data = preds.summary, linewidth = 1.5
  ) +
  geom_point(
    aes(x = 1:nrow(visit.dat.only.houses), y = n_Mna_pos_lassa, color = as.factor(Rra_at_site)), 
    data = visit.dat.only.houses, size = 2
  ) +
  theme_minimal() +
  ylab(expression(paste("Number of ", italic("Mastomys natalensis"), " captured"))) +
  ylim(0, 15) +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    text = element_text(size = 20),
    legend.position = "none",
    axis.title.x = element_blank()
  )

ggsave("outputs/misc/predictive_check_spillover_risk_visit_level.jpeg", 
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
  season = rep(
    c("Wet season", "Dry season"), 
    each = length(draws.spill$a)*2
  ),
  site_status = rep(
    c("Absent", "Present", 
      "Absent", "Present"),
    each = length(draws.spill$a)
  ),
  catch_per_trap = c(
    exp(draws.spill$a + draws.spill$bW),
    exp(draws.spill$a + draws.spill$bW + draws.spill$bR),
    exp(draws.spill$a),
    exp(draws.spill$a + draws.spill$bR)
  )
)

spill.results.house <- preds %>%
  group_by(season, site_status) %>%
  summarize(
    mean = mean(catch_per_trap),
    lower99 = HPDI(catch_per_trap, 0.99)[1],
    upper99 = HPDI(catch_per_trap, 0.99)[2],
    lower90 = HPDI(catch_per_trap, 0.9)[1],
    upper90 = HPDI(catch_per_trap, 0.9)[2]
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
  ylab(expression(atop(paste("Lassa-positive ", italic("Mastomys natalensis")), "catch per trap (average site)"))) +
  scale_y_continuous(breaks = breaks.spill, limits = c(0, 0.016)) +
  theme_minimal() +
  scale_color_manual(values = c("wheat3", "steelblue")) +
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
  "outputs/spillover_risk_index_visit_level_house_traps.jpeg", 
  width = 4000, 
  height = 2000, 
  unit = "px"
)

#==============================================================================


# Statistical analyses of other rodent species at visit level, only house traps

# Package data for Stan models, with only houses included
stan.dat <- list(
  N = nrow(visit.dat.only.houses),
  n_Mna = visit.dat.only.houses$n_Mna,
  n_Rra = visit.dat.only.houses$n_Rra,
  n_Rra_s = standardize(visit.dat.only.houses$n_Rra),
  Rra_at_site = visit.dat.only.houses$Rra_at_site,
  Mer_at_site = visit.dat.only.houses$Mer_at_site,
  Mma_at_site = visit.dat.only.houses$Mma_at_site,
  Pda_at_site = visit.dat.only.houses$Pda_at_site,
  Pro_at_site = visit.dat.only.houses$Pro_at_site,
  wet_season = visit.dat.only.houses$wet_season,
  tot_traps = visit.dat.only.houses$tot_traps,
  log_tot_traps = log(visit.dat.only.houses$tot_traps),
  N_site = n_distinct(visit.dat.only.houses$site),
  site = as.numeric(as.factor(visit.dat.only.houses$site))
)

# Load Mer model
Mer.mod <- cmdstan_model("stan_models/visit_model_Mer_at_site.stan")

# Fit model
fit.m3 <- Mer.mod$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m3$save_object("saved_models/visit_mod_house_traps_Mer_at_site.RDS")
fit.m3 <- readRDS("saved_models/visit_mod_house_traps_Mer_at_site.RDS")

fit.m3$diagnostic_summary()
fit.m3$print(max_rows = 100)

draws.m3 <- fit.m3$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bM, bW, sigma_site)

precis(draws.m3, prob = 0.9)
quantile(draws.m3$bM, c(0.05, 0.95))

# Load Mma model
Mma.mod <- cmdstan_model("stan_models/visit_model_Mma_at_site.stan")

# Fit model
fit.m4 <- Mma.mod$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m4$save_object("saved_models/visit_mod_house_traps_Mma_at_site.RDS")
fit.m4 <- readRDS("saved_models/visit_mod_house_traps_Mma_at_site.RDS")

fit.m4$diagnostic_summary()
fit.m4$print(max_rows = 100)

draws.m4 <- fit.m4$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bM, bW, sigma_site)

precis(draws.m4, prob = 0.9)
quantile(draws.m4$bM, c(0.05, 0.95))

# Load Pda model
Pda.mod <- cmdstan_model("stan_models/visit_model_Pda_at_site.stan")

# Fit model
fit.m5 <- Pda.mod$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m5$save_object("saved_models/visit_mod_house_traps_Pda_at_site.RDS")
fit.m5 <- readRDS("saved_models/visit_mod_house_traps_Pda_at_site.RDS")

fit.m5$diagnostic_summary()
fit.m5$print(max_rows = 100)

draws.m5 <- fit.m5$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bP, bW, sigma_site)

precis(draws.m5, prob = 0.9)
quantile(draws.m5$bP, c(0.05, 0.95))

# Load Pro model
Pro.mod <- cmdstan_model("stan_models/visit_model_Pro_at_site.stan")

# Fit model
fit.m6 <- Pro.mod$sample(
  data = stan.dat, 
  chains = 4, 
  parallel_chains = 4,
  iter_warmup = 2500,
  iter_sampling = 5000,
  adapt_delta = 0.99,
  seed = 8
)

# Save/load fit model object
fit.m6$save_object("saved_models/visit_mod_house_traps_Pro_at_site.RDS")
fit.m6 <- readRDS("saved_models/visit_mod_house_traps_Pro_at_site.RDS")

fit.m6$diagnostic_summary()
fit.m6$print(max_rows = 100)

draws.m6 <- fit.m6$draws(format = "matrix") %>%
  data.frame() %>%
  select(a, bP, bW, sigma_site)

precis(draws.m6, prob = 0.9)
quantile(draws.m6$bP, c(0.05, 0.95))


# Generate a figure of all rodent presence/absence effect posteriors

# Package data from all species-specific models
dat.ridges <- data.frame(
  species = rep(
    c("Rattus rattus", "Mastomys erythroleucus", "Mus mattheyi", 
      "Praomys daltoni", "Praomys rostratus"),
    each = length(draws.m2$bR)
  ),
  value = c(
    draws.m2$bR,
    draws.m3$bM,
    draws.m4$bM,
    draws.m5$bP,
    draws.m6$bP
  )
) %>%
  arrange(species)

# Plot
palette <- wesanderson::wes_palette("Darjeeling2")

dat.ridges %>%
  mutate(species = forcats::fct_rev(species)) %>%
  ggplot(aes(x = value, y = species, fill = rev(species))) +
  geom_vline(xintercept = 0, linewidth = 2, lty = 2) +
  ggridges::stat_density_ridges(
    quantile_lines = TRUE, 
    quantiles = c(0.05, 0.95), 
    alpha = 0.6
  ) +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  xlab("Parameter Value") +
  ylab("") +
  theme(
    text = element_text(size = 21),
    axis.text.y = element_text(face = "italic"),
    legend.position = "none"
  )

ggsave(
  "outputs/misc/species_presence_effect_posteriors.jpeg", 
  width = 3000, 
  height = 2000, 
  unit = "px"
)
