library(tidyverse)
library(rnaturalearth)
library(sf)

source("R/functions.R")

#==============================================================================


# Import visit-level data so we can pull the location and timing of each site
# visit
visit.dat <- read_csv("data/clean/combined/visit_level_data.csv")

sl <- ne_countries(
  country = c("Guinea", "Sierra Leone"),
  scale = "medium",
  returnclass = "sf"
)

d <- visit.dat %>%
  select(site, visit_mod, date_mod, wet_season, latitude, longitude) %>%
  mutate(
    month = formatC(month(date_mod), width = 2, format = "d", flag = "0"),
    year = year(date_mod)
  ) %>%
  select(site, visit_mod, date_mod, year, month, wet_season, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(sl))


# Import precipitation raster files
files <- list.files(
  path = "data/environmental/precipitation",
  full.names = TRUE
)
r <- terra::rast(files)

# Initiate and fill precipitation variable
d$precipitation <- rep(NA)

for(i in 1:nrow(d)) {
  
  # Get relevant data from the site visit
  row <- d[i, ]
  year <- row$year
  month <- row$month
  
  # Subset to the correct precipitation raster layer
  layer <- r[[paste0("chirps-v2.0.", year, ".", month)]]
  
  # Extract values
  d[i, "precipitation"] <- terra::extract(layer, st_coordinates(row))
}


# Import temperature raster files
files <- list.files(
  path = "data/environmental/temperature",
  full.names = TRUE
)

# Initiate and fill temperature variable
d$temperature <- rep(NA)

for(i in 1:nrow(d)) {
  
  # Get relevant data from the site visit
  row <- d[i, ]
  year <- row$year
  month <- row$month
  
  # Subset to the correct temperature raster layer
  files.for.year <- grep(files, pattern = paste0("A", year), value = TRUE)
  assertthat::assert_that(length(files.for.year) == 12)
  file.for.month <- files.for.year[as.numeric(row$month)]
  layer <- terra::rast(file.for.month, lyrs = "LST_Day_CMG")
  
  # Extract values
  d[i, "temperature"] <- terra::extract(layer, st_coordinates(row))
}

# Convert temperatures to Celsius
d$temperature <- d$temperature - 273.15


# t-tests for differences between wet and dry season
t.test(precipitation ~ wet_season, data = d, var.equal = TRUE)
t.test(temperature ~ wet_season, data = d, var.equal = TRUE)


# Plot
set.seed(4)

p <- ggplot() +
  ggbeeswarm::geom_quasirandom(
    data = d, 
    aes(x = as.factor(wet_season), y = precipitation, color = as.factor(wet_season)),
    size = 4
  ) +
  xlab("Season") +
  ylab("Total Monthly Precipitation (mm)") +
  ylim(0, 800) +
  scale_x_discrete(labels = c("Dry", "Wet")) +
  scale_color_manual(values = c("wheat3", "steelblue")) +
  theme_minimal() +
  theme(
    text = element_text(size = 22),
    legend.position = "none"
  )

t <- ggplot() +
  ggbeeswarm::geom_quasirandom(
    data = d, 
    aes(x = as.factor(wet_season), y = temperature, color = as.factor(wet_season)),
    size = 4
  ) +
  xlab("Season") +
  ylab("Monthly Average Temperature (°C)") +
  ylim(0, 40) +
  scale_x_discrete(labels = c("Dry", "Wet")) +
  scale_color_manual(values = c("wheat3", "steelblue")) +
  theme_minimal() +
  theme(
    text = element_text(size = 22),
    legend.position = "none"
  )

cowplot::plot_grid(
  p, t, 
  nrow = 1,
  labels = "auto",
  label_size = 22
)

ggsave("outputs/misc/environmental_data_by_season.jpeg",
       width = 4000, height = 2000, units = "px")
