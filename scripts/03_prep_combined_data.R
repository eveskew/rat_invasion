library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# The goal of this script is to combine rodent trapping data from Sierra Leone
# and Guinea, saving intermediate data files to "data/clean/combined"

#==============================================================================


# Import cleaned trapping and capture data files from Sierra Leone (the 
# PREEMPT project)

# Data on track-level trapping effort across all sites
summary.trap.dat <- read_csv("data/clean/PREEMPT/trap_metadata.csv") %>%
  select(-mastomys, -house_type)
# Data on rodent captures across all sites
capture.dat <- read_csv("data/clean/PREEMPT/PREEMPT_capture_data.csv")
# Summarized data on rodent capture at all sites: needed for site-level
# lat/long information
agg <- read_csv("data/clean/PREEMPT/aggregated_PREEMPT_capture_data.csv")

#==============================================================================


# Generate track-level data tables for Sierra Leone

# Summarize capture data by site, visit, and track ID
capture.dat.by.track <- capture.dat %>%
  group_by(site, visit, track_id) %>%
  summarize(
    n_catch = n(),
    n_Mna = sum(sp == "Mna"),
    n_Rra = sum(sp == "Rra")
  ) %>%
  ungroup()

sum(capture.dat.by.track$n_Mna)
sum(capture.dat.by.track$n_Rra)

# Join summarized capture data in with track-level trap data
track.level.captures <- summary.trap.dat %>%
  left_join(
    ., capture.dat.by.track,
    by = c("site", "visit", "track_id")
  ) %>%
  mutate(
    n_Mna = ifelse(is.na(n_Mna), 0, n_Mna),
    n_Rra = ifelse(is.na(n_Rra), 0, n_Rra)
  )

sum(track.level.captures$n_Mna)
sum(track.level.captures$n_Rra)

#==============================================================================


# Generate site-level data tables for Sierra Leone

# Generate site-level capture data frame from PREEMPT data
sl.site.dat <- track.level.captures %>%
  group_by(site) %>%
  summarize(
    tot_traps = sum(tot_traps),
    n_catch = sum(n_catch, na.rm = T),
    n_Mna = sum(n_Mna),
    n_Rra = sum(n_Rra),
    Mna_per_trap = n_Mna/tot_traps,
    Rra_per_trap = n_Rra/tot_traps
  ) %>%
  ungroup() %>%
  # add in site lat/longs
  left_join(
    .,
    agg %>%
      distinct(site, latitude, longitude),
    by = "site"
  ) %>%
  # add in Rra_at_site information
  mutate(Rra_at_site = ifelse(n_Rra > 0, 1, 0)) %>%
  # tag as PREEMPT data
  mutate(data_source = rep("PREEMPT", nrow(.)))

# Add in Mastomys natalensis Lassa information
sl.site.dat <- sl.site.dat %>%
  left_join(
    .,
    capture.dat %>%
      filter(species == "Mastomys natalensis") %>%
      group_by(site) %>%
      summarize(
        n_Mna_neg_lassa = sum(lassa == 0),
        n_Mna_pos_lassa = sum(lassa == 1)
      ) %>%
      ungroup(),
    by = "site"
  ) %>%
  mutate(
    n_Mna_neg_lassa = ifelse(is.na(n_Mna_neg_lassa), 0, n_Mna_neg_lassa),
    n_Mna_pos_lassa = ifelse(is.na(n_Mna_pos_lassa), 0, n_Mna_pos_lassa),
    n_Mna_tested_lassa = n_Mna_neg_lassa + n_Mna_pos_lassa
  )

nrow(sl.site.dat)

# And the same thing with only houses included
sl.site.dat.only.houses <- track.level.captures %>%
  filter(habitat_code == "H") %>%
  group_by(site) %>%
  summarize(
    tot_traps = sum(tot_traps),
    n_catch = sum(n_catch, na.rm = T),
    n_Mna = sum(n_Mna),
    n_Rra = sum(n_Rra),
    Mna_per_trap = n_Mna/tot_traps,
    Rra_per_trap = n_Rra/tot_traps
  ) %>%
  ungroup() %>%
  # add in site lat/longs
  left_join(
    .,
    agg %>%
      distinct(site, latitude, longitude),
    by = "site"
  ) %>%
  # add in Rra_at_site information
  left_join(
    .,
    sl.site.dat %>%
      select(site, Rra_at_site),
    by = "site"
  ) %>%
  # tag as PREEMPT data
  mutate(data_source = rep("PREEMPT", nrow(.)))

# Add in Mastomys natalensis Lassa information
sl.site.dat.only.houses <- sl.site.dat.only.houses %>%
  left_join(
    .,
    capture.dat %>%
      filter(species == "Mastomys natalensis") %>%
      filter(habitat_code == "H") %>%
      group_by(site) %>%
      summarize(
        n_Mna_neg_lassa = sum(lassa == 0),
        n_Mna_pos_lassa = sum(lassa == 1)
      ) %>%
      ungroup(),
    by = "site"
  ) %>%
  mutate(
    n_Mna_neg_lassa = ifelse(is.na(n_Mna_neg_lassa), 0, n_Mna_neg_lassa),
    n_Mna_pos_lassa = ifelse(is.na(n_Mna_pos_lassa), 0, n_Mna_pos_lassa),
    n_Mna_tested_lassa = n_Mna_neg_lassa + n_Mna_pos_lassa
  )

nrow(sl.site.dat.only.houses)

#==============================================================================


# Generate site-level data tables for Guinea

EFC.agg <- read_csv("data/clean/EFC/Aggregated_EFC_Capture_Data.csv")

guinea.site.dat <- EFC.agg %>%
  group_by(Site, Longitude, Latitude, Sp) %>%
  summarize(
    tot_traps = sum(TotTraps),
    Tot = sum(Tot)
  ) %>%
  ungroup() %>%
  filter(Sp %in% c("Mna", "Rra")) %>%
  pivot_wider(
    names_from = Sp,
    values_from = Tot
  ) %>%
  rename(
    site = Site,
    longitude = Longitude,
    latitude = Latitude,
    n_Mna = Mna,
    n_Rra = Rra
  ) %>%
  mutate(
    Mna_per_trap = n_Mna/tot_traps,
    Rra_per_trap = n_Rra/tot_traps,
    Rra_at_site = ifelse(n_Rra > 0, 1, 0),
    n_catch = EFC.agg %>%
      group_by(Site) %>%
      summarize(n_catch = sum(Tot)) %>%
      ungroup() %>%
      pull(n_catch)
  ) %>%
  # tag as EFC data
  mutate(data_source = rep("EFC", nrow(.)))

# Add in Mastomys natalensis Lassa information
guinea.site.dat <- guinea.site.dat %>%
  left_join(
    .,
    EFC.agg %>%
      rename(site = Site) %>%
      filter(Species_ID == "Mastomys natalensis") %>%
      group_by(site) %>%
      summarize(
        n_Mna_tested_lassa = sum(NumTestLassa),
        n_Mna_pos_lassa = sum(NumPosLassa),
        n_Mna_neg_lassa = n_Mna_tested_lassa - n_Mna_pos_lassa
      ) %>%
      ungroup(),
    by = "site"
  )

nrow(guinea.site.dat)

# And the same thing with only houses included
guinea.site.dat.only.houses <- EFC.agg %>%
  filter(Code.Habitat == "H") %>%
  group_by(Site, Longitude, Latitude, Sp) %>%
  summarize(
    tot_traps = sum(TotTraps),
    Tot = sum(Tot)
  ) %>%
  ungroup() %>%
  filter(Sp %in% c("Mna", "Rra")) %>%
  pivot_wider(
    names_from = Sp,
    values_from = Tot
  ) %>%
  rename(
    site = Site,
    longitude = Longitude,
    latitude = Latitude,
    n_Mna = Mna,
    n_Rra = Rra
  ) %>%
  mutate(
    Mna_per_trap = n_Mna/tot_traps,
    Rra_per_trap = n_Rra/tot_traps,
    n_catch = EFC.agg %>%
      filter(Code.Habitat == "H") %>%
      group_by(Site) %>%
      summarize(n_catch = sum(Tot)) %>%
      ungroup() %>%
      pull(n_catch)
  ) %>%
  left_join(
    .,
    guinea.site.dat %>%
      select(site, Rra_at_site),
    by = "site"
  ) %>%
  # tag as EFC data
  mutate(data_source = rep("EFC", nrow(.)))

# Add in Mastomys natalensis Lassa information
guinea.site.dat.only.houses <- guinea.site.dat.only.houses %>%
  left_join(
    .,
    EFC.agg %>%
      rename(site = Site) %>%
      filter(Species_ID == "Mastomys natalensis") %>%
      filter(Code.Habitat == "H") %>%
      group_by(site) %>%
      summarize(
        n_Mna_tested_lassa = sum(NumTestLassa),
        n_Mna_pos_lassa = sum(NumPosLassa),
        n_Mna_neg_lassa = n_Mna_tested_lassa - n_Mna_pos_lassa
      ) %>%
      ungroup(),
    by = "site"
  )

nrow(guinea.site.dat.only.houses)

#==============================================================================


# Combine site-level trapping data from Sierra Leone and Guinea

site.dat <- 
  bind_rows(sl.site.dat, guinea.site.dat)

site.dat.only.houses <- 
  bind_rows(sl.site.dat.only.houses, guinea.site.dat.only.houses)

# Plot observed catch per trap for both species by site
site.dat %>%
  rename(
    Mna = Mna_per_trap,
    Rra = Rra_per_trap
  ) %>%
  pivot_longer(
    cols = c("Mna", "Rra"),
    names_to = "species",
    values_to = "catch_per_trap"
  ) %>%
  ggplot(aes(x = species, y = catch_per_trap, fill = species)) +
  geom_col() +
  xlab("Rodent species") +
  ylab("Catch per trap") +
  facet_wrap(~site) +
  theme_minimal()

#==============================================================================


# Get distance to the coastline, Freetown, and Conakry for all study sites
# https://stackoverflow.com/questions/51837454/r-measuring-distance-from-a-coastline

# Import country backgrounds and covert data to spatial points
sl <- ne_countries(scale = "medium", country = c("Guinea", "Sierra Leone")) %>%
  st_as_sf()
site.points <- site.dat %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(sl))

# Generate bounding box around the region and grab coastline info
osm.box <- osmdata::getbb(place_name = "Sierra Leone")
osm.box[1,1] <- -16 # xmin
osm.box[1,2] <- -6 # xmax
osm.box[2,1] <- 4 # ymin
osm.box[2,2] <- 12 # ymax

osm.box <- osm.box %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature("natural", "coastline") %>%
  osmdata::osmdata_sf()

# Calculate distance between all points and the coastline
dist <- geosphere::dist2Line(
  p = st_coordinates(site.points),
  line = as_Spatial(osm.box$osm_lines)
) %>%
  data.frame()

# Add this info back into the "site.dat" data
site.dat <- cbind(site.dat, dist) %>%
  rename(
    distance_to_coast = distance,
    lon_coast_intersect = lon,
    lat_coast_intersect = lat
  ) %>%
  select(-ID)

# Calculate distance between all points and Freetown
dist <- st_distance(
  x = site.points,
  y = st_sfc(
    st_point(x = c(-13.2317, 8.4657), dim = "XY"), 
    crs = st_crs(site.points)
  )
)

# Add this info back into the "site.dat" data
site.dat <- cbind(site.dat, distance_to_Freetown = as.vector(dist))

# Calculate distance between all points and Conakry
dist <- st_distance(
  x = site.points,
  y = st_sfc(
    st_point(x = c(-13.5784, 9.6412), dim = "XY"), 
    crs = st_crs(site.points)
  )
)

# Add this info back into the "site.dat" data
site.dat <- cbind(site.dat, distance_to_Conakry = as.vector(dist))


# Add all distance info from the "site.dat" data frame to the house-only data
# frame
site.dat.only.houses <- site.dat.only.houses %>%
  left_join(
    .,
    site.dat %>%
      select(
        site, 
        distance_to_coast, lon_coast_intersect, lat_coast_intersect,
        distance_to_Freetown, distance_to_Conakry),
    by = "site"
  )

dim(site.dat)
dim(site.dat.only.houses)

#==============================================================================


# Plot distance calculations

ggplot() +
  geom_sf(data = osm.box$osm_lines) +
  geom_sf(data = site.points, aes(color = data_source)) +
  geom_segment(data = site.dat, 
               aes(x = longitude, y = latitude, 
                   xend = lon_coast_intersect, yend = lat_coast_intersect)) +
  theme_minimal()

ggplot() +
  geom_sf(data = osm.box$osm_lines) +
  geom_sf(data = site.points, aes(color = data_source)) +
  geom_segment(data = site.dat, 
               aes(x = longitude, y = latitude, xend = -13.2317, yend = 8.4657)) +
  theme_minimal()

ggplot() +
  geom_sf(data = osm.box$osm_lines) +
  geom_sf(data = site.points, aes(color = data_source)) +
  geom_segment(data = site.dat, 
               aes(x = longitude, y = latitude, xend = -13.5784, yend = 9.6412)) +
  theme_minimal()

# Plot distance relationships
site.dat %>%
  ggplot(aes(x = distance_to_coast/1000, y = Mna_per_trap)) +
  geom_point(aes(color = data_source)) +
  xlab("Distance from coast (km)") +
  ylab("Catch per trap") +
  theme_minimal()

site.dat %>%
  ggplot(aes(x = distance_to_Freetown/1000, y = Mna_per_trap)) +
  geom_point(aes(color = data_source)) +
  xlab("Distance from Freetown (km)") +
  ylab("Catch per trap") +
  theme_minimal()

site.dat %>%
  ggplot(aes(x = distance_to_Conakry/1000, y = Mna_per_trap)) +
  geom_point(aes(color = data_source)) +
  xlab("Distance from Conakry (km)") +
  ylab("Catch per trap") +
  theme_minimal()

#==============================================================================


# Save site-level trapping data from Sierra Leone and Guinea

write_csv(site.dat, "data/clean/combined/site_level_data.csv")
write_csv(site.dat.only.houses, "data/clean/combined/site_level_data_only_houses.csv")
