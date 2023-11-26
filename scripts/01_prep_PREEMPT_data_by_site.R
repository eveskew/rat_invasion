## This script will load in the raw PREEMPT datasets and merge these
## datasets together into an aggregated data frame called
## "all.agg.trap.capture". To accomplish this, we load in a few different 
## datasets: 

## 1. Trap summary data frame
## "data/raw/PREEMPT/trapping/Reshaped PREEMPT Trap Estimates.v6.xlsx"
## contains information on the number of traps placed in each environment
## type, in each site, in each visit. Also contains info on missing 
## and closed/empty traps.

## 2. Rodent capture data 
## "data/raw/PREEMPT/trapping/PREEMPT Master Template.xlsx"
## contains information on the rodents that were captured. Each row
## describes a single rodent capture. Contains descriptors like
## species (based on field ID), PCR status, animal ID, trap ID, etc. 

## 3. Trap shapefiles
## "data/raw/PREEMPT/shapefiles"
## layers "Houses_Lassa_5Cycles" (House traps) and "Transects_traps_5cycles"
## (InTown and OutTown traps). These data are used to fill in
## latitude and longitude coordinates for each rodent captured. 

## Output is written to "data/clean/PREEMPT/aggregated_PREEMPT_capture_data.csv" 


library(tidyverse)
library(sf)
library(assertthat)

#==============================================================================


# Read in trap shapefiles, convert both to data frames, combine

# House trap shapefile
housetrap.shp <- read_sf(
  dsn = "data/raw/PREEMPT/shapefiles/", 
  layer = "Houses_Lassa_5Cycles"
)

# Transect trap shapefile
trantrap.shp <- read_sf(
  dsn = "data/raw/PREEMPT/shapefiles/", 
  layer = "Transects_traps_5cycles"
)

# Convert house trap shapefile to data frame for ease of processing
housetrap.dat <- housetrap.shp %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2],
    Visit = as.numeric(Visit),
    # change "H01" style "Track_ID" entries to "H1" style
    Track_ID = str_replace_all(Track_ID, "H0", "H")
  ) %>%
  # the "Lat" and "Long" columns are sometimes missing, so use the coordinate
  # values for every entry
  select(-Lat, -Long) %>%
  st_drop_geometry() %>%
  arrange(Town, Visit)

colnames(housetrap.dat) <- tolower(colnames(housetrap.dat))

# Verify that the total number of traps in each house corresponds with the
# given trap IDs 
# Note: two houses from Largo, visit 1 are missing. Confirmed with Bruno we
# just don't know the locations of these houses
housetrap.dat %>%
  group_by(town, visit) %>%
  summarize(
    n = n(),
    last_track_ID = last(track_id)
  )

# Import table giving the house traps with known georeferencing issues
# Note: these are four total houses that have assigned lat/longs, but we know 
# they're erroneous
housetrap.dat <- housetrap.dat %>%
  full_join(
    ., read_csv("data/cleaning_tables/house_trap_georeferencing_issues.csv"),
    by = c("town", "visit", "track_id")
  ) %>%
  mutate(
    georeferencing_issue = ifelse(
      is.na(georeferencing_issue), 
      "no", georeferencing_issue
    )
  )

# Histograms to quickly inspect lat/long data
housetrap.dat %>% 
  filter(georeferencing_issue == "no") %>%
  ggplot(aes(x = latitude)) +
  geom_histogram()

housetrap.dat %>% 
  filter(georeferencing_issue == "no") %>%
  ggplot(aes(x = longitude)) +
  geom_histogram()

# Convert transect trap shapefile to data frame for ease of processing
trantrap.dat <- trantrap.shp %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2]
  ) %>%
  # the "Lat" and "Long" columns are sometimes missing, so use the coordinate
  # values for every entry
  select(-Rat_catch, -Lassa_pos, -Lat, -Long) %>%
  st_drop_geometry() %>%
  arrange(Town, Visit, Trap_ID)

colnames(trantrap.dat) <- tolower(colnames(trantrap.dat))

# Verify that the total number of traps in each visit corresponds with the
# given trap IDs (potential issue with Bafodia, visit 5; Barlie, visit 1;
# Benduma, visit 1; Largo, visit 3)
trantrap.dat %>%
  group_by(town, visit) %>%
  summarize(
    n = n(),
    n_transects = n_distinct(track_id),
    last_trap_ID = last(trap_id)
  )

# Histograms to quickly inspect lat/long data
trantrap.dat %>% 
  ggplot(aes(x = latitude)) +
  geom_histogram()

trantrap.dat %>% 
  ggplot(aes(x = longitude)) +
  geom_histogram()

# Combine house and transect data frames
trap.coordinates <- plyr::rbind.fill(housetrap.dat, trantrap.dat) %>%
  rename(site = town) %>%
  mutate(
    visit = as.numeric(visit),
    habitat_code = substr(track_id, 1, 1),
    georeferencing_issue = ifelse(
      is.na(georeferencing_issue), 
      "no", georeferencing_issue
    )
  ) %>%
  select(
    site, visit, track_id, habitat_code, trap_id, 
    longitude, latitude, georeferencing_issue
  ) %>%
  arrange(site, visit)

#==============================================================================


# Read in and process rodent capture data

capture.dat <- readxl::read_excel(
  "data/raw/PREEMPT/trapping/PREEMPT Master Template.xlsx",
  sheet = 4
) %>%
  select(c(60:62, 65, 14:16, 20:21, 67, 69:70, 27, 86, 90))

colnames(capture.dat) <- c(
  "district", "chiefdom", "site", "date", "animal_id", "track_id", "trap_id",
  "sex", "health_status", "species", "visit", "night", "age_class", "lassa", "cmv"
)

capture.dat <- capture.dat %>%
  mutate(
    across(c(district, chiefdom, site), str_to_sentence),
    # Note this line will introduce NAs in the "visit" field, but those are all
    # records that ultimately get filtered out of the PREEMPT data
    across(c(animal_id, trap_id, visit), as.numeric),
    date = as.Date(date),
    site = case_when(
      str_detect(site, "Benduma") ~ "Benduma",
      TRUE ~ site
    ),
    repro = case_when(
      age_class %in% c("Subadult", "Adult") ~ "A",
      age_class == "Juvenile" ~ "J",
      TRUE ~ NA_character_
    ),
    sex = substr(sex, 1, 1),
    habitat_code = substr(track_id, 1, 1)
  )

# Restrict positive Lassa results to only those later confirmed via sequencing
dim(capture.dat)
sum(capture.dat$lassa)

seq.table <- read_csv("data/raw/sequencing_tables/SL_Mna_LASV_sequencing_table.csv")
capture.dat <- left_join(
  capture.dat,
  seq.table %>%
    select(-date, -L_accession) %>%
    rename(track_id = line_number) %>%
    mutate(animal_id = as.numeric(animal_id)),
  by = c("site", "species", "animal_id", "track_id")
) %>%
  mutate(
    lassa = ifelse(
      lassa == 1 & is.na(S_accession),
      0,
      lassa
    )
  ) %>%
  select(-S_accession)

dim(capture.dat)
sum(capture.dat$lassa)

# Create column with three-letter species name (Mastomys natalensis = Mna)
return_species_code <- function(x) {
  
    y <- strsplit(x, split = " ")[[1]]
    paste0(substr(y[1], 1, 1), substr(y[2], 1, 2))
}
capture.dat$sp <- sapply(capture.dat$species, return_species_code)

# Fix one animal with incorrect transect listed (this is a Bruno-approved
# change)
capture.dat[capture.dat$animal_id == 34, "track_id"] <- "OT2"


# Remove PREDICT sites

# capture.dat contains rodent capture data from PREDICT project. These data
# do not contain trap location information nor trapping effort, so they are
# not included in downstream analyses
# PREDICT sites have "track_id" that does not start with a letter or is missing
predict.sites <- is.na(capture.dat$track_id) | !is.na(as.numeric(capture.dat$track_id))
capture.dat <- capture.dat[!predict.sites, ]

# Confirm that all records have "visit" information following this filtering
assert_that(sum(is.na(capture.dat$visit)) == 0)


# Use "trap.coordinates" coordinates to fill in "capture.dat" lat/long

# Join in non-house traps first
capture.dat.mod <- capture.dat %>%
  left_join(
    .,
    trap.coordinates %>%
      filter(habitat_code != "H") %>%
      select(site, visit, track_id, trap_id, longitude, latitude),
    by = c("site", "visit", "track_id", "trap_id")
  )

# Then join in house traps, combining the two resulting lat/long columns
capture.dat.mod <- capture.dat.mod %>%
  left_join(
    .,
    trap.coordinates %>%
      filter(habitat_code == "H") %>%
      select(site, visit, track_id, longitude, latitude),
    by = c("site", "visit", "track_id")
  ) %>%
  rename(
    longitude = longitude.x,
    latitude = latitude.x
  ) %>%
  mutate(
    longitude = ifelse(is.na(longitude), longitude.y, longitude),
    latitude = ifelse(is.na(latitude), latitude.y, latitude)
  ) %>%
  select(-longitude.y, -latitude.y)

# Verify the two data frames have matching numbers of rows
assert_that(nrow(capture.dat) == nrow(capture.dat.mod))
# Verify that all records have been assigned lat/long
assert_that(sum(is.na(capture.dat.mod$latitude)) == 0)
assert_that(sum(is.na(capture.dat.mod$longitude)) == 0)

#==============================================================================


# Load in and process trapping data

# Load and combine trap.dat sheets into a single data frame:
# summary.trap.dat. This describes the number of traps set in each
# transect, the number missing, and the number closed/empty
summary.trap.dat <- c()
site.names <- unique(capture.dat$site)
for(site in site.names) {
  
    trap.dat.sheet <- readxl::read_excel(
      path = "data/raw/PREEMPT/trapping/Reshaped PREEMPT Trap Estimates.v6.xlsx",
      sheet = site,
      na = c("", "NA")
    )
    trap.dat.sheet$site <- site
    summary.trap.dat <- plyr::rbind.fill(summary.trap.dat, trap.dat.sheet)
}
colnames(summary.trap.dat) <- tolower(colnames(summary.trap.dat))

# Get rid of any entries where sampling didn't happen and rename transect column
summary.trap.dat <- summary.trap.dat %>%
  filter(!(is.na(n1_traps) & is.na(n2_traps) & is.na(n3_traps) & is.na(n4_traps))) %>%
  rename(track_id = transect)

# Get column names containing "Traps", "Miss", and "CE"
traps.cols <- names(summary.trap.dat)[grep("_traps", names(summary.trap.dat))] 
miss.cols <- names(summary.trap.dat)[grep("_miss", names(summary.trap.dat))]
ce.cols <- names(summary.trap.dat)[grep("_ce", names(summary.trap.dat))]

# Calculate the number of traps at each transect. This is the number of traps, 
# minus the number missing or closed/empty. 
summary.trap.dat$tot_traps <- 
  rowSums(summary.trap.dat[, traps.cols], na.rm = T) -
  rowSums(summary.trap.dat[, ce.cols], na.rm = T) -
  rowSums(summary.trap.dat[, miss.cols], na.rm = T)

# Add column that describes the number of non-zero trap nights
summary.trap.dat$nights <- rowSums(summary.trap.dat[, traps.cols] > 0, na.rm = T)

# Add a column that describes habitat type in a one letter code: H, House; I, InTown; O, OutTown
summary.trap.dat <- summary.trap.dat %>%
  mutate(habitat_code = substr(track_id, 1, 1)) %>%
  # remove any "habitat_code" observations that are not H, I, or O
  filter(habitat_code %in% c("H", "I", "O"))

# Sort columns
summary.trap.dat <- summary.trap.dat %>%
  select(
    date, site, visit, track_id, habitat_code, house_type, mastomys,
    everything()
  ) %>%
  arrange(site, visit)


# Troubleshooting trap numbers for in and out of town transects

summary.trap.dat %>%
  filter(habitat_code != "H") %>%
  group_by(site, visit) %>%
  summarize(trap_count_from_trapfile = sum(n1_traps)) %>%
  left_join(
    .,
    trap.coordinates %>%
      filter(habitat_code != "H") %>%
      group_by(site, visit) %>%
      summarize(trap_count_from_shapefile = n())
  ) %>%
  filter(trap_count_from_trapfile != trap_count_from_shapefile | is.na(trap_count_from_shapefile))

summary.trap.dat %>%
  filter(habitat_code != "H") %>%
  group_by(site, visit, track_id) %>%
  summarize(trap_count_from_trapfile = sum(n1_traps)) %>%
  left_join(
    .,
    trap.coordinates %>%
      filter(habitat_code != "H") %>%
      group_by(site, visit, track_id) %>%
      summarize(trap_count_from_shapefile = n())
  ) %>%
  filter(trap_count_from_trapfile != trap_count_from_shapefile | is.na(trap_count_from_shapefile))


# Write a copy of the trap summary data to disk
write_csv(summary.trap.dat, "data/clean/PREEMPT/trap_metadata.csv")

#==============================================================================


# Aggregate and combine trap summary data and trap coordinates data frame

# Aggregate trap coordinates over site
agg.latlon <- trap.coordinates %>%
  filter(georeferencing_issue == "no") %>%
  group_by(site) %>%
  summarize(
    longitude = mean(longitude),
    latitude = mean(latitude)
  ) %>%
  ungroup()

# Aggregate summary trap data over site, visit, and habitat type
agg.trap <- summary.trap.dat %>%
  group_by(site, visit, habitat_code) %>%
  summarize(
    tot_traps = sum(tot_traps),
    nights = mean(nights),
    date = min(date)
  ) %>%
  ungroup()

# Technical note: min(date) is chosen because one site (Petema) is coded using two
# dates (two days apart) for a single visit in "summary.trap.dat"

# Merge lat/long and aggregated trap data
agg.trap <- left_join(agg.trap, agg.latlon, by = "site")


# Aggregate the capture data

# Derive another data frame that aggregates "capture.dat.mod" based on species, 
# reproduction status, habitat code, visit, and site. This data frame will be 
# merged with trap data in the next step
agg.capture <- capture.dat.mod %>%
  group_by(site, visit, habitat_code, sp, species, repro) %>%
  summarize(
    tot_captures = n(),
    num_pos_Lassa = sum(lassa, na.rm = T),
    num_test_Lassa = sum(!is.na(lassa)),
    num_pos_CMV = sum(cmv, na.rm = T),
    num_test_CMV = sum(!is.na(cmv))
  ) %>%
  ungroup()


# Merge aggregated trap summary data and aggregated capture data

all.agg.trap.capture <- left_join(
  agg.trap, 
  agg.capture, 
  by = c("site", "visit", "habitat_code")
)

# Number of visits for each site

all.agg.trap.capture %>%
  distinct(site, visit) %>%
  group_by(site) %>%
  summarize(n_visits = n())


# Write filled-in version of "capture.dat" (has lat/long, has correct IDs) 
write_csv(capture.dat.mod, "data/clean/PREEMPT/PREEMPT_capture_data.csv")

# Write data frame that aggregates summary trap data, capture data, and trap shapefiles
write_csv(all.agg.trap.capture, "data/clean/PREEMPT/aggregated_PREEMPT_capture_data.csv")
