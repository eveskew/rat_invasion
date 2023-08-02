## This script processes the EFC_INCO-DEV dataset. Capture data from EFC
## is incorporated into a second data frame that lists each trap
## location. After that, this script aggregates the data in an attempt to 
## create a data frame that summarizes trap data and capture data for each 
## visit x date x site x local habitat combination. The first file is written
## to "data/clean/EFC/EFC_Trap_Data.csv", and the second is written to 
## "data/clean/EFC/Aggregated_EFC_Capture_Data.csv". 
## To the extent that is possible, this processing script is similar to that
## which processes the PREEMPT data.

## This script reads in two data files for processing: 

## 1. Trap summary data frame
## "data/raw/EFC/AJB_Modified_Trap_Metadata.xlsx"
## contains information on the number of traps placed in each environment
## type, in each site, in each visit. Also contains info on missing 
## and closed/empty traps.

## 2. Rodent capture data
## "data/raw/EFC/Rodent_Capture_EFC_INCO-DEV.xlsx"
## contains information on the rodents that were captured. Each row
## describes a single rodent capture. Contains descriptors like
## species, PCR status, animal ID, trap ID, etc. 


library(tidyverse)
library(readxl)
library(geosphere)
library(sp)
library(raster)

# Load pre-defined helper functions
source("R/functions.R")

#==============================================================================


# Load in and process trapping data

# Load and combine datasheets into a single data frame,
# "summary.trap.dat". This describes the number of traps set in each
# transect, the number missing, and the number closed/empty

summary.trap.dat <- c()
path <- "data/raw/EFC/trapping/AJB_Modified_Trap_Metadata.xls"
sheet.names <- excel_sheets(path = path)
# Remove the key sheet
sheet.names <- sheet.names[-which(sheet.names == "key")]
for(sheet in sheet.names){
  try({
    trap.dat.sheet <- read_excel(path = path, sheet = sheet)
    summary.trap.dat <- plyr::rbind.fill(summary.trap.dat, trap.dat.sheet)
  }, silent = TRUE)
}


# Translate coordinates from deg/min/sec to decimal degrees

# Use "get.decimal.coord" to convert transect start coordinate into decimal degrees
raw.dec.coords <- as.data.frame(
  t(sapply(summary.trap.dat$Start_Coord, FUN = get.decimal.coord, USE.NAMES = FALSE))
)
# Make extracted longitude negative (W)
raw.dec.coords[,2] <- -raw.dec.coords[,2]
names(raw.dec.coords) <- c("sLatitude", "sLongitude")
all.coords <- raw.dec.coords

# Now get the end coordinates of each transect
raw.dec.coords <- as.data.frame(
  t(sapply(summary.trap.dat$End_Coord, FUN = get.decimal.coord, USE.NAMES = FALSE))
)
# Make extracted longitude negative (W)
raw.dec.coords[,2] <- -raw.dec.coords[,2]
names(raw.dec.coords) <- c("eLatitude", "eLongitude")
all.coords <- cbind(all.coords, raw.dec.coords)
summary.trap.dat <- cbind(summary.trap.dat, all.coords)


# Average start and end coordinates together to get an average coordinate of each trap-line
summary.trap.dat$Longitude <- rowMeans(summary.trap.dat[, c("sLongitude", "eLongitude")], na.rm = TRUE)
summary.trap.dat$Latitude <- rowMeans(summary.trap.dat[, c("sLatitude", "eLatitude")], na.rm = TRUE)

# Rename columns for consistency with PREEMPT data
summary.trap.dat$TotTraps <- summary.trap.dat[, "trap-nights"]
summary.trap.dat$EFC.Code.Habitat <- summary.trap.dat[, "code habitat"]

# Remove "/" from "summary.trap.dat" Line entries. These are not in capture data
summary.trap.dat$Line <- gsub("/", "", x = summary.trap.dat$Line)

# Site names are encoded into the first two letters of
# Line. Extract this information and add a spelled-out site
# column. Remove entries without meaningful site name in the
# original datase. These removals are either irrelevant (empty row)
# or do not have trap-nights associated with them and are therefore
# unusable (e.g., remarkable points).
site.code <- substr(summary.trap.dat$Line, 1, 2)
summary.trap.dat$Site <- ifelse(site.code == 'BB', 'Bamba', 
                               ifelse(site.code == 'YA', 'Yafraya',
                                      ifelse(site.code == 'BA', 'Bantou', 
                                             ifelse(site.code == 'GA', 'Gayebombo',
                                                    ifelse(site.code == 'GB', 'Gbetaya',
                                                           ifelse(site.code == 'GG', 'Gagal',
                                                                  ifelse(site.code == 'KA', 'Kaali',
                                                                         ifelse(site.code == 'TA', 'Tanganya',
                                                                                ifelse(site.code == 'KO', 'Khoneya',
                                                                                       ifelse(site.code == 'SA', 'Sangassou',
                                                                                              ifelse(site.code == 'GN', 'Gania', NA)))))))))))
summary.trap.dat <- summary.trap.dat[!is.na(summary.trap.dat$Site), ]

# Use the start date of trapping to define a session name (session
# month: "sm"). In the capture dataset, EFC's sessions are Jan,
# May, Oct of the years 2002-2005. Define session months to one of
# these three months. This is analogous to "Visit" in PREEMPT
# data. 
summary.trap.dat$sm <- format(summary.trap.dat[, 'Date j1'], "%b")
summary.trap.dat$sm <- with(summary.trap.dat, ifelse(sm %in% c('Jan', 'Feb'), 'Jan',
                                                     ifelse(sm %in% c('May', 'Jun'), 'May',
                                                            ifelse(sm %in% c('Sep', 'Oct', 'Nov'), 'Oct',
                                                                   NA))))
if(sum(is.na(summary.trap.dat$sm))){writeLines("\n \n *** ERROR: NA SESSION MONTH *** \n \n")}
summary.trap.dat$Session <- paste(summary.trap.dat$sm, format(summary.trap.dat[, 'Date j1'], "-%y"), sep = '')


# Manually fill in coordinates in sites that are missing all coordinates
summary.trap.dat[summary.trap.dat$Site == 'Yafraya', 'Latitude'] <- 10.01194444	
summary.trap.dat[summary.trap.dat$Site == 'Yafraya', 'Longitude'] <- -13.67888889
summary.trap.dat[summary.trap.dat$Site == 'Bamba', 'Latitude'] <- 10.00055556		
summary.trap.dat[summary.trap.dat$Site == 'Bamba', 'Longitude'] <- -13.885

# Fill in other missing coordinates by averaging lat/lon over each site
agg.lat <- aggregate(Latitude~Site, data = summary.trap.dat, FUN = mean)
agg.lon <- aggregate(Longitude~Site, data = summary.trap.dat, FUN = mean)
summary.trap.dat <- subset(summary.trap.dat, , -c(Latitude, Longitude))
summary.trap.dat <- merge(summary.trap.dat, data.frame(Site = agg.lon$Site,
                                                       Longitude = agg.lon$Longitude,
                                                       Latitude = agg.lat$Latitude),
                          all.x = TRUE, by = 'Site')


# Convert EFC.Code.Habitat into PREEMPT Code.Habitat
summary.trap.dat$Code.Habitat <- ifelse(summary.trap.dat$EFC.Code.Habitat == '1', 'H',
                                        ifelse(summary.trap.dat$EFC.Code.Habitat %in% c('201', '301', '401', '501'),
                                               'I',
                                               'O'))
habs <- summary.trap.dat$Code.Habitat
summary.trap.dat$Type <- ifelse(habs == 'H', 'House',
                                ifelse(habs == 'I', 'InTown',
                                       ifelse(habs == 'O', 'OutTown', NA)))


# Add date information to "summary.trap.dat"
# Make a table to translate between session and start date
date.table <- unique(summary.trap.dat[, c('Session', 'Date j1', 'Site', 'EFC.Code.Habitat')])
date.table$Date <- as.Date(date.table[, 'Date j1'])
date.table <- as.data.frame(
  data.table::setDT(date.table)[,.(Date = median(as.Date(Date))),
                                by = list(Site, Session)]
)
summary.trap.dat <- merge(summary.trap.dat, date.table, all.x = TRUE, by = c('Site', 'Session'))

# Make a column that determines the number of trap nights ("Nights")
# Look for column names of the form "Nbi_Traps"
ntrap.cols <- grepl('_Traps', names(summary.trap.dat)) 
summary.trap.dat$Nights <- rowSums(summary.trap.dat[, ntrap.cols] > 0)

summary.trap.dat$Year <- as.numeric(format(summary.trap.dat$Date, format = '%Y'))

# Document the source of these data
summary.trap.dat$Country <- 'Guinea'
summary.trap.dat$Source <- 'EFC'

#==============================================================================


# Load in and process the rodent capture data

# Combine capture data sheets into a single data frame
capture.dat <- c()
# Load in sheets until no more are found
path <- "data/raw/EFC/trapping/Rodent_Capture_EFC_INCO-DEV.xlsx"
sheet.names <- excel_sheets(path = path)
capture.dat.sheet <- NULL
# Remove the key sheet
sheet.names <- sheet.names[-which(sheet.names == "key")]
for(sheet in sheet.names){
  try({
    capture.dat.sheet <- read_excel(path = path, sheet = sheet)
    capture.dat <- plyr::rbind.fill(capture.dat, capture.dat.sheet)
  }, silent = TRUE)
}
# Warnings come from certain cell entries being non-numeric (which should be fine)


# Rename/redefine certain columns
capture.dat$Repro <- ifelse(capture.dat[, "sex activity"] == "inactive", 'J', 'A')
capture.dat$EFC.Code.Habitat <- capture.dat[, "code habitat"]
capture.dat$Lassa <- NA
capture.dat$Lassa[(capture.dat[, "RT PCR-arena"] == 'positif')] <- 1
capture.dat$Lassa[(capture.dat[, "RT PCR-arena"] == 'negatif')] <- 0
capture.dat$LassaSero <- NA
capture.dat$Date <- as.Date(capture.dat$date)
capture.dat$Session <- format(capture.dat$session, "%b-%y")
capture.dat$Sp <- capture.dat[, 'Sp ADN']
capture.dat$foe <- as.numeric(capture.dat$foe)
capture.dat$Weight <- as.numeric(capture.dat$Weight)
capture.dat$Lens <- as.numeric(capture.dat$Lens)

# Row of "capture.dat" with Line number "TA0104X1" should be TA010401
capture.dat[capture.dat$Line=='TA0104X1', 'Line'] <- 'TA010401'


# Add full species names 

# From EFC Emails:
# N ma = Mus (sub-genera Nannomys) mattheyi
# N mi = Mus (Nannomys) minutoides
# Ns = Mus (Nannomys) setulosus
# Md = Myomys daltoni = Praomys daltoni in the updated taxonomy
# N sp = Mus (Nannomys) species when the molecular identity was not done
# N ba = Mus (Nannomys) baoulei
# Lo = Lophuromys sikapusi
# Tg = Tatera guinea, now Gerbilliscus guinea in the updated taxonomy
# Cg = Cricetomys gambianus
# Aa = Arvicanthis ansorguei
# Hybo = Hyomys sp
# Paraxerus = Paraxerus sp
# Funisciurus = Funisciurus sp

species.df <- data.frame(Sp = c("Me", "Rr",
                                "Pr",
                                "Ls", "Co", "Mn",
                                "N ma", "N mi",
                                "Cb", "Ns", "Tg",
                                "Md", "Ct", "Ur",
                                "N sp", "Mm", "Cl",
                                "Ll", "N ba", "Cc",
                                "Lo", "Hylo", "Hsi",
                                "Cg", "Aa", "Hybo",
                                "Paraxerus", "Funisciurus"),
                         Species_ID = c('Mastomys erythroleucus', 'Rattus rattus',
                                        'Praomys rostratus',
                                        'Lemniscomys striatus', 'Crocidura olivieri', 'Mastomys natalensis',
                                        'Mus mattheyi', 'Mus minutoides',
                                        'Crocidura buettikoferi', 'Mus setulosus', 'Gerbilliscus guinea',
                                        'Praomys daltoni', 'Crocidura theresae', 'Uranomys ruddi',
                                        'Mus sp', 'Mus musculus', 'Crocidura lamottei',
                                        'Lemniscomys linulus', 'Mus baoulei', 'Crocidura crossei',
                                        'Lophuromys sikapusi', 'Hylomyscus sp', 'Hylomyscus simus',
                                        'Cricetomys gambianus', 'Arvicanthis ansorguei', 'Hyomys sp',
                                        'Paraxerus sp', 'Funisciurus sp'))


# Convert "Sp" to character
capture.dat$Sp <- as.character(capture.dat$Sp)

# Merge full species names into "capture.dat"
capture.dat <- merge(capture.dat, species.df, all.x = TRUE)

# For consistency with PREEMPT data, 
# create a function that returns three-letter species name (Mastomys natalensis = Mna)
return_species_code <- function(x){
  y <- strsplit(x, split = " ")[[1]]
  return(paste0(substr(y[1],1,1), substr(y[2],1,2)))
}

capture.dat$Sp <- sapply(capture.dat$Species_ID, return_species_code)

# Change "Sp" to NA for any species that has NA "Species_ID"
not.valid <- is.na(capture.dat$Species_ID)
capture.dat[not.valid, 'Sp'] <- NA_character_
capture.dat[not.valid, 'Species_ID'] <- NA_character_


# Write to file
write_csv(capture.dat, "data/clean/EFC/EFC_Capture_Data.csv")

#==============================================================================


# Convert EFC habitat codes into PREEMPT habitat codes

# Roughly, the EFC codes are
# 1: house
# 201: proximal cultivation
# 202: distal cultivation
# 301: proximal meadow
# 302: distal meadow
# 401: proximal forest
# 402: distal forest
# 501: proximal savannah
# 502: distal savannah

# PREEMPT values are 
# H: in house
# I: in town / proximal cultivation
# O: out of town / distal cultivation

# Only keep entries with valid "EFC.Code.Habitat" entry
capture.dat <- subset(capture.dat, EFC.Code.Habitat %in% c(1, 201, 202, 301, 302, 401, 402, 501, 502))

# Convert "EFC.Code.Habitat" into PREEMPT "Code.Habitat"
capture.dat$Code.Habitat <- ifelse(capture.dat$EFC.Code.Habitat == '1', 'H',
                                   ifelse(capture.dat$EFC.Code.Habitat %in% c('201', '301', '401', '501'),
                                          'I',
                                          'O'))
habs <- capture.dat$Code.Habitat
capture.dat$Type <- ifelse(habs == 'H', 'House',
                           ifelse(habs == 'I', 'InTown',
                                  ifelse(habs == 'O', 'OutTown', NA)))

#==============================================================================


# Aggregate all capture data into counts by species x site x session x habitat x age

#  Aggregate capture data
capture.dat$Repro <- paste(capture.dat$Repro)

agg.capture <- data.table::setDT(capture.dat)[,.(Date = median(Date, na.rm = TRUE),
                                                 TotCaptures = .N,
                                                 NumPosLassa = sum(Lassa, na.rm = TRUE),
                                                 NumTestLassa = sum(!is.na(Lassa)),
                                                 NumPosLassaAb = sum(LassaSero, na.rm = TRUE),
                                                 NumTestLassaAb = sum(!is.na(LassaSero))
),
by = list(Site, Session, Sp,
          Species_ID, Repro, Line)]

agg.capture$rec <- FALSE # Indicator of being recorded (for debugging)


# Merge aggregated trap summary data and aggregated capture data
# The aggregated data frame will be called "all.agg.trap.capture" 

all.species <- as.character(unique(capture.dat$Species_ID))
all.agg.trap.capture <- c()

# Define a data frame that is used for debugging
all.err.dat <- c()

efc.sites <- unique(capture.dat$Site)

for(focal.species in all.species) {
  
  writeLines(paste0('Aggregating: ', focal.species))
  
  mask.species <- agg.capture$Species_ID == focal.species
  
  split.species.name <- strsplit(focal.species, ' ')[[1]]
  genus <- split.species.name[1]
  species <- split.species.name[2]
  mask.adult <- agg.capture$Repro == 'A'
  mask.juvenile <- agg.capture$Repro == 'J'
  mask.na <- agg.capture$Repro == 'NA'
  
  agg.trap.capture <- summary.trap.dat
  agg.trap.capture$Sp <- return_species_code(focal.species)
  agg.trap.capture$Species_ID <- focal.species
  agg.trap.capture$Genus <- genus
  agg.trap.capture$Species <- species
  
  # Loop through aggregated trap data, fill in information on focal rodent into "agg.trap.capture"
  for(ii in 1:nrow(summary.trap.dat)) {
    Site <- summary.trap.dat$Site[ii]
    Code.Habitat <- summary.trap.dat$Code.Habitat[ii]
    Session <- summary.trap.dat$Session[ii]
    TotTraps <- summary.trap.dat$TotTraps[ii]
    Line <- summary.trap.dat$Line[ii]
    Nights <- summary.trap.dat$Nights
    
    # Find the corresponding entries in "agg.capture"
    mask.captures <- agg.capture$Site == Site & 
      agg.capture$Session == Session & agg.capture$Line == Line
    
    agg.capture[mask.captures, 'rec'] <- TRUE # Mark as recorded (for debugging)
    
    # Extract data from "agg.capture"
    tot <- mask.species & mask.captures
    tot.juv <- mask.juvenile & mask.species & mask.captures
    tot.adult <- mask.adult & mask.species & mask.captures
    tot.na <- mask.na & mask.species & mask.captures
    TotCaptures <- ifelse(sum(tot), sum(agg.capture[tot, 'TotCaptures']), 0)
    TotJuvCaptures <- ifelse(sum(tot.juv), sum(agg.capture[tot.juv, 'TotCaptures']), 0)
    TotAdultCaptures <- ifelse(sum(tot.adult), sum(agg.capture[tot.adult , 'TotCaptures']), 0)
    TotNACaptures <- ifelse(sum(tot.na), sum(agg.capture[tot.na , 'TotCaptures']), 0)
    Date <- date.table[date.table$Session == Session & date.table$Site == Site, 'Date']
    
    # Extract PCR info
    TotPosVirus <- ifelse(sum(tot), sum(agg.capture[tot, 'NumPosLassa']), 0)
    TotTestVirus <- ifelse(sum(tot), sum(agg.capture[tot, 'NumTestLassa']), 0)
    
    # Extract serology info (placeholder)
    TotPosAb <- ifelse(sum(tot), sum(agg.capture[tot, 'NumPosLassaAb']), 0)
    TotTestAb <- ifelse(sum(tot), sum(agg.capture[tot, 'NumTestLassaAb']), 0)
    
    agg.trap.capture[ii, 'Tot'] <- TotCaptures
    agg.trap.capture[ii, 'TotJuv'] <- TotJuvCaptures
    agg.trap.capture[ii, 'TotAdult'] <- TotAdultCaptures
    agg.trap.capture[ii, 'Tot_NA_Age'] <- TotNACaptures
    
    agg.trap.capture[ii, 'Date'] <- as.Date(Date)
    
    # Add in LASV PCR results
    agg.trap.capture[ii, 'NumPosLassa'] <- TotPosVirus
    agg.trap.capture[ii, 'NumTestLassa'] <- TotTestVirus
    agg.trap.capture[ii, 'PropVirus'] <- TotPosVirus/TotTestVirus
    
    # Add in LASV serology results
    agg.trap.capture[ii, 'NumPosLassaAb'] <- TotPosAb
    agg.trap.capture[ii, 'NumTestLassaAb'] <- TotTestAb
    
  } # End loop through "agg.trap"
  
  all.agg.trap.capture <- plyr::rbind.fill(all.agg.trap.capture, agg.trap.capture)        
  
  # Loop through sites and make sure that each focal rodent capture
  # from "capture.dat" is accounted for in "agg.trap.capture", which
  # was derived from trap metadata
  err.dat <- c()
  for(site in efc.sites) {
    
    tot.cap1 <- sum(capture.dat$Site == site & capture.dat$Species_ID == focal.species)
    
    mask2 <- agg.trap.capture$Site == site & agg.trap.capture$Species_ID == focal.species 
    
    tot.cap2 <- if(sum(mask2)){sum(agg.trap.capture[mask2, 'Tot'])}else{0}
    
    new.row <- data.frame(Species = focal.species, Site = site,
                          t1 = tot.cap1, t2 = tot.cap2)
    err.dat <- rbind(err.dat, new.row)
  }
  
  if(sum(err.dat$tot.cap1 != err.dat$tot.cap2)) { 
    
    all.err.dat <- rbind(all.err.dat, err.dat)
    writeLines("\n \n *** ERROR: CAPTURE DATA NOT ACCOUNTED FOR *** \n \n ")
  }
  
} # End loop through all species


# Fill in year and month information
all.agg.trap.capture[, 'Month'] <- paste(month.abb[lubridate::month(all.agg.trap.capture$Date)])
all.agg.trap.capture[, 'Year'] <- as.numeric(format(all.agg.trap.capture$Date, format = '%Y'))

# Add "Visit" column to make this work well with PREEMPT data
all.agg.trap.capture$Visit <- all.agg.trap.capture$Session
all.agg.trap.capture <- subset(all.agg.trap.capture, , -Session)

# Add trap success of juveniles, adults
all.agg.trap.capture$TSA <- with(all.agg.trap.capture, TotAdult / TotTraps)
all.agg.trap.capture$TSJ <- with(all.agg.trap.capture, TotJuv / TotTraps)
all.agg.trap.capture$TS <- with(all.agg.trap.capture, Tot / TotTraps)


# Only keep certain columns and reorder
keep.cols <- c('Site', 'Visit', 'Code.Habitat', 'TotTraps', 'Nights',
               'Date', 'Longitude', 'Latitude', 'Sp', 'Species_ID',
               'Genus', 'Species', 'Country', 'Year', 'Type',
               'Source', 'Tot', 'TotJuv', 'TotAdult', 'TS',
               'TSJ', 'TSA', 'NumPosLassa', 'NumTestLassa', 'PropVirus',
               'NumPosLassaAb', 'NumTestLassaAb')
all.agg.trap.capture.save <- all.agg.trap.capture[, keep.cols]


# Write cleaned data to disk
write_csv(all.agg.trap.capture.save, "data/clean/EFC/Aggregated_EFC_Capture_Data.csv")


# Number of visits for each site
rowSums(table(all.agg.trap.capture$Site, all.agg.trap.capture$Visit) > 0)
