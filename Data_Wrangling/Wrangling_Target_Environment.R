#### Initial Settings ####
library(tidyverse)
library(tidycensus)
library(sf)
library(lubridate)
library(viridis)
library(reshape)
library(reshape2)
library(stargazer)
library(FNN)

# school_master data obtained from Professor Jensen
# crime data, litter index, 311 requests, code violations, land use and DOR downloaded from open data philly
# crime data downloaded on 01/19/2020
# crime incidence metadata: http://metadata.phila.gov/#home/datasetdetails/5543868920583086178c4f8e/representationdetails/570e7621c03327dc14f4b68d/
# litter index (2017-2018) metadata: http://metadata.phila.gov/#home/datasetdetails/555f812bf15fcb6c6ed44110/representationdetails/5d0d300ef71c9f000a28758e/
# 311 calls and code violation data downloaded on 10/05/2019
# 311 calls metadata: http://metadata.phila.gov/#home/datasetdetails/5543864d20583086178c4e98/representationdetails/5762e19fa237544b2ecfe722/
# code violation metadata: http://metadata.phila.gov/#home/datasetdetails/5543865120583086178c4ead/
# landuse metadata: http://metadata.phila.gov/#home/datasetdetails/5543864420583086178c4e74/?view_219_sort=field_15|asc

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )}
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}


nn_function <- function(measureFrom,measureTo,k) {
  
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = max(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint)
  
  return(output)  
}

setwd("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Data")

#### get census data ####

census_api_key("REPLACE THIS TEXT WITH YOUR API KEY")

varlist_2010 <- load_variables(2010, "sf1", cache = TRUE)

census2010_vars <- c("P001001", # Total population by race
              "P008003", # white population
              "P008004", # black population
              "P004003", # hispanic population
              "P003005", # estimate asian alone
              "P012A006", # male 15-17
              "P012A007", # male 18-19
              "P012A008", # male 20
              "P012A009", # male 21
              "P012A010", # male 22-24
              "P012A011") # male 25-29
 
census_2010 <- get_decennial(geography = "block", variables = census2010_vars, year = 2010,
                    state = "PA", county = "Philadelphia", geometry = FALSE, output = "wide")

census_2010 <- census_2010 %>% 
  mutate(pct_white = (P008003/P001001)*100,
         pct_black = (P008004/P001001)*100,
         pct_hisp = (P004003/P001001)*100,
         pct_asian = (P003005/P001001)*100,
         pct_young_male = ((P012A006 + P012A007 + P012A008 + P012A009 + P012A010 + P012A011)/P001001)*100) %>% 
  dplyr::rename(total_pop = P001001) %>% 
  select(GEOID, NAME, total_pop, pct_white, pct_black, pct_hisp, pct_asian, pct_young_male)

# replace NAs generated in blocks with population = 0
census_2010[is.na(census_2010)] <- 0

philly_blocks <- st_read("Census_Blocks_2010.shp")
philly_blocks <- st_transform(philly_blocks, 2272)
philly_blocks <- philly_blocks %>% 
  select(GEOID10, geometry)

census_2010 <- census_2010 %>% 
  dplyr::rename(GEOID10 = GEOID)

dat <- left_join(philly_blocks, census_2010)

varlist_2017 <- load_variables(2017, "acs5", cache = TRUE)

acs2017_vars <- c("B19013_001E", # median hh_income
                  "B01003_001E", # estimate pop 2017
                  "B99172_001E") # families in poverty

acs_2017 <- get_acs (geography = "block group",
                     year = 2017,
                     variables = acs2017_vars, 
                     geometry = FALSE, 
                     state = "PA", 
                     county = "Philadelphia",
                     output = "wide")

acs_2017 <- acs_2017 %>%
  select (GEOID, NAME, acs2017_vars)

dat$block_geoid = substr(dat$GEOID10, 1, 12)

acs_2017 <- acs_2017 %>% 
  dplyr::rename(block_geoid = GEOID)

acs_2017 <- acs_2017 %>%
  dplyr::rename(
    median_hh_inc = B19013_001E,
    pop_17 = B01003_001E,
    family_in_pov = B99172_001E
  )

dat <- left_join(dat, acs_2017, by = "block_geoid")

# replace NAs for 0

dat[is.na(dat)] <- 0

dat <- dat %>% 
  select(-NAME.y) %>% 
  dplyr::rename(NAME = NAME.x)

#### crime dataset ####

crime_incidence <- read.csv("incidents_part1_part2.csv") # downloaded on 01/19/2020

crime_incidence$dispatch_as_date <- ymd(crime_incidence$dispatch_date)
crime_incidence$year_crime <- year(crime_incidence$dispatch_as_date)
crime_incidence$month_crime <- month(crime_incidence$dispatch_as_date)
crime_incidence$day_of_week <- weekdays(crime_incidence$dispatch_as_date)
crime_incidence$is_weekend <- if_else(crime_incidence$day_of_week == "Sunday" | crime_incidence$day_of_week == "Saturday", 1, 0)

crime_incidence$month_crime <- as.integer(crime_incidence$month_crime)
crime_incidence$summer <- if_else(crime_incidence$month_crime > 5 & crime_incidence$month_crime < 9, 1, 0)

crime_incidence$target <- if_else(crime_incidence$is_weekend == 0 & crime_incidence$hour_ > 5 & crime_incidence$hour_ < 21 & crime_incidence$summer == 0, 1, 0)
summary(as.factor(crime_incidence$target))

target_crime_incidence = filter(crime_incidence, crime_incidence$target == 1)
target_crime_incidence = na.omit(target_crime_incidence, point_x)

# now, to an sf object:
crime_sf <- st_as_sf(na.omit(target_crime_incidence), coords = c("point_x", "point_y"), crs = 4326)
crime_sf <- crime_sf %>%
  st_transform(st_crs(2272))

# recategorizing crime types
# we will select crimes related to schools, and discard unrelated crimes, like fraud and embezzlement
summary(as.factor(crime_sf$text_general_code))

crime_sf <- crime_sf %>% 
  mutate(crime_type = 
           case_when(
             ucr_general == 400 ~ "violent_crime", # aggravated assaults
             ucr_general == 100 ~ "violent_crime", # homicide
             ucr_general == 800 ~ "violent_crime", # other assaults
             ucr_general == 200 ~ "violent_crime", # rape
             ucr_general == 300 ~ "violent_crime", # robbery
             ucr_general == 1700 ~ "violent_crime", # other sex offenses
             
             ucr_general == 1800 ~ "drug_crime", # narcotics
             
             ucr_general == 900 ~ "crime_against_property", # arson
             ucr_general == 500 ~ "crime_against_property", # burglary 
             ucr_general == 700 ~ "crime_against_property", # Motor Vehicle Theft
             ucr_general == 600 ~ "crime_against_property", # theft
             ucr_general == 1300 ~ "crime_against_property", # Receiving Stolen Property
             
             ucr_general == 2600 ~ "other_crimes", # All Other Offenses
             ucr_general == 2400 ~ "other_crimes", # Disorderly Conduct
             ucr_general == 2100 ~ "other_crimes", # DUI
             ucr_general == 2200 ~ "other_crimes", # Liquor Law Violations
             ucr_general == 2000 ~ "other_crimes", # Offenses Against Family and Children
             ucr_general == 1600 ~ "other_crimes", # Prostitution
             ucr_general == 2300 ~ "other_crimes", # Public Drunkenness
             ucr_general == 2500 ~ "other_crimes", # Vagrancy/Loitering
             ucr_general == 1400 ~ "other_crimes", # Vandalism/Criminal Mischief
             ucr_general == 1500 ~ "other_crimes", # weapon Violations
             
             ucr_general == 1200 ~ "unrelated", # Embezzlement
             ucr_general == 1000 ~ "unrelated", # Forgery
             ucr_general == 1100 ~ "unrelated", # Fraud
             ucr_general == 1900 ~ "unrelated", # Gambling
             
             TRUE ~ "non_categorized"
           )
  )


summary(as.factor(crime_sf$crime_type))

crime_sf <- crime_sf %>% 
  select(geometry, crime_type, text_general_code)

crime_per_block <- st_join(philly_blocks, crime_sf)

crime_per_block <- crime_per_block %>% 
  mutate(count_crime = 1) %>% 
  as_tibble() %>% 
  group_by(GEOID10, crime_type) %>% 
  summarise(total_crime = sum(count_crime))
  
crime_per_block <- cast(crime_per_block, GEOID10~crime_type, sum)
  
crime_per_block <- crime_per_block %>% 
  select(GEOID10, crime_against_property, drug_crime, other_crimes, violent_crime, unrelated)

dat_crime <- left_join(dat, crime_per_block, by = "GEOID10")

#### school dataset #### 

schools <- read.csv("school_master.csv")
schools <- schools %>%
  dplyr::rename(level = School.Level)

schools = schools %>%
  select(lon, lat, level)

schools <- st_as_sf(schools, coords = c("lon", "lat"), crs = 4326)
schools <- schools %>%
  st_transform(st_crs(2272))

schools_buffer <- st_buffer(schools, dist = 656.168)

blocks_with_school <- st_join(schools_buffer, philly_blocks)

blocks_with_school <- blocks_with_school %>% 
  as_tibble() %>% 
  select(-geometry)

blocks_with_school <- blocks_with_school %>% 
  mutate(
    highest_level = case_when(
      blocks_with_school$level == "HIGH" ~ 10000,
      blocks_with_school$level == "MIDDLEHIGH" ~ 10000,
      blocks_with_school$level == "ELEMENTARYMIDDLEHIGH" ~ 10000,
      
      blocks_with_school$level == "MIDDLE" ~ 100,
      blocks_with_school$level == "ELEMENTARYMIDDLE" ~ 100,
      
      blocks_with_school$level == "ELEMENTARY" ~ 1,
      
      TRUE ~ 0
    )
  )

blocks_with_school <- blocks_with_school %>% 
  group_by(GEOID10) %>% 
  summarise(school_level = sum(highest_level)) %>% 
  select(GEOID10, school_level)

dat_crime <- left_join(dat_crime, blocks_with_school, by = "GEOID10")
summary(as.factor(dat_crime$school_level))

dat_crime$near_school <- if_else(is.na(dat_crime$school_level), 0, 1)

dat_crime$near_highschool <- if_else(dat_crime$school_level > 9999, 1, 0)

dat_crime$near_middleschool <- if_else(dat_crime$school_level > 99 & dat_crime$school_level < 9999, 1, 0)

dat_crime$near_elementaryschool <- if_else(dat_crime$school_level < 99, 1, 0)

#### litter index dataset ####

litter_index <- st_read("Litter_Index_Blocks.shp")

litter_index <- litter_index %>% 
  st_transform(st_crs(2272))

litter_index <- litter_index %>% 
  st_centroid() %>% 
  select(geometry, HUNDRED_BL)

litter_index <- st_join(philly_blocks, litter_index)

summary_litter <- litter_index %>% 
  as_tibble() %>% 
  group_by(GEOID10) %>% 
  summarise(avg_litter = mean(HUNDRED_BL))

dat_crime <- left_join(dat_crime, summary_litter)

#### violations dataset ####

violations <- read.csv("streets_code_violation_notices.csv") # downloaded at 01/19/2020

# to an sf object:

violations <- violations %>% 
  drop_na(lat, lng)

violations <- st_as_sf(violations, coords = c("lng", "lat"), crs = 4326)
violations <- st_transform(violations, st_crs(2272))

violations <- violations %>% 
  mutate(viola = 1) %>% 
  select(viola, geometry)

violations_per_block <- st_join(philly_blocks, violations, left = TRUE)
violations_per_block$viola[is.na(violations_per_block$viola)] <- 0

summary_violations <- violations_per_block %>% 
  as_tibble() %>% 
  group_by(GEOID10) %>% 
  summarise(total_code_viola = sum(viola))

dat_crime <- left_join(dat_crime, summary_violations)

#### 311 calls dataset ####

request_311 <- read.csv("public_cases_fc.csv") # downloaded at 01/19/2020
request_311 <- na.omit(request_311, request_311$lon)

# convert it to a sf object:
request_311 <- st_as_sf(request_311, coords = c("lon", "lat"), crs = 4326)
request_311 <- st_transform(request_311, st_crs(2272))

# Look at the type of requests and create new variables
summary(as.factor(request_311$service_name))

abandoned_vehicle <- filter(request_311, request_311$service_name == "Abandoned Bike" |
                              request_311$service_name == "Abandoned Vehicle") %>% 
  mutate(aband_vehicl = 1) %>% 
  select(aband_vehicl, geometry)

street_light_out <- filter(request_311, request_311$service_name == "Street Light Outage" |
                             request_311$service_name == "Alley Light Outage" ) %>% 
  mutate(light_out = 1) %>% 
  select(light_out, geometry)

graffiti <- filter(request_311, request_311$service_name == "Graffiti Removal") %>% 
  mutate(graffiti = 1) %>% 
  select(graffiti, geometry)

dumping <- filter(request_311, request_311$service_name == "Sanitation / Dumpster Violation" |
                    request_311$service_name == "Illegal Dumping" ) %>% 
  mutate(dumping = 1) %>% 
  select(dumping, geometry)

infestation <- filter(request_311, request_311$service_name == "Infestation Residential") %>% 
  mutate(infestation = 1) %>% 
  select(infestation, geometry)

bad_street <- filter(request_311, request_311$service_name == "Dangerous Sidewalk" |
                       request_311$service_name == "Hydrant Knocked Down (No Water)" |
                       request_311$service_name == "Hydrant Request" |
                       request_311$service_name == "Manhole Cover" |
                       request_311$service_name == "Street Defect" |
                       request_311$service_name == "Street Paving") %>% 
  mutate(bad_street = 1) %>% 
  select(bad_street, geometry)

abandoned_vehicle <- st_join(abandoned_vehicle, philly_blocks, left = TRUE)
sum(is.na(abandoned_vehicle$GEOID10))
abandoned_vehicle <- na.omit(abandoned_vehicle)

abandoned_vehicle <- abandoned_vehicle %>% 
  as_tibble() %>% 
  group_by(GEOID10) %>% 
  summarise(aband_vehicl = sum(aband_vehicl))

dat_crime <- left_join(dat_crime, abandoned_vehicle)

street_light_out <- st_join(street_light_out, philly_blocks, left = TRUE)
sum(is.na(street_light_out$GEOID10))
street_light_out <- na.omit(street_light_out)

street_light_out <- street_light_out %>% 
  as_tibble() %>% 
  group_by(GEOID10) %>% 
  summarise(light_out = sum(light_out))

dat_crime <- left_join(dat_crime, street_light_out)

graffiti <- st_join(graffiti, philly_blocks, left = TRUE)
sum(is.na(graffiti$GEOID10))
graffiti <- na.omit(graffiti)

graffiti <- graffiti %>% 
  as_tibble() %>% 
  group_by(GEOID10) %>% 
  summarise(graffiti = sum(graffiti))

dat_crime <- left_join(dat_crime, graffiti)

dumping <- st_join(dumping, philly_blocks, left = TRUE)
sum(is.na(dumping$GEOID10))
dumping <- na.omit(dumping)

dumping <- dumping %>% 
  as_tibble() %>% 
  group_by(GEOID10) %>% 
  summarise(dumping = sum(dumping))

dat_crime <- left_join(dat_crime, dumping)

infestation <- st_join(infestation, philly_blocks, left = TRUE)
sum(is.na(infestation$GEOID10))
infestation <- na.omit(infestation)

infestation <- infestation %>% 
  as_tibble() %>% 
  group_by(GEOID10) %>% 
  summarise(infestation = sum(infestation))

dat_crime <- left_join(dat_crime, infestation)

bad_street <- st_join(bad_street, philly_blocks, left = TRUE)
sum(is.na(bad_street$GEOID10))
bad_street <- na.omit(bad_street)

bad_street <- bad_street %>% 
  as_tibble() %>% 
  group_by(GEOID10) %>% 
  summarise(bad_street = sum(bad_street))

dat_crime <- left_join(dat_crime, bad_street)

#### land use dataset ####

land_use <- st_read("land_use.shp")
land_use <- st_transform(land_use, 2272)

land_use <- land_use %>% 
  mutate(LU_type = 
           case_when(
             c_dig2desc == 11 ~ "residential",
             c_dig2desc == 12 ~ "residential",
             c_dig2desc == 13 ~ "residential",
             
             c_dig2desc == 21 ~ "comercial",
             c_dig2desc == 22 ~ "comercial",
             c_dig2desc == 23 ~ "comercial",
             
             c_dig2desc == 31 ~ "industrial",
             
             c_dig2desc == 41 ~ "civic",
             
             c_dig2desc == 51 ~ "transport",
             c_dig2desc == 52 ~ "transport",
             
             c_dig2desc == 61 ~ "culture",
             c_dig2desc == 62 ~ "culture",
             c_dig2desc == 71 ~ "culture",
             c_dig2desc == 72 ~ "culture",
             
             c_dig2desc == 81 ~ "water",
             
             c_dig2desc == 91 ~ "vacant",
             
             TRUE ~ "other"
           ))

land_use <- land_use %>% 
  select(geometry, LU_type)

land_use$lu_area <- st_area(land_use)

land_use <- st_centroid(land_use)

land_use$lu_area_acres <- land_use$lu_area/43560

options(scipen = 999)

philly_tracts <- st_read("Census_Tracts_2010.shp")
philly_tracts <- st_transform(philly_tracts, 2272)

tracts_LU <- st_join(philly_tracts, land_use)

tracts_LU <- tracts_LU %>% 
  as_tibble() %>% 
  group_by(GEOID10, LU_type) %>% 
  summarise(area_lu_acres = sum(lu_area_acres))

tracts_LU <- cast(tracts_LU, GEOID10~LU_type, sum)

tracts_LU <- tracts_LU %>% 
  select(GEOID10, civic, comercial, culture, industrial, other, residential, transport, vacant, water)

dat_crime$joingeoid <- substr(dat_crime$GEOID10, 1, 11)

tracts_LU <- tracts_LU %>% 
  dplyr::rename(joingeoid = GEOID10)
                
dat_crime <- left_join(dat_crime, tracts_LU, by = "joingeoid")

dat_crime <- dat_crime %>% 
  select(-joingeoid)

# replace NAs:
dat_crime[is.na(dat_crime)] <- 0

dat_crime <- dat_crime %>% 
  mutate(
    area = st_area(dat_crime),
    area_acres = area/43560,
    crime_index = (drug_crime + crime_against_property + other_crimes + violent_crime)/area_acres,
    violent_crime_index = violent_crime/area_acres,
    drug_crime_index = drug_crime/area_acres,
    property_crime_index = crime_against_property/area_acres
  )

dat_crime$crime_index <- as.numeric(dat_crime$crime_index)
dat_crime$violent_crime_index <- as.numeric(dat_crime$violent_crime_index)
dat_crime$drug_crime_index <- as.numeric(dat_crime$drug_crime_index)
dat_crime$property_crime_index <- as.numeric(dat_crime$property_crime_index)

dat_crime <- dat_crime %>% 
  mutate(total_LU = civic + culture + comercial + industrial + other + 
           residential + transport + vacant + water,
         pct_civic = civic/total_LU,
         pct_culture = culture/total_LU,
         pct_comercial = comercial/total_LU,
         pct_industrial = industrial/total_LU,
         pct_other = other/total_LU,
         pct_residential = residential/total_LU,
         pct_transport = transport/total_LU,
         pct_vacant = vacant/total_LU,
         pct_water = water/total_LU
  )


dat_crime <- dat_crime %>% 
  mutate(code_violation_index = total_code_viola/area_acres,
         aband_vehicl_index = aband_vehicl/area_acres,
         light_out_index = light_out/area_acres,
         graffiti_index = graffiti/area_acres,
         dumping_index = dumping/area_acres,
         infestation_index = infestation/area_acres,
         bad_street_index = bad_street/area_acres
  )

dat_crime$code_violation_index <- as.numeric(dat_crime$code_violation_index)
dat_crime$aband_vehicl_index <- as.numeric(dat_crime$aband_vehicl_index)
dat_crime$dumping_index <- as.numeric(dat_crime$dumping_index)
dat_crime$light_out_index <- as.numeric(dat_crime$light_out_index)
dat_crime$graffiti_index <- as.numeric(dat_crime$graffiti_index)
dat_crime$infestation_index <- as.numeric(dat_crime$infestation_index)
dat_crime$bad_street_index <- as.numeric(dat_crime$bad_street_index)

#### include business dataset ####

business <- read.csv("business_frame_hours.csv")

business_sf <- st_as_sf(business, coords = c("lng", "lat"), crs = 4326)

business_sf <- business_sf %>%
  st_transform(st_crs(2272))

ggplot()+
  geom_sf(data = dat_crime, aes(fill = pct_white), color = NA) +
  geom_sf(data = business_sf, aes(), color = 'white', size = .6) +
  scale_fill_viridis() +
  labs(title = "All Business Location") +
  mapTheme()

business_sf$business_type <- case_when(
  business_sf$cafe == TRUE ~ "cafe",
  business_sf$convenience == TRUE ~ "convenience",
  business_sf$gym == TRUE ~ "gym",
  business_sf$institution == TRUE ~ "institution",
  business_sf$liquor == TRUE ~ "liquor",
  business_sf$lodging == TRUE ~ "lodging",
  business_sf$nightlife == TRUE ~ "nightlife",
  business_sf$pharmacy == TRUE ~ "pharmacy",
  business_sf$restaurant == TRUE ~ "restaurant",
  business_sf$retail == TRUE ~ "retail",
  TRUE ~ "other"
)

summary(as.factor(business_sf$business_type))

business_per_block <- st_join(business_sf, philly_blocks)

business_summ <- business_per_block %>% 
  as_tibble() %>% 
  mutate(count_event = 1) %>% 
  group_by(GEOID10, business_type) %>% 
  summarise(total_business = sum(count_event))

business_summ <- cast(business_summ, GEOID10~business_type, sum)

dat_crime <- left_join(dat_crime, business_summ, by = "GEOID10")

dat_crime[is.na(dat_crime)] <- 0

# Visualize Business Concentration

ggplot()+
  geom_sf(data = dat_crime, aes(fill = retail), color = NA) +
  scale_fill_viridis() +
  labs(title = "Concentration business per block") +
  mapTheme()

blocks_centroids <- st_centroid(philly_blocks)
cafes <- filter(business_sf, cafe == TRUE) %>% select(geometry)
conv <- filter(business_sf, convenience == TRUE) %>% select(geometry)
gyms <- filter(business_sf, gym == TRUE) %>% select(geometry)
institutions <- filter(business_sf, institution == TRUE) %>% select(geometry)
liquors <- filter(business_sf, liquor == TRUE) %>% select(geometry)
lodgings <- filter(business_sf, lodging == TRUE) %>% select(geometry)
nightlifes <- filter(business_sf, nightlife == TRUE) %>% select(geometry)
pharmacys <- filter(business_sf, pharmacy == TRUE) %>% select(geometry)
restaurants <- filter(business_sf, restaurant == TRUE) %>% select(geometry)
retails <- filter(business_sf, retail == TRUE) %>% select(geometry)

block_XY <- blocks_centroids %>%
  cbind(.,st_coordinates(blocks_centroids))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

cafe_XY <- cafes %>%
  cbind(.,st_coordinates(cafes))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

dist_cafe <- as.data.frame(nn_function(block_XY, cafe_XY, 5)) %>% dplyr::rename(distance_cafe = pointDistance)
dat_crime$distance_cafe <- dist_cafe$distance_cafe

conv_XY <- conv %>%
  cbind(.,st_coordinates(conv))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

dist_conv <- as.data.frame(nn_function(block_XY, conv_XY, 5)) %>% dplyr::rename(distance_conv = pointDistance)
dat_crime$distance_conv <- dist_conv$distance_conv

gym_XY <- gyms %>%
  cbind(.,st_coordinates(gyms)) %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(X,Y) %>% 
  as.matrix()

dist_gym <- as.data.frame(nn_function(block_XY, gym_XY, 5)) %>% dplyr::rename(distance_gym = pointDistance)
dat_crime$distance_gym <- dist_gym$distance_gym

institution_XY <- institutions %>%
  cbind(.,st_coordinates(institutions)) %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(X,Y) %>% 
  as.matrix()

dist_institution <- as.data.frame(nn_function(block_XY, institution_XY, 3)) %>% dplyr::rename(distance_institution = pointDistance)
dat_crime$distance_institution <- dist_institution$distance_institution

liquor_XY <- liquors %>% 
  cbind(.,st_coordinates(liquors))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

dist_liquor <- as.data.frame(nn_function(block_XY, liquor_XY, 3)) %>% dplyr::rename(distance_liquor = pointDistance)
dat_crime$distance_liquor <- dist_liquor$distance_liquor

lodge_XY <- lodgings %>% 
  cbind(.,st_coordinates(lodgings))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

dist_lodge <- as.data.frame(nn_function(block_XY, lodge_XY, 5)) %>% dplyr::rename(distance_lodge = pointDistance)
dat_crime$distance_lodge <- dist_lodge$distance_lodge

nightlife_XY <- nightlifes %>% 
  cbind(.,st_coordinates(nightlifes))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

dist_night <- as.data.frame(nn_function(block_XY, nightlife_XY, 3)) %>% dplyr::rename(distance_nightlife = pointDistance)
dat_crime$distance_nightlife <- dist_night$distance_nightlife

pharmacy_XY <- pharmacys %>% 
  cbind(.,st_coordinates(pharmacys))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

dist_pharmacy <- as.data.frame(nn_function(block_XY, pharmacy_XY, 5)) %>% dplyr::rename(distance_pharmacy = pointDistance)
dat_crime$distance_pharmacy <- dist_pharmacy$distance_pharmacy

restaurant_XY <- restaurants %>% 
  cbind(.,st_coordinates(restaurants))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

dist_restaurant <- as.data.frame(nn_function(block_XY, restaurant_XY, 5)) %>% dplyr::rename(distance_restaurant = pointDistance)
dat_crime$distance_restaurant <- dist_restaurant$distance_restaurant

retail_XY <- retails %>% 
  cbind(.,st_coordinates(retails))  %>%
  st_set_geometry(NULL) %>%
  dplyr::select(X,Y) %>%
  as.matrix()

dist_retail <- as.data.frame(nn_function(block_XY, retail_XY, 10)) %>% dplyr::rename(distance_retail = pointDistance)
dat_crime$distance_retail <- dist_retail$distance_retail

# VISUALIZE DISTANCE VARS

ggplot()+
  geom_sf(data = dat_crime, aes(fill = distance_retail), color = NA) +
  scale_fill_viridis() +
  labs(title = "Distance from centroid to 5 closest business") +
  mapTheme()

# Exploratory Plots:

ggplot(dat_crime) +
  geom_histogram(aes(x= crime_index, fill = as.factor(near_school)), bins = 100) +
  labs(title = "Histogram Crime Index - Target dataset", fill = "Near School") +
  xlab("crime per area") +
  ylab("Counts")+
  plotTheme()

ggplot(dat_crime) +
  geom_histogram(aes(x = log(crime_index), fill = as.factor(near_school)), bins = 100) +
  labs(title = "Histogram Logged Crime Index - Target dataset", fill = "Near School") +
  xlab("log of crime per area") +
  ylab("Counts")+
  plotTheme()

# family_in_pov
ggplot(dat_crime, aes(x = family_in_pov, y = log(crime_index), color = as.factor(near_school))) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplot(dat_crime, aes(x = log(family_in_pov), y = log(crime_index), color = as.factor(near_school))) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

remove(abandoned_vehicle, acs_2017, bad_street, blocks_with_school, census_2010, crime_incidence,
       crime_per_block, crime_sf, dat, dumping, graffiti, infestation, land_use, litter_index, target_crime_incidence, 
       philly_blocks, request_311, schools, schools_buffer, street_light_out, summary_litter, summary_violations,
       varlist_2010, varlist_2017, violations, violations_per_block, acs2017_vars, census2010_vars, philly_tracts, tracts_LU)

remove(block_XY, blocks_centroids, business, business_per_block, business_sf, business_summ, cafe_XY, cafes, conv, conv_XY,
       dist_cafe, dist_conv, dist_gym, dist_institution, dist_liquor, dist_lodge, dist_night, dist_pharmacy, dist_restaurant,
       dist_retail, gym_XY, gyms, institution_XY, institutions, liquor_XY, liquors, lodgings, lodge_XY, nightlife_XY, nightlifes,
       pharmacy_XY, pharmacys, restaurant_XY, restaurants, retail_XY, retails)

#### Visualize max dist frequency ####

ggplot(dat_crime) +
  geom_histogram(aes(x = distance_cafe), bins = 100) +
  labs(title = "Histogram max distance to 50 closet cafes", fill = "Near School") +
  ylab("")+
  xlab("")+
  plotTheme()

ggplot(dat_crime) +
  geom_histogram(aes(x = distance_conv), bins = 100) +
  labs(title = "Histogram max distance to 20 closet convenience stores", fill = "Near School") +
  ylab("")+
  xlab("")+
  plotTheme()

ggplot(dat_crime) +
  geom_histogram(aes(x = distance_gym), bins = 100) +
  labs(title = "Histogram max distance to 50 closet gym", fill = "Near School") +
  ylab("")+
  xlab("")+
  plotTheme()

ggplot(dat_crime) +
  geom_histogram(aes(x = distance_institution), bins = 100) +
  labs(title = "Histogram max distance to 5 closet institutions", fill = "Near School") +
  ylab("")+
  xlab("")+
  plotTheme()

ggplot(dat_crime) +
  geom_histogram(aes(x = distance_liquor), bins = 100) +
  labs(title = "Histogram max distance to 50 closet liquor shops", fill = "Near School") +
  ylab("")+
  xlab("")+
  plotTheme()

ggplot(dat_crime) +
  geom_histogram(aes(x = distance_lodge), bins = 100) +
  labs(title = "Histogram max distance to 50 closet lodging facilities", fill = "Near School") +
  ylab("")+
  xlab("")+
  plotTheme()

ggplot(dat_crime) +
  geom_histogram(aes(x = distance_nightlife), bins = 100) +
  labs(title = "Histogram max distance to closet nightlife place", fill = "Near School") +
  ylab("")+
  xlab("")+
  plotTheme()

ggplot(dat_crime) +
  geom_histogram(aes(x = distance_pharmacy), bins = 100) +
  labs(title = "Histogram max distance to 10 closet pharmacies", fill = "Near School") +
  ylab("")+
  xlab("")+
  plotTheme()

ggplot(dat_crime) +
  geom_histogram(aes(x = distance_restaurant), bins = 100) +
  labs(title = "Histogram max distance to 2 closet restaurants", fill = "Near School") +
  ylab("")+
  xlab("")+
  plotTheme()

ggplot(dat_crime) +
  geom_histogram(aes(x = distance_retail), bins = 100) +
  labs(title = "Histogram max distance to 8 closet retail stores", fill = "Near School") +
  ylab("")+
  xlab("")+
  plotTheme()

#### Regressions ####

dat_final <- dat_crime %>%
  st_set_geometry(NULL) %>% 
  as_tibble()

dat_final[is.na(dat_final)] <- 0

save.image("~/UPENN/Box Sync/WISE FELLOWSHIP FALL/FINAL_R_CODE/Environments/Environment_Target_Dataset.RData")
