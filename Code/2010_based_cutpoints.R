library(tidyverse)
library(tidycensus)

# Set environmental variables

options(scipen = 999) # No scientific notation

readRenviron("~/.Renviron") # Reload .Renviron file cleanly

# Requires you to have your api key saved as CENSUS_API_KEY in your .Renviron file
census_api_key(key = Sys.getenv("CENSUS_API_KEY"))

# Get variables for 2010 PL and SF1 and 2020 PL

vars_2010 <- load_variables(2010, "pl", cache = TRUE)
vars_2020 <- load_variables(2020, "pl", cache = TRUE)
vars_2010_sf1 <- load_variables(2010, "sf1", cache = TRUE)

# pull in evaluation estimates -- must download them because they are not on API
# all data downloaded from: 
# https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates.2010.html
# https://www2.census.gov/programs-surveys/popest/datasets/2010-2012/counties/asrh/

v2010_components <- read_csv("Raw/co-est2010-alldata.csv") #used for GQ only
v2010_characteristics <- read_csv("Raw/cc-est2010-alldata.csv") #used for characteristics and total
v2010_housing <- read_csv("Raw/hu-est2010.csv") #used for housing units
v2012_characteristics <- read_csv("Raw/cc-est2012-alldata.csv") #used for Census 2010 characteristics


##### Pull PL Data using tidycensus

# Create variable vectors for census to pull race by hispanic origin for total and 18+
# Must get GQ data from the SF1 as it is not in the PL data on the API

list_length <- c(1:73)

list_char <- ifelse(list_length < 10, paste0("0", as.character(list_length)), as.character(list_length))

vec_2010 <- c(paste0("P0020", list_char), paste0("P0040", list_char))

redist_2010 <- get_decennial(geography = "county", 
                             variables = vec_2010, 
                             year = 2010, 
                             sumfile = "pl")

sf1_2010 <- get_decennial(geography = "county",
                          variables = c(GQ_TOT = "P042001"),
                          year = 2010,
                          sumfile = "sf1")

hu_2010 <- get_decennial(geography = "county",
                         variables = c(HU_TOT = "H001001",
                                       HU_VAC = "H001003"),
                         year = 2010,
                         sumfile = "pl")


#### Trim estimates datasets to needed variables

# GQ Only -- they did not publish April, 2010 estimates so I have to do my own interpolation.
v2010_gq <- v2010_components %>%
  filter(SUMLEV == "050") %>%
  mutate(GEOID = paste0(STATE, COUNTY)) %>%
  select(GEOID, STNAME, CTYNAME, GQESTIMATES2010, GQESTIMATES2009) %>%
  mutate(GQ_TOT = round(GQESTIMATES2009 + ((GQESTIMATES2010 - GQESTIMATES2009) / 12) * 9))%>%
  select(GEOID, GQ_TOT)

# HU only
v2010_hu <- v2010_housing %>%
  filter(sumlev == "050") %>%
  mutate(GEOID = paste0(state, county)) %>%
  select(GEOID, huest_042010)

# create the final estimates file -- start by bringing in race and total pop for April, 2020
v2010_chars_tot <- v2010_characteristics %>%
  filter(YEAR == "13" & AGEGRP == "0") %>%
  mutate(POP_TOT = TOT_POP,
         NH_WA_TOT = NHWA_MALE + NHWA_FEMALE,
         NH_BA_TOT = NHBA_MALE + NHBA_FEMALE,
         NH_AI_TOT = NHIA_MALE + NHIA_FEMALE,
         NH_AA_TOT = NHAA_MALE + NHAA_FEMALE,
         NH_NA_TOT = NHNA_MALE + NHNA_FEMALE,
         NH_TOM_TOT = NHTOM_MALE + NHTOM_FEMALE,
         H_TOT_TOT = H_MALE + H_FEMALE,
         STATE = ifelse(STATE < 10, paste0("0", as.character(STATE)), as.character(STATE)),
         COUNTY = ifelse(COUNTY < 10, paste0("00", as.character(COUNTY)),
                         ifelse(COUNTY < 100, paste0("0", as.character(COUNTY)), as.character(COUNTY))),
         GEOID = paste0(STATE, COUNTY),
         NH_WA_PROP = NH_WA_TOT / POP_TOT,
         NH_BA_PROP = NH_BA_TOT / POP_TOT,
         NH_AI_PROP = NH_AI_TOT / POP_TOT,
         NH_AA_PROP = NH_AA_TOT / POP_TOT,
         NH_NA_PROP = NH_NA_TOT / POP_TOT,
         NH_TOM_PROP = NH_TOM_TOT / POP_TOT,
         H_TOT_PROP = H_TOT_TOT / POP_TOT) %>%
  select(-c(1, 2, 3, 6:80)) %>%
  relocate(GEOID) %>%
  
  #Join in HU data and rename
  full_join(v2010_hu, by = c("GEOID")) %>%
  rename(HU_TOT = huest_042010) %>%
  
  # Join in GQ data
  full_join(v2010_gq, by = c("GEOID")) %>%
  
  # Create a Household pop number so that we can create a proper persons per household
  mutate(HH_TOT = POP_TOT - GQ_TOT,
         PPH_RAT = HH_TOT / HU_TOT) %>%
  select(-HH_TOT) %>%
  
  # Create a long file for easier calculations later
  pivot_longer(cols = c(4:21), names_to = "variable", values_to = "est_2010")

### Trim census datasets to needed variables

# GQ only. Must come from the SF1 because GQ was not on the PL in 2010
gq_2010 <- sf1_2010 %>%
  mutate(st = str_sub(GEOID, 1, 2)) %>%
  filter(!(st == "72")) %>%
  select(-st, -NAME, -variable) %>%
  rename(GQ_TOT = value)

# Total population only not using 18 plus right now
totpop_2010 <- redist_2010 %>%
  filter(variable == "P002001") %>%
  mutate(st = str_sub(GEOID, 1, 2),
         variable = ifelse(variable == "P002001", "POP_TOT", "TOT_18P_POP")) %>%
  filter(!(st == "72")) %>%
  select(-st, -NAME, -variable) %>%
  rename(POP_TOT = value)

# Housing units (total and vacant)
hu_cen2010 <- hu_2010 %>%
  mutate(st = str_sub(GEOID, 1, 2)) %>%
  filter(!(st == "72")) %>%
  select(-st, -NAME) %>%
  pivot_wider(names_from = variable, values_from = value)

# Create final Census 2010 file for comparison to estimates
cen2010_chars_tot <- v2012_characteristics %>%
  
  filter(YEAR == "1" & AGEGRP == "0") %>%
  mutate(POP_TOT = TOT_POP,
         NH_WA_TOT = NHWA_MALE + NHWA_FEMALE,
         NH_BA_TOT = NHBA_MALE + NHBA_FEMALE,
         NH_AI_TOT = NHIA_MALE + NHIA_FEMALE,
         NH_AA_TOT = NHAA_MALE + NHAA_FEMALE,
         NH_NA_TOT = NHNA_MALE + NHNA_FEMALE,
         NH_TOM_TOT = NHTOM_MALE + NHTOM_FEMALE,
         H_TOT_TOT = H_MALE + H_FEMALE,
         GEOID = paste0(STATE, COUNTY),
         NH_WA_PROP = NH_WA_TOT / POP_TOT,
         NH_BA_PROP = NH_BA_TOT / POP_TOT,
         NH_AI_PROP = NH_AI_TOT / POP_TOT,
         NH_AA_PROP = NH_AA_TOT / POP_TOT,
         NH_NA_PROP = NH_NA_TOT / POP_TOT,
         NH_TOM_PROP = NH_TOM_TOT / POP_TOT,
         H_TOT_PROP = H_TOT_TOT / POP_TOT) %>%
  select(-c(1, 2, 3, 6:80)) %>%
  relocate(GEOID) %>%
  
  # Join in total gq and hu data
  full_join(gq_2010, by = c("GEOID")) %>%
  full_join(hu_cen2010, by = c("GEOID")) %>%
  
  # calculate proportions for later use as well as coding size of county
  mutate(HH_TOT = POP_TOT - GQ_TOT,
         PPH_RAT = HH_TOT / HU_TOT,
         SIZE_CLASS = case_when(POP_TOT >= 100000 ~ "Large",
                                POP_TOT >= 40000  ~ "Medium",
                                POP_TOT <  40000  ~ "Small")) %>%
  select(-HH_TOT, -HU_VAC) %>%
  
  # Make a longer file so it is easier to calculate differences
  pivot_longer(cols = c(4:21), names_to = "variable", values_to = "cen_2010")

### Comparison file

# Create our final grades for everything but vacancy

final_output <- v2010_chars_tot %>%
  full_join(cen2010_chars_tot, by = c("GEOID", "STNAME", "CTYNAME", "variable")) %>%
  
  # Not currently grading by race/ethnicity pop totals, but instead shares
  filter(!(variable %in% c("NH_WA_TOT", "NH_BA_TOT", "NH_AI_TOT", "NH_AA_TOT", "NH_NA_TOT", "NH_TOM_TOT", "H_TOT_TOT"))) %>%
  
  # Calculate the differences we care about (numeric or percentage) depending on variable
  mutate(difference = ifelse(variable %in% c("POP_TOT", "HU_TOT", "GQ_TOT"), 
                             (cen_2010 - est_2010) / est_2010, 
                             cen_2010 - est_2010),
         difference = ifelse(is.na(difference) | is.infinite(difference), 0, difference)) %>%
  
  # Creating grading cells by VARIABLE and SIZE of county (based on DHC suggestions)
  group_by(variable, SIZE_CLASS) %>%
  
  # Create our measurement groups (over/under 1 and 2 standard deviations of each measure)
  summarize(count = n(),
            mean = mean(difference),
            sd = sd(difference),
            range_1_high = mean + sd,
            range_1_low = mean - sd,
            range_2_high = mean + 2*sd,
            range_2_low = mean - 2*sd)

write_csv(final_output, "Raw/cutpoints.csv")

## Calculate and output vacancy rate

final_vacancy <- hu_cen2010 %>%
  
  #deal with geography changes between 2010 and 2020
  mutate(GEOID = ifelse(GEOID == "51515", "51019",
                        ifelse(GEOID == "46113", "46102",
                               ifelse(GEOID == "02270", "02158", GEOID)))) %>%
  
  # Summarize because Bedford City, VA was subsumed by Bedford County
  group_by(GEOID) %>%
  summarize(HU_VAC = sum(HU_VAC),
            HU_TOT = sum(HU_TOT)) %>%
  
  # Calculate Vacancy percent
  mutate(cen2010_pct_vacant_hu = (HU_VAC / HU_TOT) *100) %>%
  select(GEOID, cen2010_pct_vacant_hu)

# Fixing for Alaska changes
ak_fix1 <- final_vacancy %>%
  filter(GEOID == "02261") %>%
  mutate(GEOID = "02066")

ak_fix2 <- final_vacancy %>%
  filter(GEOID == "02261") %>%
  mutate(GEOID = "02063")


# Create final file with AK changes
final_vacancy <- final_vacancy %>%
  bind_rows(ak_fix1) %>%
  bind_rows(ak_fix2)

write_csv(final_vacancy, "Raw/cen2010_vacancy.csv")
