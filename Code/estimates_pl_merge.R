library(tidyverse)
library(tidycensus)

# Set environmental variables

options(scipen = 999) # No scientific notation

readRenviron("~/.Renviron") # Reload .Renviron file cleanly

# Requires you to have your api key saved as CENSUS_API_KEY in your .Renviron file
census_api_key(key = Sys.getenv("CENSUS_API_KEY"))

#### 2020 Census Counts frame ------------------------------

### read in PL data
pl <- read_csv("Raw/county_pl.csv") %>%
  
  ## select only the needed vars
  select(GEOID = fips, 
         CEN_GQ_TOT = gq_total, # total group quarters
         CEN_HU_TOT = hu_total, # total housing units
         CEN_HU_VACANT_TOT = hu_vacant, # vacant housing units
         CEN_POP_TOT = total_all_all, # total population
         CEN_NH_WA_TOT = white_all_all, # total White Non-Hispanic population
         CEN_NH_BA_TOT = black_all_all, # total Black Non-Hispanic population
         CEN_NH_AI_TOT = aian_all_all, # total AIAN population
         CEN_NH_AA_TOT = asian_all_all, # total Asian population
         CEN_NH_NA_TOT = nhpi_all_all, # total NH/PI population
         CEN_NH_TOM_TOT = twoplus_all_all, # total 2 plus races
         CEN_H_TOT_TOT = all_hisp_all,
         CEN_POP_18P = total_all_18P, # total 18 plus population
         CEN_NH_WA_18P = white_all_18P, # 18P White Non-Hispanic population
         CEN_NH_BA_18P = black_all_18P, # 18P Black Non-Hispanic population
         CEN_NH_AI_18P = aian_all_18P, # 18P AIAN population
         CEN_NH_AA_18P = asian_all_18P, # 18P Asian population
         CEN_NH_NA_18P = nhpi_all_18P, # 18P NH/PI population
         CEN_NH_TOM_18P = twoplus_all_18P, # 18P 2 plus races
         CEN_H_TOT_18P = all_hisp_18P, # 18P Hispanic population
         CEN_POP_17U = total_all_17U, # total 17U population
         CEN_NH_WA_17U = white_all_17U, # 17U White Non-Hispanic population
         CEN_NH_BA_17U = black_all_17U, # 17U Black Non-Hispanic population
         CEN_NH_AI_17U = aian_all_17U, # 17U AIAN population
         CEN_NH_AA_17U = asian_all_17U, # 17U Asian population
         CEN_NH_NA_17U = nhpi_all_17U, # 17U NH/PI population
         CEN_NH_TOM_17U = twoplus_all_17U, # 17U 2 plus races
         CEN_H_TOT_17U = all_hisp_17U) # 17U Hispanic population
         

### combine Chugach and Copper River Census Areas in AK to create Valdez-Cordova Census Area, pre-2020 geography, so ACS data can be joined
pl_ak <- pl %>%
  filter(GEOID == "02066" | GEOID == "02063") %>% 
  summarise_if(is.numeric, sum) 

pl_ak$GEOID <- "02261"

pl <- rbind(pl, pl_ak)


#### 2020 Census Estimates frame ------------------------------

# Read in the 2020 estimates data
est <- read_csv("Raw/estimates.csv")


#### Merge the PL data and the estimates data frames ------------------------------
pl_est_merge <- left_join(pl, est, by = "GEOID") %>%
  
  ## fix DC state name
  mutate(CTYNAME = case_when(CTYNAME == "District of Columbia" ~ "Washington",
                            TRUE ~ CTYNAME),
         
         # create a households unit by taking the total population and removing group quarters
         CEN_HH_TOT = CEN_POP_TOT - CEN_GQ_TOT,
         EST_HH_TOT = EST_POP_TOT - EST_GQ_TOT,
         
         # create a persons per household variable by dividing households by housing units
         CEN_PPH_TOT = CEN_HH_TOT / CEN_HU_TOT,
         EST_PPH_TOT = EST_HH_TOT / EST_HU_TOT,
         
         # calculate raw difference between estimate and actual count
         POP_DIFF = CEN_POP_TOT - EST_POP_TOT, # total population difference
         P18P_DIFF = CEN_POP_18P - EST_TOT_TOT_18P, # 18P population difference
         P17U_DIFF = CEN_POP_17U - EST_TOT_TOT_17U, # 17U population difference
         GQ_DIFF = CEN_GQ_TOT - EST_GQ_TOT, # group quarters difference
         HU_DIFF = CEN_HU_TOT - EST_HU_TOT, # housing units difference
         NH_BA_DIFF = CEN_NH_BA_TOT - EST_NH_BA_TOT, # Black Non-Hispanic difference
         NH_WA_DIFF = CEN_NH_WA_TOT - EST_NH_WA_TOT, # White Non-Hispanic difference
         NH_AI_DIFF = CEN_NH_AI_TOT - EST_NH_AI_TOT, # AIAN Non-Hispanic difference
         NH_AA_DIFF = CEN_NH_AA_TOT - EST_NH_AA_TOT, # Asian Non-Hispanic difference
         NH_NA_DIFF = CEN_NH_NA_TOT - EST_NH_NA_TOT, # NHPI Non-Hispanic difference
         NH_TOM_DIFF = CEN_NH_TOM_TOT - EST_NH_TOM_TOT, #Two or more races Non-Hispanic difference
         H_DIFF = CEN_H_TOT_TOT - EST_H_TOT_TOT, # Hispanic difference
         NH_BA_18P_DIFF = CEN_NH_BA_18P - EST_NH_BA_18P, # 18P Black Non-Hispanic difference
         NH_WA_18P_DIFF = CEN_NH_WA_18P - EST_NH_WA_18P, # 18P White Non-Hispanic difference
         NH_AI_18P_DIFF = CEN_NH_AI_18P - EST_NH_AI_18P, # 18P AIAN Non-Hispanic difference
         NH_AA_18P_DIFF = CEN_NH_AA_18P - EST_NH_AA_18P, # 18P Asian Non-Hispanic difference
         NH_NA_18P_DIFF = CEN_NH_NA_18P - EST_NH_NA_18P, # 18P NHPI Non-Hispanic difference
         NH_TOM_18P_DIFF = CEN_NH_TOM_18P - EST_NH_TOM_18P, # 18P Two or more races Non-Hispanic difference
         H_18P_DIFF = CEN_H_TOT_18P - EST_H_TOT_18P, # 18P Hispanic difference
         NH_BA_17U_DIFF = CEN_NH_BA_17U - EST_NH_BA_17U, # 17U Black Non-Hispanic difference
         NH_WA_17U_DIFF = CEN_NH_WA_17U - EST_NH_WA_17U, # 17U White Non-Hispanic difference
         NH_AI_17U_DIFF = CEN_NH_AI_17U - EST_NH_AI_17U, # 17U AIAN Non-Hispanic difference
         NH_AA_17U_DIFF = CEN_NH_AA_17U - EST_NH_AA_17U, # 17U Asian Non-Hispanic difference
         NH_NA_17U_DIFF = CEN_NH_NA_17U - EST_NH_NA_17U, # 17U NHPI Non-Hispanic difference
         NH_TOM_17U_DIFF = CEN_NH_TOM_17U - EST_NH_TOM_17U, # 17U Two or more races Non-Hispanic difference
         H_17U_DIFF = CEN_H_TOT_17U - EST_H_TOT_17U, # 17U Hispanic difference
         PPH_DIFF = CEN_PPH_TOT - EST_PPH_TOT, # Persons per Household difference
         
         # If the population difference is zero, keep it at zero. Otherwise, calculate the share difference by taking the difference and dividing it by the total count
         SHARE_POP_DIFF = ifelse(POP_DIFF == 0, 0, (POP_DIFF/EST_POP_TOT)), 
         SHARE_GQ_DIFF = ifelse(GQ_DIFF == 0, 0, (GQ_DIFF/EST_GQ_TOT)), 
         SHARE_HU_DIFF = ifelse(HU_DIFF == 0, 0, (HU_DIFF/EST_HU_TOT)),
         SHARE_NH_BA_DIFF = ifelse((NH_BA_DIFF == 0 | CEN_NH_BA_TOT == 0), 0, (NH_BA_DIFF/EST_NH_BA_TOT)),
         SHARE_NH_WA_DIFF = ifelse((NH_WA_DIFF == 0 | CEN_NH_WA_TOT == 0), 0, (NH_WA_DIFF/EST_NH_WA_TOT)),
         SHARE_NH_AI_DIFF = ifelse((NH_AI_DIFF == 0 | CEN_NH_AI_TOT == 0), 0, (NH_AI_DIFF/EST_NH_AI_TOT)),
         SHARE_NH_AA_DIFF = ifelse((NH_AA_DIFF == 0 | CEN_NH_AA_TOT == 0), 0, (NH_AA_DIFF/EST_NH_AA_TOT)),
         SHARE_NH_NA_DIFF = ifelse((NH_NA_DIFF == 0 | CEN_NH_NA_TOT == 0), 0, (NH_NA_DIFF/EST_NH_NA_TOT)),
         SHARE_NH_TOM_DIFF = ifelse((NH_TOM_DIFF == 0 | CEN_NH_TOM_TOT == 0), 0, (NH_TOM_DIFF/EST_NH_TOM_TOT)),
         SHARE_H_DIFF = ifelse(H_DIFF == 0, 0, (H_DIFF/EST_H_TOT_TOT)),
         SHARE_NH_BA_18P_DIFF = ifelse((NH_BA_18P_DIFF == 0 | CEN_NH_BA_18P == 0), 0, (NH_BA_18P_DIFF/EST_NH_BA_18P)),
         SHARE_NH_WA_18P_DIFF = ifelse((NH_WA_18P_DIFF == 0 | CEN_NH_WA_18P == 0), 0, (NH_WA_18P_DIFF/EST_NH_WA_18P)),
         SHARE_NH_AI_18P_DIFF = ifelse((NH_AI_18P_DIFF == 0 | CEN_NH_AI_18P == 0), 0, (NH_AI_18P_DIFF/EST_NH_AI_18P)),
         SHARE_NH_AA_18P_DIFF = ifelse((NH_AA_18P_DIFF == 0 | CEN_NH_AA_18P == 0), 0, (NH_AA_18P_DIFF/EST_NH_AA_18P)),
         SHARE_NH_NA_18P_DIFF = ifelse((NH_NA_18P_DIFF == 0 | CEN_NH_NA_18P == 0), 0, (NH_NA_18P_DIFF/EST_NH_NA_18P)),
         SHARE_NH_TOM_18P_DIFF = ifelse((NH_TOM_18P_DIFF == 0 | CEN_NH_TOM_18P == 0), 0, (NH_TOM_18P_DIFF/EST_NH_TOM_18P)),
         SHARE_H_18P_DIFF = ifelse((H_18P_DIFF == 0 | CEN_H_TOT_18P == 0), 0, (H_18P_DIFF/EST_H_TOT_18P)),
         SHARE_NH_BA_17U_DIFF = ifelse((NH_BA_17U_DIFF == 0 | CEN_NH_BA_17U == 0), 0, (NH_BA_17U_DIFF/EST_NH_BA_17U)),
         SHARE_NH_WA_17U_DIFF = ifelse((NH_WA_17U_DIFF == 0 | CEN_NH_WA_17U == 0), 0, (NH_WA_17U_DIFF/EST_NH_WA_17U)),
         SHARE_NH_AI_17U_DIFF = ifelse((NH_AI_17U_DIFF == 0 | CEN_NH_AI_17U == 0), 0, (NH_AI_17U_DIFF/EST_NH_AI_17U)),
         SHARE_NH_AA_17U_DIFF = ifelse((NH_AA_17U_DIFF == 0 | CEN_NH_AA_17U == 0), 0, (NH_AA_17U_DIFF/EST_NH_AA_17U)),
         SHARE_NH_NA_17U_DIFF = ifelse((NH_NA_17U_DIFF == 0 | CEN_NH_NA_17U == 0), 0, (NH_NA_17U_DIFF/EST_NH_NA_17U)),
         SHARE_NH_TOM_17U_DIFF = ifelse((NH_TOM_17U_DIFF == 0 | CEN_NH_TOM_17U == 0), 0, (NH_TOM_17U_DIFF/EST_NH_TOM_17U)),
         SHARE_H_17U_DIFF = ifelse((H_17U_DIFF == 0 | CEN_H_TOT_17U == 0), 0, (H_17U_DIFF/EST_H_TOT_17U)),
         SHARE_PPH_DIFF = ifelse(PPH_DIFF == 0, 0, (PPH_DIFF/EST_PPH_TOT)),
         
         # Create percent vacant HU for vacancy calculations later on
         PCT_VACANT_HU_2020 = (CEN_HU_VACANT_TOT/CEN_HU_TOT)*100, 
         
         # Add a notes field for AK geography
         NOTES = case_when(GEOID == "02261" ~ "In 2019, this census area was split into Copper River and Chugach census areas. These two census areas were combined to create the measures for the Valdez-Cordova Census Area. The measures for the 2020 geographies are available in the raw data on GitHub.", 
                           TRUE ~ "")) 


#### 2010 Divergence frame ------------------------------

### Read in the 2010 divergence cutpoints
div_2010 <- read_csv("Raw/cutpoints.csv")

### Create divergence measures
divergence <- pl_est_merge %>%
  
  ## Select only needed variables
  select(GEOID, 
         EST_NH_AI_TOT,
         CEN_NH_AI_TOT,
         EST_NH_AI_18P,
         CEN_NH_AI_18P,
         EST_NH_AI_17U,
         CEN_NH_AI_17U,
         EST_NH_BA_TOT,
         CEN_NH_BA_TOT,
         EST_NH_BA_18P,
         CEN_NH_BA_18P,
         EST_NH_BA_17U,
         CEN_NH_BA_17U,
         EST_GQ_TOT,
         CEN_GQ_TOT,
         EST_H_TOT_TOT,
         CEN_H_TOT_TOT,
         EST_H_TOT_18P,
         CEN_H_TOT_18P,
         EST_H_TOT_17U,
         CEN_H_TOT_17U,
         EST_HU_TOT,
         CEN_HU_TOT,
         EST_POP_TOT,
         CEN_POP_TOT,
         EST_TOT_TOT_18P,
         CEN_POP_18P,
         EST_TOT_TOT_17U,
         CEN_POP_17U,
         EST_NH_WA_TOT,
         CEN_NH_WA_TOT,
         EST_NH_WA_18P,
         CEN_NH_WA_18P,
         EST_NH_WA_17U,
         CEN_NH_WA_17U,
         EST_NH_AA_TOT,
         CEN_NH_AA_TOT,
         EST_NH_AA_18P,
         CEN_NH_AA_18P,
         EST_NH_AA_17U,
         CEN_NH_AA_17U,
         EST_NH_NA_TOT,
         CEN_NH_NA_TOT,
         EST_NH_NA_18P,
         CEN_NH_NA_18P,
         EST_NH_NA_17U,
         CEN_NH_NA_17U,
         EST_NH_TOM_TOT,
         CEN_NH_TOM_TOT,
         EST_NH_TOM_18P,
         CEN_NH_TOM_18P,
         EST_NH_TOM_17U,
         CEN_NH_TOM_17U) %>%
  
  ## Calculate variables needed for grading (proportions, PPH, Size class of geography)
  mutate(EST_NH_WA_PROP = EST_NH_WA_TOT / EST_POP_TOT,
         EST_NH_BA_PROP = EST_NH_BA_TOT / EST_POP_TOT,
         EST_NH_AI_PROP = EST_NH_AI_TOT / EST_POP_TOT,
         EST_NH_AA_PROP = EST_NH_AA_TOT / EST_POP_TOT,
         EST_NH_NA_PROP = EST_NH_NA_TOT / EST_POP_TOT,
         EST_NH_TOM_PROP = EST_NH_TOM_TOT / EST_POP_TOT,
         EST_H_TOT_PROP = EST_H_TOT_TOT / EST_POP_TOT,
         EST_NH_WA_18P_PROP = EST_NH_WA_18P / EST_TOT_TOT_18P,
         EST_NH_BA_18P_PROP = EST_NH_BA_18P / EST_TOT_TOT_18P,
         EST_NH_AI_18P_PROP = EST_NH_AI_18P / EST_TOT_TOT_18P,
         EST_NH_AA_18P_PROP = EST_NH_AA_18P / EST_TOT_TOT_18P,
         EST_NH_NA_18P_PROP = EST_NH_NA_18P / EST_TOT_TOT_18P,
         EST_NH_TOM_18P_PROP = EST_NH_TOM_18P / EST_TOT_TOT_18P,
         EST_H_TOT_18P_PROP = EST_H_TOT_18P / EST_TOT_TOT_18P,
         EST_NH_WA_17U_PROP = EST_NH_WA_17U / EST_TOT_TOT_17U,
         EST_NH_BA_17U_PROP = EST_NH_BA_17U / EST_TOT_TOT_17U,
         EST_NH_AI_17U_PROP = EST_NH_AI_17U / EST_TOT_TOT_17U,
         EST_NH_AA_17U_PROP = EST_NH_AA_17U / EST_TOT_TOT_17U,
         EST_NH_NA_17U_PROP = EST_NH_NA_17U / EST_TOT_TOT_17U,
         EST_NH_TOM_17U_PROP = EST_NH_TOM_17U / EST_TOT_TOT_17U,
         EST_H_TOT_17U_PROP = EST_H_TOT_17U / EST_TOT_TOT_17U,
         EST_HH_TOT = EST_POP_TOT - EST_GQ_TOT,
         EST_PPH_RAT = EST_HH_TOT / EST_HU_TOT,
         CEN_NH_WA_PROP = CEN_NH_WA_TOT / CEN_POP_TOT,
         CEN_NH_BA_PROP = CEN_NH_BA_TOT / CEN_POP_TOT,
         CEN_NH_AI_PROP = CEN_NH_AI_TOT / CEN_POP_TOT,
         CEN_NH_AA_PROP = CEN_NH_AA_TOT / CEN_POP_TOT,
         CEN_NH_NA_PROP = CEN_NH_NA_TOT / CEN_POP_TOT,
         CEN_NH_TOM_PROP = CEN_NH_TOM_TOT / CEN_POP_TOT,
         CEN_H_TOT_PROP = CEN_H_TOT_TOT / CEN_POP_TOT,
         CEN_NH_WA_18P_PROP = CEN_NH_WA_18P / CEN_POP_18P,
         CEN_NH_BA_18P_PROP = CEN_NH_BA_18P / CEN_POP_18P,
         CEN_NH_AI_18P_PROP = CEN_NH_AI_18P / CEN_POP_18P,
         CEN_NH_AA_18P_PROP = CEN_NH_AA_18P / CEN_POP_18P,
         CEN_NH_NA_18P_PROP = CEN_NH_NA_18P / CEN_POP_18P,
         CEN_NH_TOM_18P_PROP = CEN_NH_TOM_18P / CEN_POP_18P,
         CEN_H_TOT_18P_PROP = CEN_H_TOT_18P / CEN_POP_18P,
         CEN_NH_WA_17U_PROP = CEN_NH_WA_17U / CEN_POP_17U,
         CEN_NH_BA_17U_PROP = CEN_NH_BA_17U / CEN_POP_17U,
         CEN_NH_AI_17U_PROP = CEN_NH_AI_17U / CEN_POP_17U,
         CEN_NH_AA_17U_PROP = CEN_NH_AA_17U / CEN_POP_17U,
         CEN_NH_NA_17U_PROP = CEN_NH_NA_17U / CEN_POP_17U,
         CEN_NH_TOM_17U_PROP = CEN_NH_TOM_17U / CEN_POP_17U,
         CEN_H_TOT_17U_PROP = CEN_H_TOT_17U / CEN_POP_17U,
         CEN_HH_TOT = CEN_POP_TOT - CEN_GQ_TOT,
         CEN_PPH_RAT = CEN_HH_TOT / CEN_HU_TOT,
         SIZE_CLASS = case_when(CEN_POP_TOT >= 100000 ~ "Large",
                                CEN_POP_TOT >= 40000  ~ "Medium",
                                CEN_POP_TOT <  40000  ~ "Small")) %>%
  
  ## Make some changes to data structure to make calculations easier
  pivot_longer(cols = c(2:99), names_to = "variable", values_to = "value") %>%
  separate(col = variable, into = c("source", "variable"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = source, values_from = value) %>%
  filter(!(variable %in% c("NH_WA_TOT", "NH_BA_TOT", "NH_AI_TOT", "NH_AA_TOT", "NH_NA_TOT", "NH_TOM_TOT", "H_TOT_TOT",
                           "NH_WA_18P", "NH_BA_18P", "NH_AI_18P", "NH_AA_18P", "NH_NA_18P", "NH_TOM_18P", "H_TOT_18P",
                           "NH_WA_17U", "NH_BA_17U", "NH_AI_17U", "NH_AA_17U", "NH_NA_17U", "NH_TOM_17U", "H_TOT_17U"))) %>% # No need for totals anymore
  
  ## Calculate differences for divergence measures. Use percent diff for totals and PP diff for race cats / PPH
  mutate(difference_count = CEN - EST,
         difference_share = ifelse(difference_count == 0, 0, difference_count / ((CEN+EST/2))),
         difference = ifelse(variable %in% c("POP_TOT", "POP_18P", "POP_17U", "HU_TOT", "GQ_TOT"), difference_share, difference_count)) %>%
  filter(!(variable == "HH_TOT")) %>%
  
  ## Merge in divergence from 2010
  full_join(div_2010, by = c("variable", "SIZE_CLASS")) %>%
  
  ## Create divergence measure
  mutate(divergence_measure = case_when((difference >= range_2_high | difference <= range_2_low) ~ "Highly Divergent from Expectation",
                                        (difference >= range_1_high | difference <= range_1_low) ~ "Slightly Divergent from Expectation",
                                        (difference < range_1_high & difference > range_1_low) ~ "Close to Expectation")) %>%
  
  ## Clean up file for merging
  select(GEOID, SIZE_CLASS, variable, divergence_measure) %>%
  pivot_wider(names_from = variable, values_from = divergence_measure) %>%

  ## Rename to match needed inputs
  rename(POP_MEASURE = POP_TOT,
         POP_18P_MEASURE = POP_18P,
         POP_17U_MEASURE = POP_17U,
         GQ_MEASURE = GQ_TOT,
         HU_MEASURE = HU_TOT,
         PPH_MEASURE = PPH_RAT,
         NH_BA_MEASURE = NH_BA_PROP,
         NH_WA_MEASURE = NH_WA_PROP,
         NH_AI_MEASURE = NH_AI_PROP,
         NH_AA_MEASURE = NH_AA_PROP,
         NH_NA_MEASURE = NH_NA_PROP,
         NH_TOM_MEASURE = NH_TOM_PROP,
         H_MEASURE = H_TOT_PROP,
         NH_BA_18P_MEASURE = NH_BA_18P_PROP,
         NH_WA_18P_MEASURE = NH_WA_18P_PROP,
         NH_AI_18P_MEASURE = NH_AI_18P_PROP,
         NH_AA_18P_MEASURE = NH_AA_18P_PROP,
         NH_NA_18P_MEASURE = NH_NA_18P_PROP,
         NH_TOM_18P_MEASURE = NH_TOM_18P_PROP,
         H_18P_MEASURE = H_TOT_18P_PROP,
         NH_BA_17U_MEASURE = NH_BA_17U_PROP,
         NH_WA_17U_MEASURE = NH_WA_17U_PROP,
         NH_AI_17U_MEASURE = NH_AI_17U_PROP,
         NH_AA_17U_MEASURE = NH_AA_17U_PROP,
         NH_NA_17U_MEASURE = NH_NA_17U_PROP,
         NH_TOM_17U_MEASURE = NH_TOM_17U_PROP,
         H_17U_MEASURE = H_TOT_17U_PROP)


#### 2010 Vacancy Data frame ------------------------------

### Read in the 2010 vacancy data using tidycensus
vac_2010 <- get_decennial(geography = "county", 
                          variables = c(HU_TOT = "H003001", 
                                        HU_VAC = "H003003"),
                          year = 2010) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  filter(!str_detect(GEOID, "^72")) %>%
  mutate(GEOID = case_when(GEOID == "02270" ~ "02158",
                           GEOID == "46113" ~ "46102", 
                           GEOID == "51515" ~ "51019",
                           TRUE ~ GEOID)) %>%
  group_by(GEOID) %>%
  summarize(GEOID, 
            HU_VAC = sum(HU_VAC),
            HU_TOT = sum(HU_TOT)) %>%
  mutate(PCT_VACANT_HU_2010 = (HU_VAC/HU_TOT)*100) %>%
  unique()
  

divergence_vacancy <- vac_2010 %>%
  full_join(pl_est_merge, by = c("GEOID")) %>%
  select(GEOID, PCT_VACANT_HU_2010, PCT_VACANT_HU_2020) %>%
  mutate(PCT_VACANT_CHANGE = PCT_VACANT_HU_2020 - PCT_VACANT_HU_2010,
         VACANCY_MEASURE = case_when(abs(PCT_VACANT_CHANGE) >= 5 ~ "Highly Divergent from Expectation",
                                 abs(PCT_VACANT_CHANGE) >= 2.5  ~ "Slightly Divergent from Expectation",
                                 abs(PCT_VACANT_CHANGE) < 2.5   ~ "Close to Expectation")) %>%
  select(GEOID, VACANCY_MEASURE, PCT_VACANT_HU_2010, PCT_VACANT_CHANGE)



#### Merge the 2020 counts/estimates data, divergence, and 2010 vacancy data frames --------

### Merge the counts/estimates data with divergence and vacancy date
final_frame <- pl_est_merge %>%
  full_join(divergence, by = c("GEOID")) %>% # with divergence data
  full_join(divergence_vacancy, by = c("GEOID")) %>% # with vacancy data
  
  ## Set up output in the desired order
  relocate(GEOID, STNAME, CTYNAME, GEOYEAR, SIZE_CLASS, 
           contains("CEN_"), contains("EST_"), contains("SHARE_"),
           contains("PCT_"), contains("_DIFF"))

### Save to output folder for ingestion in Tableau
write_csv(final_frame, "Output/county_main.csv")


