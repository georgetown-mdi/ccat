library(tidyverse)

# read in FEMA data
fema <- read_csv("Raw/FEMACY2020.csv") %>%
  mutate(declarationTitle = str_to_title(declarationTitle)) %>% # clean up names so they are in title case
  select(STNAME, State, CTYNAME, County, GEOID, declarationTitle, FEMAWEB, fema_id, IB_date, IE_date) # set up output schema

# write output to file
write_csv(fema, "Output/fema.csv")