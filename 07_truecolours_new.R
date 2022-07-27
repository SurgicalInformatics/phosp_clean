# PHOSP-COVID analysis: Import True Colours data
## Centre for Medical Informatics, Usher Institute, University of Edinburgh 2021
## These are received via an encrypted data transfer and stored in the data folder. 

# Packages ----------------------------------------
library(tidyverse)
library(lubridate)


# Import data -------------------------------------
path = "/home/common/phosp/truecolours/tcphosp.20220511/"
import_files = list.files(path, full.names = TRUE)

new_tc = import_files %>% 
  map(~ read_csv(.) %>% 
        rename_with(~ paste0(.x, "_tc")) %>% # Add _tc to each variable name
        rename("phosp_id" = 1,#               # Bring back phosp_id and make new trucolours date common across all
               "date_tc" = 2) %>%
        mutate(
          date_tc = as.Date(as.POSIXct(ifelse(hour(date_tc) <= 4, date_tc - days(1), date_tc), origin = "1970-01-01")) # if input before 4am, set date as day before
          ) %>%
        distinct(phosp_id, date_tc, .keep_all = TRUE)
  )

# Join/collapse all tables --------------------------
phosp_tc = new_tc %>% 
  map( ~ select(., phosp_id, date_tc)) %>% 
  reduce(bind_rows) %>% # make full list of ids and dates
  distinct() %>% 
  list() %>% 
  append(new_tc) %>% # append to list of dataframes to use as tc backbone
  reduce(full_join, by = c("phosp_id", "date_tc")) %>% 
  arrange(phosp_id, date_tc)

