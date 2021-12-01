# PHOSP-Covid REDCap database analysis: API pull
# API pull from Leicester Oxford REDCap server
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# To use this, set your REDCap API token as an environment variable.
## Uncomment and run the following line:
# usethis::edit_r_environ()
## this opens up .Renviron, add your token, e.g. ccp_token = 2F3xxxxxxxxxxxxE0111
## Restart R

# 1. API pull
# 2. Apply REDCap R formatitng, file edited.
# 3. Final object created: ccp_data

# Libraries
library(RCurl)
library(tidyverse)
library(REDCapR)

# Functions for safe api pull
rate = rate_backoff(pause_cap = 60*5, max_times = 10)
insistent_postForm = purrr::insistently(postForm, rate)
insistent_redcap_read = purrr::insistently(redcap_read, rate)
batch = function(.vector, .n = 200){
  split(.vector, ceiling(seq_along(.vector)/.n))
}

# Get subjid
subjid = insistent_postForm(
  uri='https://data.phosp.org/api/',
  token = Sys.getenv("phosp_token"),
  content='record',
  'fields[0]'='study_id',
  format='csv',
  type='flat',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json'
) %>% 
  read_csv() %>% 
  distinct(study_id) %>% 
  pull(study_id)

# Get data in batches
data_raw = batch(subjid) %>% 
  map_df(~ insistent_redcap_read(
    redcap_uri = "https://data.phosp.org/api/",
    export_data_access_groups = TRUE,
    token = Sys.getenv("phosp_token"),
    records = .x,
    guess_type = FALSE)$data
  )

data = data_raw %>% 
  type_convert() %>% 
  as_tibble()

# Formating
## Update 11/05/2021
#source("/home/eharrison/phosp_clean/PHOSPCOVID19FU_R_2021-02-10_1805.r")
#source("/home/eharrison/phosp_clean/PHOSPCOVID19FU_R_2021-05-11_0708.r")
source("/home/eharrison/phosp_clean/PHOSPCOVID19FU_R_2021-06-30_1749.r")

# Out object and clean
phosp = data

# Remove empty columns
phosp = purrr::discard(phosp, ~all(is.na(.)))

rm(subjid, batch, rate, insistent_postForm, insistent_redcap_read)
