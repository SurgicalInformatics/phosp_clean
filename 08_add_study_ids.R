# PHOSP-COVID analysis: Add cluster and predict new patients
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022

library(tidyverse)

# Add other identifiers
phosp_3m_paper_study_id_1077 = readRDS("/home/common/phosp/share/phosp_3m_paper_study_id_1077.rds")
phosp_12m_paper_3m_study_id_2320 = readRDS("/home/common/phosp/share/phosp_12m_paper_3m_study_id_2320.rds")
phosp_12m_paper_12m_study_id_924 =   readRDS("/home/common/phosp/share/phosp_12m_paper_12m_study_id_924.rds")

# Define study_id:redcap_event_name pairs for minimum dataset.
minimum_dataset_3m_12m = phosp %>% 
  filter(tier == 2) %>%
  mutate(discharge_to_review = (crf3b_date_visit - crf1a_date_dis) %>% as.numeric()) %>% 
  drop_na(age_admission) %>% 
  drop_na(crf1a_sex) %>% # Note crf2a_sex contains no extra information
  drop_na(discharge_to_review) %>% 
  filter(crf_early_main_reason___7 == "Unchecked" | is.na(crf_early_main_reason___7)) %>% # Withdraws
  mutate(
    in_range = case_when(
      redcap_event_name== "3 Months (1st Research Visit)" &
        (discharge_to_review >= 42 | discharge_to_review < 240) ~ "Yes",
      redcap_event_name== "12 Months (2nd Research Visit)" &
        (discharge_to_review >= 300) ~ "Yes"
    )
  ) %>%
  filter(in_range == "Yes") %>% 
  select(study_id, redcap_event_name) %>% 
  mutate(minimum_dataset_3m_12m = "Yes")


phosp = phosp %>% 
  mutate(
    study_id_in_first_paper_1077 = if_else(study_id %in% phosp_3m_paper_study_id_1077, "Yes", "No"),
    study_id_in_second_paper_3m_2320 = if_else(study_id %in% phosp_12m_paper_3m_study_id_2320, "Yes", "No"),
    study_id_in_second_paper_12m_924 = if_else(study_id %in% phosp_12m_paper_12m_study_id_924, "Yes", "No")
  ) %>% 
  left_join(minimum_dataset_3m_12m)


rm(phosp_3m_paper_study_id_1077, 
   phosp_12m_paper_3m_study_id_2320, 
   phosp_12m_paper_12m_study_id_924,
   minimum_dataset_3m_12m)

# New variables
# study_id_in_first_paper_1077 
# study_id_in_second_paper_3m_2320 
# study_id_in_second_paper_12m_924
# minimum_dataset_3m_12m
