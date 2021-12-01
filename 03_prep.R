# PHOSP-COVID analysis: DATA PREP
# Cleaning and preparation of variables
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# Functions require library(tidyverse), requires() nor :: not currently written in.  

# Packages
library(finalfit)
library(tidyverse)
library(lubridate)
library(rspiro)

# Define tier 2 --------------------------------------------------------------------------
tier2_study_id = phosp %>% 
  filter(!is.na(crf3b_visit)) %>% 
  distinct(study_id) %>% 
  pull(study_id)

# Fill important variables across events and instruments ---------------------------------
## Within phosp, fill within patients across rows  ---------------------------------------
## This can be changed to joins in the future if causes any issues
## See join below. 
## Currently this is for PFT calculations
phosp = phosp %>%  
  group_by(study_id) %>% 
  fill(age_admission, crf1a_sex,  crf1b_eth, crf3a_rest_height,
       crf1a_date_dis, crf1b_edu,
       dplyr::starts_with("crf1a_com_"),
       .direction = "downup") %>% 
  ungroup() %>% 
  ff_relabel_df(phosp)

# Fill 12 months from 3 months------------------------------------------------------------
## Added 04/08/2021
## This may have other applications, but in particular baseline eq5d is missing from most
## 12 month forms
## This fill will preferentially use 3 months over 12 months over 6 weeks

# Variables to fill
vars_to_fill =  phosp %>% 
  select(matches("eq5d5l.*_pre")) %>% # add more here 
  names()

## Fill
phosp_fill = phosp %>% 
  select(study_id, redcap_event_name, redcap_repeat_instance, all_of(vars_to_fill)) %>% 
  filter(redcap_event_name %in% c("6 Weeks", "3 Months (1st Research Visit)",
                                  "12 Months (2nd Research Visit)")) %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  mutate(redcap_event_name = fct_relevel(redcap_event_name, 
                                         "3 Months (1st Research Visit)",
                                         "12 Months (2nd Research Visit)")) %>% 
  arrange(study_id, redcap_event_name) %>% 
  group_by(study_id) %>% 
  fill(all_of(vars_to_fill), .direction = "downup")

## Join
phosp = phosp %>% 
  select(-all_of(vars_to_fill)) %>% 
  left_join(phosp_fill)

rm(phosp_fill, vars_to_fill)

# Variable definitions-------------------------------------------------------------------
phosp = phosp %>% 
  mutate(
    
    # Tier 
    tier = if_else(study_id %in% tier2_study_id, 2, 1),
    
    # Explanatory variables ------------------
    ## Age
    age_admission_factor = case_when(
      age_admission < 30 ~ "<30",
      age_admission < 40 ~ "30-39",
      age_admission < 50 ~ "40-49",
      age_admission < 60 ~ "50-59",
      age_admission < 70 ~ "60-69",
      age_admission < 80 ~ "70-79",
      is.na(age_admission) ~ NA_character_,
      TRUE ~ "80+"
    ) %>% 
      fct_relevel("50-59") %>% 
      ff_label("Age at admission (y)"),
    
    ## Ethnicity
    crf1b_eth_5levels = case_when(
      crf1b_eth == "(1) White - English / Welsh / Scottish / Northern Irish / British" |                           
        crf1b_eth == "(2) White - Irish" | 
        crf1b_eth == "(3) White - Gypsy or Irish Traveller" |
        crf1b_eth == "(4) White - Any other White background" ~ "White", 
      crf1b_eth == "(5) Mixed/ Multiple Ethnic Backgrounds - White and Black Caribbean" |
        crf1b_eth == "(6) Mixed/ Multiple Ethnic Backgrounds - White and Black African" |
        crf1b_eth == "(7) Mixed/ Multiple Ethnic Backgrounds  - White and Asian" |
        crf1b_eth == "(8) Mixed/ Multiple Ethnic Backgrounds  - Any other Mixed/ Multiple ethnic background" ~ "Mixed", 
      crf1b_eth == "(9) Asian/ Asian British -  Indian" |
        crf1b_eth == "(10) Asian/ Asian British - Pakistani" | 
        crf1b_eth == "(11) Asian/ Asian British - Bangladeshi" |
        crf1b_eth == "(13) Asian/ Asian British - Any other Asian background" ~ "South Asian", 
      crf1b_eth == "(12) Asian/ Asian British - Chinese" ~ "Other",
      crf1b_eth == "(14) Black/ African/ Caribbean/ Black British - African" |
        crf1b_eth == "(15) Black/ African/ Caribbean/ Black British - Caribbean" |
        crf1b_eth == "(16) Black/ African/ Caribbean/ Black British - Any other Black/ African/ Caribbean background" ~ "Black",
      crf1b_eth == "(17) Other ethnic group - Arab" |
        crf1b_eth == "(18) Other ethnic group - Any other ethnic group" ~ "Other",
      crf1b_eth == "Prefer not to say" ~ NA_character_
    ) %>% 
      fct_relevel("White", "South Asian", "Black", "Mixed", "Other") %>% 
      ff_label("Ethnicity"),
    
    crf1b_eth_pft = case_when(
      crf1b_eth == "(1) White - English / Welsh / Scottish / Northern Irish / British" |
        crf1b_eth == "(2) White - Irish" |
        crf1b_eth == "(3) White - Gypsy or Irish Traveller" |
        crf1b_eth == "(4) White - Any other White background" ~ 1,
      crf1b_eth == "(5) Mixed/ Multiple Ethnic Backgrounds - White and Black Caribbean" |
        crf1b_eth == "(6) Mixed/ Multiple Ethnic Backgrounds - White and Black African" |
        crf1b_eth == "(7) Mixed/ Multiple Ethnic Backgrounds  - White and Asian" |
        crf1b_eth == "(8) Mixed/ Multiple Ethnic Backgrounds  - Any other Mixed/ Multiple ethnic background" ~ 5,
      crf1b_eth == "(9) Asian/ Asian British -  Indian" |
        crf1b_eth == "(10) Asian/ Asian British - Pakistani" |
        crf1b_eth == "(11) Asian/ Asian British - Bangladeshi" ~ 4,
      crf1b_eth == "(12) Asian/ Asian British - Chinese" ~ 3,
      crf1b_eth == "(13) Asian/ Asian British - Any other Asian background" ~ 5,
      crf1b_eth == "(14) Black/ African/ Caribbean/ Black British - African" |
        crf1b_eth == "(15) Black/ African/ Caribbean/ Black British - Caribbean" |
        crf1b_eth == "(16) Black/ African/ Caribbean/ Black British - Any other Black/ African/ Caribbean background" ~ 2,
      crf1b_eth == "(17) Other ethnic group - Arab" |
        crf1b_eth == "(18) Other ethnic group - Any other ethnic group" ~ 5,
      crf1b_eth == "Prefer not to say" ~ NA_real_
    ) %>%
      ff_label("Ethnicity"),
    
    ## Diabetes yes no
    crf1a_com_mer_diab_yn = if_else(crf1a_com_mer_diab == "Type 1" | crf1a_com_mer_diab == "Type 2", "Yes", "No") %>% 
      ff_label("Diabetes"),
    
    ## Malignancy
    crf1a_com_mh_stm_yn = if_else(crf1a_com_mh_stm == "Localised" | crf1a_com_mh_stm == "Metastatic", "Yes", "No") %>% 
      ff_label("Malignancy"),
    
    ## Symptoms - change N/K to missing
    across(crf1a_fever_history:crf1a_bleeding, fct_recode, NULL = "N/K"),
    
    ## Symptom scales - convert to numeric
    across(starts_with("psq_scale_"), ~ as.character(.) %>% parse_number()),
    
    ## Erectile dysfunction in men only
    psq_symp_ed = as.character(psq_symp_ed),
    psq_symp_ed = case_when(
      crf1a_sex == "Female" ~ NA_character_,
      psq_symp_ed == "N/A" ~ NA_character_,
      TRUE ~ psq_symp_ed 
    ),
    
  ) %>% 
  
  mutate( # (new mutate needed to utilise diabetes and malig variables)
    
    ## Comorbidity count (no missings here)
    no_comorbid = select(., 
                         # Count these variables
                         crf1a_com_card_mi:crf1a_com_id_other_yn,
                         crf1a_com_mer_diab_yn,
                         crf1a_com_mh_stm_yn,
                         
                         # But don't count these, note not counting the "others"
                         -c(crf1a_com_card_catia,
                            crf1a_com_card_other_yn,
                            crf1a_com_card_other,
                            crf1a_com_neupsy_other,
                            crf1a_com_res_other_yn,
                            crf1a_com_res_other,
                            crf1a_com_rheu_other_yn,
                            crf1a_com_rheu_other,
                            crf1a_com_gast_ld,
                            crf1a_com_gast_other_yn,
                            crf1a_com_gast_other,
                            crf1a_com_mer_other_yn,
                            crf1a_com_mer_ckd_stage,
                            crf1a_com_mer_other,
                            crf1a_com_mer_diab,
                            crf1a_com_mer_diab_com,
                            crf1a_com_mh_stm,
                            crf1a_com_mh_stm_detail,
                            crf1a_com_mh_leuk_detail,
                            crf1a_com_mh_lymp,
                            crf1a_com_mh_other,
                            crf1a_com_id_cvh,
                            crf1a_com_id_mt,
                            crf1a_com_id_other_yn)
    ) %>% 
      {. == "Yes"} %>% 
      rowSums(na.rm = TRUE) %>% 
      ff_label("Number of comorbidities"), 
    
    no_comorbid_3levels = case_when(
      no_comorbid == 0 ~ "No comorbidity",
      no_comorbid == 1 ~ "1 comorbidity", 
      no_comorbid > 1 ~ "2+ comorbidities"
    ) %>% 
      factor() %>% 
      fct_relevel("No comorbidity") %>% 
      ff_label("Number of comorbidities (factor)"), 
    
    no_comorbid_2levels = case_when(
      no_comorbid == 0 ~ "No comorbidity",
      no_comorbid > 1 ~ "1+ comorbidity", 
    ) %>% 
      factor() %>% 
      fct_relevel("No comorbidity") %>% 
      ff_label("Number of comorbidities (factor)"), 
    
    # Dates ------------------
    ## Symptom duration
    crf1a_symptom_duration = (crf1a_date_adm - crf1a_date_first_symptoms) %>% 
      as.numeric("days") %>% 
      ff_label("Symptom duration (days)"),
    
    ## Admission duration
    crf1a_admission_duration = (crf1a_date_dis - crf1a_date_adm) %>% 
      as.numeric("days") %>% 
      ff_label("Admission duration (days)"),
    
    
    # Comorbidities ------------------
    ## Make summary variables
    crf1a_com_card = rowSums(select(., dplyr::starts_with("crf1a_com_card"), -contains("other")) %>% 
                               mutate(across(everything(), ~ . == "Yes")),
                             na.rm = TRUE),
    crf1a_com_res = rowSums(select(., dplyr::starts_with("crf1a_com_res"), -c(contains("other"), crf1a_com_res_ild)) %>% 
                              mutate(across(everything(), ~ . == "Yes")),
                            na.rm = TRUE),
    crf1a_com_gast = rowSums(select(., dplyr::starts_with("crf1a_com_gast"), -contains("other")) %>% 
                               mutate(across(everything(), ~ . == "Yes")),
                             na.rm = TRUE),
    crf1a_com_neupsy = rowSums(select(., dplyr::starts_with("crf1a_com_neupsy"), -c(contains("other"), crf1a_com_res_ild)) %>% 
                                 mutate(across(everything(), ~ . == "Yes")),
                               na.rm = TRUE),
    crf1a_com_rheu = rowSums(select(., dplyr::starts_with("crf1a_com_rheu"), -contains("other")) %>% 
                               mutate(across(everything(), ~ . == "Yes")),
                             na.rm = TRUE),
    crf1a_com_mer = rowSums(select(., dplyr::starts_with("crf1a_com_mer"), -c(contains("other"),
                                                                              crf1a_com_mer_diab,
                                                                              crf1a_com_mer_diab_com,
                                                                              crf1a_com_mer_diab_yn)) %>% 
                              mutate(across(everything(), ~ . == "Yes")),
                            na.rm = TRUE),
    crf1a_com_mh = rowSums(select(., dplyr::starts_with("crf1a_com_mh"), -c(contains("other"), 
                                                                            contains("detail"), 
                                                                            crf1a_com_mh_lymp)) %>% 
                             mutate(across(everything(), ~ . == "Yes")),
                           na.rm = TRUE),
    crf1a_com_id = rowSums(select(., dplyr::starts_with("crf1a_com_id"), -contains("other")) %>% 
                             mutate(across(everything(), ~ . == "Yes")),
                           na.rm = TRUE),
    
    # Special case diabetes separated out - need to compare this with crf1a_com_mer_diab_yn for missingness
    # Diabetes update 15/03/2021 - include type 1 in endocrine and only pull type 2 for the separate variable
    crf1a_com_diab = fct_explicit_na(crf1a_com_mer_diab, na_level = "No") %>%
      fct_recode("No" = "Type 1") %>% 
      fct_drop() %>% 
      fct_relevel("No"),
    
    ## Add type 1 to mer
    crf1a_com_mer = case_when(
      crf1a_com_mer_diab == "Type 1" ~ crf1a_com_mer + 1,
      TRUE ~ crf1a_com_mer
    ),
    
    across(c(crf1a_com_card, crf1a_com_res,
             crf1a_com_gast, crf1a_com_neupsy,
             crf1a_com_rheu, crf1a_com_mer,
             crf1a_com_mh, crf1a_com_id),
           ~ case_when(
             . > 0 ~ "Yes",
             TRUE ~ "No") %>%
             factor()
    ),
    
    crf1a_com_card = ff_label(crf1a_com_card, "Cardiovascular"), 
    crf1a_com_res = ff_label(crf1a_com_res, "Respiratory"),
    crf1a_com_gast = ff_label(crf1a_com_gast, "Gastrointestinal"), 
    crf1a_com_neupsy = ff_label(crf1a_com_neupsy, "Neurological and psychiatric"),
    crf1a_com_rheu = ff_label(crf1a_com_rheu, "Rheumatological"), 
    crf1a_com_mer = ff_label(crf1a_com_mer, "Metabolic/endocrine/renal"), 
    crf1a_com_mh = ff_label(crf1a_com_mh, "Malignancy"), 
    crf1a_com_id = ff_label(crf1a_com_id, "Infectious disease"), 
    
    # Anthro values ------------------
    ## Height (character)
    crf3a_rest_height = parse_number(crf3a_rest_height),
    crf3a_rest_weight = parse_number(crf3a_rest_weight),
    crf3a_bmi = (crf3a_rest_weight / (crf3a_rest_height / 100)^2) %>% 
      ff_label("BMI"),
    
    crf3a_bmi_5levels = case_when(
      crf3a_bmi < 18.5 ~ "Underweight (<18.5)",
      crf3a_bmi < 25 ~ "Normal weight (18.5 to 24.9)",
      crf3a_bmi < 30 ~ "Overweight (25 to 29.9)",
      crf3a_bmi < 40 ~ "Obese (30 to 39.9)",
      is.na(crf3a_bmi) ~ NA_character_,
      TRUE ~ "Severe obesity (40+)"
    ) %>% 
      factor() %>% 
      fct_relevel("Underweight (<18.5)", "Normal weight (18.5 to 24.9)",
                  "Overweight (25 to 29.9)", "Obese (30 to 39.9)",
                  "Severe obesity (40+)") %>% 
      ff_label("BMI (5 levels)"), 
    
    crf3a_bmi_2levels = case_when(
      crf3a_bmi < 30 ~ "BMI < 30 kg/m^2",
      is.na(crf3a_bmi) ~ NA_character_,
      TRUE ~ "BMI 30+ kg/m^2"
    ) %>% 
      factor() %>% 
      fct_relevel("BMI < 30 kg/m^2", "BMI 30+ kg/m^2") %>% 
      ff_label("BMI (2 levels)"),
    
    
    
    # PROMs ------------------
    ## EQ5D
    eq5d5l_summary_pre = parse_number(eq5d5l_summary_pre),
    eq5d5l_summary = parse_number(eq5d5l_summary),
    eq5d5l_summary_delta = (eq5d5l_summary - eq5d5l_summary_pre) %>% 
      ff_label("How good or bad is your health overall? Now vs pre-covid"),
    
    eq5d5l_summary_delta_change = case_when(
      eq5d5l_summary_delta == 0 ~ "No change",
      eq5d5l_summary_delta > 0 ~ "Improvement",
      eq5d5l_summary_delta < 0 ~ "Worse"
    )   %>%
      factor() %>%
      fct_relevel("No change"),
    
    
    ## EQ5D sum across domains. as.numeric makes lowest == 1, so subtract 1 for zero as reference
    eq5d5l_total_pre = rowSums(select(., eq5d5l_q1_pre:eq5d5l_q5_pre) %>% 
                                 mutate(across(everything(), ~ {as.numeric(.) - 1})),
                               na.rm = TRUE) %>% 
      ff_label("EQ5D sum of domains pre-covid"),
    
    eq5d5l_total = rowSums(select(., eq5d5l_q1:eq5d5l_q5) %>% 
                             mutate(across(everything(), ~ {as.numeric(.) - 1})),
                           na.rm = TRUE) %>% 
      ff_label("EQ5D sum of domains"),
    
    eq5d5l_total_delta = (eq5d5l_total - eq5d5l_total_pre) %>% 
      ff_label("EQ5D change in sum of domains"), 
    
    across(starts_with("eq5d5l_q"), ~ as.numeric(.) - 1, .names = "{.col}_numeric"),
    eq5d5l_q1_delta = eq5d5l_q1_numeric - eq5d5l_q1_pre_numeric,
    eq5d5l_q2_delta = eq5d5l_q1_numeric - eq5d5l_q2_pre_numeric,
    eq5d5l_q3_delta = eq5d5l_q1_numeric - eq5d5l_q3_pre_numeric,
    eq5d5l_q4_delta = eq5d5l_q1_numeric - eq5d5l_q4_pre_numeric,
    eq5d5l_q5_delta = eq5d5l_q1_numeric - eq5d5l_q5_pre_numeric,
    
    across(matches("eq5d5l_q.*_delta"), ~ case_when(
      . == 0 ~ "No change",
      . < 0 ~ "Improvement",
      . > 0 ~ "Worse"
    )   %>% 
      factor() %>% 
      fct_relevel("No change"), .names = "{.col}_change"),
    
    # EQ5D population norms, added 01/07/2021 EMH
    
    eq5d_population_norm = case_when(
      age_admission < 25 ~ 0.940,
      age_admission < 35 ~ 0.927,
      age_admission < 45 ~ 0.911,
      age_admission < 55 ~ 0.847,
      age_admission < 65 ~ 0.799,
      age_admission < 75 ~ 0.779,
      is.na(age_admission) ~ NA_real_,
      TRUE ~ 0.726 # 75+
    )
  ) %>% 
  ff_relabel_df(phosp)


# Calculate EQ5DL utility index for phosp data -----------------------------------------------------------------------
##  Code author: Steven Kerr

## Look up table
crosswalk_lookup <- readxl::read_excel('/home/eharrison/phosp_first_report/crosswalk_lookup.xls',sheet = "EQ-5D-5L Value Sets")


## Concatenate responses functions  
concat_answers <- function(ans1,ans2,ans3,ans4,ans5){
  answers <- bind_cols(ans1, ans2 , ans3, ans4, ans5)
  answerString <- ifelse(complete.cases(answers), paste(ans1, ans2, ans3, ans4, ans5, sep = ''), NA)
}

concat_answers2 <- function(answers){
  answers <- mutate_all(answers, as.numeric)
  answerString <- ifelse(complete.cases(answers), paste(answers[,1], answers[,2], answers[,3], answers[,4], answers[,5], sep = ''), NA)
}

## Pre-covid
phosp <- mutate(phosp, answers = 
                  concat_answers(as.numeric(eq5d5l_q1_pre), as.numeric(eq5d5l_q2_pre), 
                                 as.numeric(eq5d5l_q3_pre), as.numeric(eq5d5l_q4_pre), 
                                 as.numeric(eq5d5l_q5_pre) )) %>%
  left_join( crosswalk_lookup[c('...1', 'UK')] ,    by = c("answers" = "...1")) %>%
  dplyr::select(-c(answers)) %>% 
  dplyr::rename(eq5d5l_utility_index_pre = UK) %>% 
  mutate(eq5d5l_utility_index_pre = as.numeric(eq5d5l_utility_index_pre))

## Post-covid
phosp <- mutate(phosp, answers = 
                  concat_answers(as.numeric(eq5d5l_q1), as.numeric(eq5d5l_q2), 
                                 as.numeric(eq5d5l_q3), as.numeric(eq5d5l_q4), 
                                 as.numeric(eq5d5l_q5) )) %>%
  left_join( crosswalk_lookup[c('...1', 'UK')] , by = c("answers" = "...1")) %>%
  dplyr::select(-c(answers)) %>% 
  dplyr::rename(eq5d5l_utility_index = UK) %>% 
  mutate(eq5d5l_utility_index = as.numeric(eq5d5l_utility_index),
         eq5d5l_utility_index_delta = (eq5d5l_utility_index - eq5d5l_utility_index_pre) %>% 
           ff_label("EQ5D utility index (difference)")
  )

rm(crosswalk_lookup, concat_answers, concat_answers2)

# Patient questionnaire cleaning ----------------------------------------------------------------------------
## Change
phosp = phosp %>% 
  mutate(
    across(starts_with("patient_sq_l_"), ~ as.numeric(.) - 1, .names = "{.col}_numeric"),
    patient_sq_l_t_seeing_delta = patient_sq_l_t_seeing_numeric - patient_sq_l_b_seeing_numeric,
    patient_sq_l_t_hearing_delta = patient_sq_l_t_hearing_numeric - patient_sq_l_b_hearing_numeric,
    patient_sq_l_t_walking_delta = patient_sq_l_t_walking_numeric - patient_sq_l_b_walking_numeric,
    patient_sq_l_t_remembering_delta = patient_sq_l_t_remembering_numeric - patient_sq_l_b_remembering_numeric,
    patient_sq_l_t_self_care_delta = patient_sq_l_t_self_care_numeric - patient_sq_l_b_self_care_numeric,
    patient_sq_l_t_communicate_delta = patient_sq_l_t_communicate_numeric - patient_sq_l_b_communicate_numeric,
    
    across(matches("patient_sq_l_t_.*_delta"), ~ case_when(
      . == 0 ~ "No change",
      . < 0 ~ "Improvement",
      . > 0 ~ "Worse"
    )   %>% 
      factor() %>% 
      fct_relevel("No change"), .names = "{.col}_change"),
    
    # Want if_all and if_any here to keep the regex column names select, but on-going issues
    # https://github.com/tidyverse/dplyr/issues/5782
    
    # % with a disability
    patient_sq_l_t_disability = case_when(
      patient_sq_l_t_seeing == "Yes - a lot difficulty" |
        patient_sq_l_t_hearing == "Yes - a lot difficulty" |
        patient_sq_l_t_walking == "Yes - a lot difficulty" |
        patient_sq_l_t_remembering == "Yes - a lot difficulty" |
        patient_sq_l_t_self_care  == "Yes - a lot difficulty" |
        patient_sq_l_t_communicate == "Yes - a lot difficulty" ~ "Yes",
      
      is.na(patient_sq_l_t_seeing) &
        is.na(patient_sq_l_t_hearing) &
        is.na(patient_sq_l_t_walking) &
        is.na(patient_sq_l_t_remembering) &
        is.na(patient_sq_l_t_self_care) &
        is.na(patient_sq_l_t_communicate) ~ NA_character_,
      TRUE ~ "No") %>% 
      factor() %>% 
      ff_label("WG-SS: any 'Yes - lot of difficulty'"),
    
    # New disability, 0-2, 0-3, 1-2, 1-3
    ## All these are > 1 except, 1 to 2 which is coded specifically
    patient_sq_l_t__new_disability = case_when(
      patient_sq_l_t_seeing_delta > 1 ~ "Yes",
      patient_sq_l_t_hearing_delta > 1 ~ "Yes",          
      patient_sq_l_t_walking_delta > 1 ~ "Yes",            
      patient_sq_l_t_remembering_delta > 1 ~ "Yes",       
      patient_sq_l_t_self_care_delta > 1 ~ "Yes",     
      patient_sq_l_t_communicate_delta > 1 ~ "Yes",
      
      patient_sq_l_t_seeing_numeric == 2 & patient_sq_l_b_seeing_numeric == 1 ~ "Yes",
      patient_sq_l_t_hearing_numeric == 2 & patient_sq_l_b_hearing_numeric == 1 ~ "Yes",
      patient_sq_l_t_walking_numeric == 2 & patient_sq_l_b_walking_numeric == 1 ~ "Yes",
      patient_sq_l_t_remembering_numeric == 2 & patient_sq_l_b_remembering_numeric == 1 ~ "Yes",
      patient_sq_l_t_self_care_numeric == 2 & patient_sq_l_b_self_care_numeric == 1 ~ "Yes",
      patient_sq_l_t_communicate_numeric == 2 & patient_sq_l_b_communicate_numeric == 1 ~ "Yes",
      
      is.na( patient_sq_l_t_seeing_delta) &
        is.na( patient_sq_l_t_hearing_delta) &
        is.na( patient_sq_l_t_walking_delta) &
        is.na( patient_sq_l_t_remembering_delta) &
        is.na( patient_sq_l_t_self_care_delta) &
        is.na( patient_sq_l_t_communicate_delta) ~ NA_character_,
      TRUE ~ "No"
    ) %>% 
      factor() %>% 
      ff_label("WG-SS: new disability"),
    
    ## PSQ
    psq_scale_blness_delta = psq_scale_blness_24hrs - psq_scale_blness_pre,
    psq_scale_fatigue_delta = psq_scale_fatigue_24hrs - psq_scale_fatigue_pre,
    psq_scale_cough_delta = psq_scale_cough_24hrs - psq_scale_cough_pre,
    psq_scale_pain_delta = psq_scale_pain_24hrs - psq_scale_pain_pre,
    psq_scale_sleep_delta = psq_scale_sleep_24hrs - psq_scale_sleep_pre,
    
    across(matches("psq_scale_.*_delta"), ~ case_when(
      . == 0 ~ "No change",
      . < 0 ~ "Improvement",
      . > 0 ~ "Worse"
    )   %>% 
      factor() %>% 
      fct_relevel("No change"), .names = "{.col}_change"),
    
    
    
    ## GAD7
    gad7_summary_2levels = case_when(
      gad7_summary <= 8 ~ "No",
      gad7_summary > 8 ~ "Yes",
    ) %>%
      factor() %>% 
      ff_label("Anxiety (GAD7 >8)"),
    
    
    ## PHQ9
    phq9_summary_2levels = case_when(
      phq9_summary < 10 ~ "No",
      phq9_summary >=10 ~ "Yes",
    ) %>%
      factor() %>% 
      ff_label("Depression (PHQ-9 >= 10)"),
    
    ## PCL-5
    pcl5_summary_2levels = case_when(
      pcl5_summary < 38 ~ "No",
      pcl5_summary >=38 ~ "Yes",
    ) %>%
      factor() %>% 
      ff_label("PTSD (PCL-5 >= 38)"),
    
    ## Dyspoea-12
    dyspnoea12_summary = rowSums(select(., matches("^dysp")) %>% 
                                   mutate(across(everything(), ~ {as.numeric(.) - 1})),
                                 na.rm = FALSE) %>% 
      ff_label("Dyspnoea-12 score"),
    
    ## BPI
    bpi_severity_summary = rowSums(select(., bpi_worst:bpi_rightnow) %>% 
                                     mutate(across(everything(), ~ {as.numeric(.) - 1})),
                                   na.rm = FALSE) %>% 
      ff_label("BPI severity"),
    
    bpi_interference_summary = rowSums(select(., bpi_past24_general:bpi_past24_enjoyment) %>% 
                                         mutate(across(everything(), ~ {as.numeric(.) - 1})),
                                       na.rm = FALSE) %>% 
      ff_label("BPI interference"),
    
    
    ## FACIT (needs reversed)
    facit_item_total = 52 - facit_item_total,
    
    
    ## SPBB
    sppb_score_summary = case_when(
      sppb_score <= 10 ~ "Yes",
      sppb_score > 10 ~ "No",
    ) %>% 
      ff_label("SPPB (mobility disability <=10"), 
    
    
    ## Rockwood clinical frailty score (RCF)
    rcf_score_numeric = rcf_score %>% as.numeric() %>% ff_label("RCF score (numeric)"),
    rcf_score_summary = case_when(
      rcf_score_numeric >= 5 ~ "Yes",
      rcf_score_numeric < 5 ~ "No",
    ) %>% 
      ff_label("Rockwood clinical frailty score >=5"), 
    
    ## MOCA
    mocal_total = if_else(mocal_total == 0, NA_real_, mocal_total),
    
    mocal_total_summary = case_when(
      mocal_total == 0 ~ NA_character_,
      mocal_total < 23 ~ "Yes",
      mocal_total >= 23 ~ "No",
    ) %>% 
      ff_label("MOCA <23"), 
    
    ## MOCA corrected is done below, because it needs a fill on education level. 
    
    
    # Treatment -------------------
    across(c("crf1a_treat_ss", "crf1a_treat_at", "crf1a_treat_tdac"), 
           ~ fct_recode(., 
                        NULL = "N/K") %>% 
             fct_relevel("No")
    ),
    
    
    # Outcomes ------------------
    ## Respiratory support
    crf1a_resp_support_8levels = case_when(
      crf1a_o2_ecmo == "Yes" ~ "ECMO", 
      crf1a_treat_rrt == "Yes" ~ "RRT",
      crf1a_o2_imv == "Yes" ~ "IMV", 
      crf1a_o2_hfn == "Yes" ~ "HFN", 
      crf1a_o2_blniv == "Yes" ~ "BIPAP", 
      crf1a_o2_cpapv == "Yes" ~ "CPAP", 
      crf1a_o2_supp == "Yes" ~ "O2",
      is.na(crf1a_o2_ecmo) & 
        is.na(crf1a_treat_rrt) & 
        is.na(crf1a_o2_imv) & 
        is.na(crf1a_o2_hfn) & 
        is.na(crf1a_o2_blniv) & 
        is.na(crf1a_o2_cpapv) & 
        is.na(crf1a_o2_supp) ~ NA_character_,
      TRUE ~ "No respiratory support"
    ) %>% 
      factor() %>% 
      fct_relevel("No respiratory support", "O2", "CPAP", "BIPAP", "HFN", "IMV", "RRT", "ECMO") %>% 
      ff_label("Maximal organ support"),
    
    
    crf1a_resp_support_4levels = case_when(
      crf1a_o2_ecmo == "Yes" ~ "WHO – class 7-9", 
      crf1a_treat_rrt == "Yes" ~ "WHO – class 7-9",
      crf1a_o2_imv == "Yes" ~ "WHO – class 7-9", 
      crf1a_o2_hfn == "Yes" ~ "WHO – class 6", 
      crf1a_o2_blniv == "Yes" ~ "WHO – class 6", 
      crf1a_o2_cpapv == "Yes" ~ "WHO – class 6", 
      crf1a_o2_supp == "Yes" ~ "WHO – class 5",
      is.na(crf1a_o2_ecmo) & 
        is.na(crf1a_treat_rrt) & 
        is.na(crf1a_o2_imv) & 
        is.na(crf1a_o2_hfn) & 
        is.na(crf1a_o2_blniv) & 
        is.na(crf1a_o2_cpapv) & 
        is.na(crf1a_o2_supp) ~ NA_character_,
      TRUE ~ "WHO – class 3-4"
    ) %>% 
      factor() %>% 
      fct_relevel("WHO – class 3-4", "WHO – class 5",	"WHO – class 6",	"WHO – class 7-9") %>% 
      # Old levels: "No respiratory support", "O2", "CPAP/BIPAP/HFN", "IMV/RRT/ECMO") %>% 
      ff_label("WHO clinical progression scale")
    
  ) %>% 
  ff_relabel_df(phosp) %>% 
  
  # Tmp change labels - take these back to REDCap label file
  mutate(
    gad7_summary = ff_label(gad7_summary, "GAD7 total score"),
    phq9_summary = ff_label( phq9_summary, "PHQ9 total score"),
    rcf_score = ff_label(rcf_score, "RCF score (factor)"),
    mocal_total = ff_label(mocal_total, "MOCA total score"),
  )


# MOCA adjusted for level of educational attainment, which needs the above fill ----------------
phosp = phosp %>% 
  mutate(
    mocal_total_corrected = case_when(
      crf1b_edu %in% c("None", "Primary school",
                       "Secondary school (GCSE level, NVQ level 1/2 or equivalent, typically age 16)") ~ mocal_total + 1,
      TRUE ~ mocal_total
    ),
    
    mocal_total_corrected_summary = case_when(
      mocal_total_corrected < 23 ~ "Yes",
      mocal_total_corrected >= 23 ~ "No",
    ) %>% 
      ff_label("MOCA (corrected) <23")
  ) %>% 
  ff_relabel_df(phosp)


# Temporarily remove out-of-range values for certain variables --------------------------
phosp = phosp %>% 
  mutate(
    age_admission = case_when(
      age_admission < 18 ~ NA_real_,
      age_admission > 120 ~ NA_real_,
      TRUE ~ age_admission
    ),
    
    crf3a_rest_height = case_when(
      crf3a_rest_height < 100 ~ NA_real_,
      crf3a_rest_height > 250 ~ NA_real_,
      TRUE ~ crf3a_rest_height
    ),
    
    crf3a_rest_weight = case_when(
      crf3a_rest_weight < 40 ~ NA_real_,
      crf3a_rest_weight > 300 ~ NA_real_,
      TRUE ~ crf3a_rest_weight
    ),
    
    crf3a_bmi = case_when(
      crf3a_bmi  < 15 ~ NA_real_,
      crf3a_bmi  > 80 ~ NA_real_,
      TRUE ~ crf3a_bmi 
    ),
    
    crf3a_visit_date = case_when(
      study_id == "62-46" & crf3a_visit_date == ymd("2021-11-20") ~ ymd("2020-11-20"),
      study_id == "23-35" & crf3a_visit_date == ymd("2021-10-15") ~ ymd("2020-10-15"),
      study_id == "3-189" & crf3a_visit_date == ymd("2021-07-14") ~ ymd("2020-07-14"),
      study_id ==  "33-9" & crf3a_visit_date == ymd("2021-05-26") ~ ymd("2020-05-26"),
      
      study_id ==  "54-13" & crf3a_visit_date == ymd("2020-01-04") ~ ymd("2021-01-04"),
      study_id ==  "61-35" & crf3a_visit_date == ymd("2020-01-12") ~ ymd("2021-01-12"),
      study_id ==  "62-72" & crf3a_visit_date == ymd("2020-01-22") ~ ymd("2021-01-22"),
      study_id ==  "10-119" & crf3a_visit_date == ymd("2020-01-20") ~ ymd("2021-01-20"),
      study_id ==  "3-220" & crf3a_visit_date == ymd("2020-01-26") ~ ymd("2021-01-26"),
      study_id ==  "62-30" & crf3a_visit_date == ymd("2020-01-12") ~ ymd("2021-01-12"),
      study_id ==  "12-18" & crf3a_visit_date == ymd("2020-01-01") ~ ymd("2021-01-01"),
      
      
      TRUE ~ crf3a_visit_date
    )
  ) %>% 
  ff_relabel_df(phosp)

# Blood results -------------------------------------------------------------------------------
phosp = phosp %>% 
  mutate(
    # BNP
    bnp_summary = case_when(
      pnbnp_result >= 400 ~ "Yes",
      bnp_result >= 100 ~ "Yes",
      pnbnp_less_greater == "> (Greater than)" ~ "Yes",
      bnp_less_greater == "> (Greater than)" ~ "Yes",
      is.na(pnbnp_result) & is.na(bnp_result) &
        is.na(pnbnp_less_greater) & is.na(bnp_less_greater) ~ NA_character_,
      TRUE ~ "No"
    ) %>% 
      factor() %>% 
      ff_label("BNP/NT-Pro-BNP above threshold"),
    
    hba1c_result = case_when(
      hba1c_result > 20 ~ (0.09148 * hba1c_result) + 2.152,
      TRUE ~ hba1c_result
    ),
    
    hba1c_summary = case_when(
      hba1c_result >= 6.0 ~ "Yes",
      # hba1c_less_greater == "> (Greater than)" ~ "Yes", ## Nothing in this variable?
      hba1c_result < 6.0 ~ "No"
    ) %>% 
      factor() %>% 
      ff_label("HbA1C above threshold (>=6.0%)"),
    
    egfr_summary = case_when(
      egfr_result < 60 ~ "Yes",
      egfr_result >= 60 ~ "No"
    ) %>% 
      factor() %>% 
      ff_label("eGFR < 60 ml/min/1.73 m2"),
    
    ddi_result = as.numeric(ddi_result),
    ddi_result = if_else(ddi_result < 10, ddi_result * 500, ddi_result),
    
    ddi_summary = case_when(
      ddi_result < 500 ~ "No",
      ddi_result >= 500 ~ "Yes"
    ) %>% 
      ff_label("D-dimer (>= 500 ng/ml)"),
    
    crp_summary = case_when(
      crp_result > 10 ~ "Yes",
      crp_result <= 10 ~ "No"
    ) %>% 
      ff_label("CRP (>10 mg/L)"),
    
    crp_summary5 = case_when(
      crp_result > 5 ~ "Yes",
      crp_result <= 5 ~ "No"
    ) %>% 
      ff_label("CRP (>5 mg/L)")
    
  ) %>% 
  ff_relabel_df(phosp)


# Occupation ----------------------------------------------------------------------------------
phosp = phosp %>% 
  mutate(
    social_status_before = ff_label(social_status_before, "Occupation/working status before COVID-19"),
    
    social_status_after = case_when(
      patient_sq_q == "Same as before" ~ social_status_before,
      patient_sq_q == "Different from before" & 
        social_status_before != patient_sq_q_today ~ patient_sq_q_today
    ) %>% 
      ff_label("Occupation/working status since COVID-19"),
    
    # working_status change
    working_status_change = case_when( 
      patient_sq_q == "Different from before" ~ "Yes",
      patient_sq_q == "Same as before" ~ "No"
    ), 
    
    # working_less
    working_less = case_when( 
      working_status_change == "Yes" & 
        social_status_before == "Working full-time" & 
        patient_sq_q_today == "Working part-time" & 
        !is.na(patient_sq_q_change) ~ "Yes",
      working_status_change == "Yes" ~ "No",
      working_status_change == "No" ~ "No"),
    
    
    working_before = case_when(
      social_status_before %in% c("Working full-time",
                                  "Working part-time") ~ "Yes",
      is.na(social_status_before) ~ NA_character_,
      TRUE ~ "No"
    ),
    
    # no_longer_working
    no_longer_working = case_when(
      working_status_change == "Yes" & 
        patient_sq_q_today %in% c(
          "Full time carer (children or other)", 
          "Unemployed",                          
          "Unable to work due to chronic illness",
          "Student",
          "Retired",
          "Medically retired",
          "Prefer not to say") ~ "Yes",
      is.na(working_status_change) ~ NA_character_,
      TRUE ~ "No"),
    
    # health_reasons
    health_reasons = case_when( 
      is.na(patient_sq_q_change) ~ NA_character_,
      patient_sq_q_change %in% c("Poor health", "Sick leave") ~ "Yes",
      TRUE ~ "No"
    ),
    
    # employer_reasons
    employer_reasons = case_when( 
      is.na(patient_sq_q_change) ~ NA_character_,
      patient_sq_q_change %in% c("Made redundant", "Working hours reduced by employer") ~ "Yes",
      TRUE ~ "No"
    ),
    
    # other reasons
    other_reasons = case_when( 
      is.na(patient_sq_q_change) ~ NA_character_,
      patient_sq_q_change %in% c("New caring responsibility",
                                 "Other",
                                 "Prefer not to say")  ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% 
  ff_relabel_df(phosp)



# PFTs ----------------------------------------------------------------------------------------
phosp = phosp %>% 
  mutate(    
    ### These should be numeric
    across(c(pft_tlco_reading1, pft_tlco_reading2, pft_kco_reading1, pft_kco_reading2, 
             pft_mip_reading, pft_mip_reading2, pft_mep_reading1, pft_mep_reading2), parse_number)
  ) %>% 
  
  mutate(
    
    ### FEV1:FVC ratio reported as percentage normal by some, recalculate from raw values
    pft_fev1_fvc_r1 = pft_fev1_r1a / pft_fvc_r1a,
    pft_fev1_fvc_r2 = pft_fev1_r2a / pft_fvc_r2a,
    pft_fev1_fvc_r3 = pft_fev1_r3a / pft_fvc_r3a#,
    
    ### Means of three readings
    # pft_fev1 = select(., starts_with("pft_fev1_r")) %>% rowMeans(na.rm = TRUE) %>% ff_label("FEV1"),
    # pft_fvc = select(., starts_with("pft_fvc_r")) %>% rowMeans(na.rm = TRUE) %>% ff_label("FVC"),
    # pft_fev1_fvc = (pft_fev1 / pft_fvc) %>% ff_label("FEV1/FVC"),
    # pft_tlco = select(., pft_tlco_reading1, pft_tlco_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("TLCO"),
    # pft_kco = select(., pft_kco_reading1, pft_kco_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("KCO"),
    # pft_mip = select(., pft_mip_reading, pft_mip_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("MIP"), 
    # pft_mep = select(., pft_mep_reading1, pft_mep_reading2) %>% rowMeans(na.rm = TRUE) %>% ff_label("MEP"),
    
    ### Change to max of three readings
    # Note new method here. 
  ) %>% 
  
  mutate(
    pft_fev1 = pmap_dbl(select(., starts_with("pft_fev1_r")), pmax, na.rm=TRUE) %>% ff_label("FEV1"),
    pft_fvc = pmap_dbl(select(., starts_with("pft_fvc_r")), pmax, na.rm=TRUE) %>% ff_label("FVC"),
    pft_tlco = pmap_dbl(select(., c(pft_tlco_reading1, pft_tlco_reading2)), pmax, na.rm=TRUE) %>% ff_label("TLCO"),
    pft_kco = pmap_dbl(select(., c(pft_kco_reading1, pft_kco_reading2)), pmax, na.rm=TRUE) %>% ff_label("KCO"),
    pft_mip = pmap_dbl(select(., c(pft_mip_reading, pft_mip_reading2)), pmax, na.rm=TRUE) %>% ff_label("MIP"),
    pft_mep = pmap_dbl(select(., c(pft_mep_reading1, pft_mep_reading2)), pmax, na.rm=TRUE) %>% ff_label("MEP")
  ) %>% 
  ungroup() %>% 
  
  mutate(
    pft_fev1_fvc = (pft_fev1 / pft_fvc) %>% ff_label("FEV1/FVC"),
    pft_fev1_pred = pred_GLI(age_admission, crf3a_rest_height / 100, crf1a_sex, crf1b_eth_pft, param = c("FEV1")),
    pft_fvc_pred = pred_GLI(age_admission, crf3a_rest_height / 100, crf1a_sex, crf1b_eth_pft, param = c("FVC")),
    
    pft_fev1_perc_pred = (100 * pft_fev1 / pft_fev1_pred) %>% ff_label("FEV1 % predicted"), 
    pft_fvc_perc_pred = (100 * pft_fvc / pft_fvc_pred) %>% ff_label("FVC % predicted"),
    
    
    pft_fev1_perc_pred_80 = case_when(
      pft_fev1_perc_pred < 80 ~ "Yes",
      pft_fev1_perc_pred >= 80 ~ "No",
    ) %>% 
      ff_label("FEV1 % pred <80%"),
    
    
    pft_fvc_perc_pred_80 = case_when(
      pft_fvc_perc_pred < 80 ~ "Yes",
      pft_fvc_perc_pred >= 80 ~ "No",
    ) %>% 
      ff_label("FVC % pred <80%"),
    
    
    pft_fev1_fvc_70 = case_when(
      pft_fev1_fvc < 0.7 ~ "Yes",
      pft_fev1_fvc >= 0.7 ~ "No",
    ) %>% 
      ff_label("FEV1/FVC <0.7"),

    pft_tlco = case_when(
      pft_tlco >  15 ~ 0.335 * pft_tlco,
      TRUE ~ pft_tlco
    ),
    
    pft_kco = case_when(
      pft_kco > 2.5 ~ 0.335 * pft_kco,
      TRUE ~ pft_kco
    )
  )


# IMD ------------------------------------------------------------------------------------
post_code_main_lookup = read_csv('http://argonaut.is.ed.ac.uk/public/lookup/NSPL_FEB_2020_UK.csv')

pcode_data = phosp %>% 
  select(study_id, post_code) %>% 
  mutate(length_pcode = str_length(post_code),
         post_code = toupper(post_code),
         number_digits = str_count(post_code, "[0-9]"),
         number_alphanum = str_count(post_code, "[A-Z]")) %>% 
  filter(number_digits >= 1 & number_alphanum >=1) %>% 
  mutate(post_code = gsub(' O', ' 0', post_code),
         post_code = gsub('C0', 'CO', post_code),
         post_code = gsub('S0', 'SO', post_code),
         post_code = ifelse(number_digits > 4, sub(" .*", "", post_code), post_code),
         post_code = gsub('ZZ.*', '', post_code),
         post_code = gsub(' ', '', post_code),
         half_post_code = gsub('.{3}$', '', post_code))

post_code_main_lookup = post_code_main_lookup %>%
  mutate(country = ifelse(startsWith(ccg, 'S'), 'Scotland', NA),
         country = ifelse(startsWith(ccg, 'E'), 'England', country),
         country = ifelse(startsWith(ccg, 'W'), 'Wales', country) %>% 
           factor(),
         pcds = gsub(' ', '', pcds),
         half_pcds = gsub('.{3}$', '', pcds)) %>% 
  group_by(country) %>% 
  mutate(imd_quintile = ntile(imd, 5)) %>% 
  select(pcds, half_pcds, country, ccg, imd, imd_quintile) %>% 
  ungroup()

post_code_supp_lookup = post_code_main_lookup %>% 
  group_by(half_pcds) %>% 
  mutate(median_imd_quintile = median(imd_quintile) %>% floor()) %>% 
  ungroup() %>% 
  distinct(half_pcds, .keep_all = T) %>% 
  select(-imd_quintile)

pcode_data = pcode_data %>% 
  left_join(post_code_main_lookup, by = c('post_code' = 'pcds')) %>% 
  left_join(post_code_supp_lookup, by = c('half_post_code' = 'half_pcds')) %>%
  mutate(imd = ifelse(is.na(imd.x), imd.y, imd.x)) %>% 
  mutate(imd_quintile = ifelse(is.na(imd_quintile), median_imd_quintile, imd_quintile)) %>% 
  select(study_id, imd, imd_quintile)  

phosp = phosp %>% 
  left_join(pcode_data %>% 
              select(study_id, imd, imd_quintile), by = 'study_id') %>% 
  mutate(
    imd_quintile = factor(imd_quintile,
                          levels = c("1", "2", "3", "4", "5"),
                          labels = c("1 - most deprived", "2", "3", "4", "5 - least deprived")
    ) %>% 
      ff_label("Index of multiple deprivation")
  )

rm(pcode_data, post_code_main_lookup, post_code_supp_lookup)

# SARS-CoV-2 status ---------------------------------------------------------------------
phosp_pcr = phosp %>% 
  filter(redcap_event_name == "Hospital Discharge") %>% 
  filter(redcap_repeat_instrument == "SARS-CoV-2 Swab PCR Test Result") %>% 
  select(study_id, swab_pcr_result) %>% 
  group_by(study_id) %>% 
  summarise(
    swab_pcr_result = ifelse(any(swab_pcr_result == "Positive", na.rm = TRUE), "Positive", 
                             ifelse(any(swab_pcr_result == "Indeterminate", na.rm = TRUE), "Indeterminate", 
                                    ifelse(any(swab_pcr_result == "Negative", na.rm = TRUE), "Negative", 
                                           NA))) %>% 
      factor()) %>%
  mutate( # Don't know why this required taken out of summarise. 
    swab_pcr_result = 
      fct_relevel(swab_pcr_result, "Negative", "Positive", "Indeterminate") %>% 
      ff_label("SARS-CoV-2 swab")
  )

phosp = phosp %>% 
  select(-swab_pcr_result) %>% 
  left_join(phosp_pcr) %>% 
  ff_relabel_df(phosp_pcr)

rm(phosp_pcr)

# Hospital discharge event only ----------------------------------------------------------
## This should be one row per patient, check below
phosp_hosp = phosp %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  filter(redcap_event_name == "Hospital Discharge") %>% 
  purrr::discard(~all(is.na(.)))

# Join back in admission only variables that cannot be filled --------------------------
phosp = phosp %>% 
  select(-c(no_comorbid, no_comorbid_2levels, no_comorbid_3levels)) %>% 
  left_join(phosp_hosp %>% select(study_id, no_comorbid, no_comorbid_2levels, no_comorbid_3levels))

# 6 week event only ----------------------------------------------------------
## This should be one row per patient, check below
phosp_6w = phosp %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  filter(redcap_event_name== "6 Weeks")%>% 
  purrr::discard(~all(is.na(.)))

# 3 month event only ----------------------------------------------------------
## This should be one row per patient, check below
## NOTE this date should likely be changed to crf3b_date_visit
phosp_3m = phosp %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  filter(redcap_event_name== "3 Months (1st Research Visit)") %>% 
  mutate(discharge_to_3m_review = (crf3b_date_visit - crf1a_date_dis) %>% as.numeric() %>% 
           ff_label("Discharge to review time (days)")) %>%  
  purrr::discard(~all(is.na(.)))

# Walk test instrument ---------------------------------------------------
phosp_wt = phosp %>% 
  filter(redcap_repeat_instrument == "Walk Test - 6 Min / ISWT 15 Level") %>%
  purrr::discard(~all(is.na(.))) %>% 
  left_join(phosp %>% select(study_id, crf3a_bmi) %>% drop_na()) %>% 
  
  # Make ISWT predicited and percent predicted
  mutate(
    wt_distance = parse_number(wt_distance), 
    
    wt_iswt_predicted = 1449.701 - 
      (11.735 * age_admission) + 
      (241.897 * (crf1a_sex %>% as.numeric() %>% {. - 2} %>% abs())) - # female = 0
      (5.686 * crf3a_bmi) %>% 
      ff_label("ISWT predicted"),
    
    wt_iswt_predicted_perc = (100 * wt_distance / wt_iswt_predicted) %>% 
      ff_label("ISWT % predicted"),
  ) %>% 
  ff_relabel_df(phosp)

# Define patients for 1st 1000 -  discharge before 30112020
study_id_before_end_nov = phosp %>% 
  filter(crf1a_date_dis <= ymd(20201130)) %>% 
  distinct(study_id) %>% 
  pull(study_id)

# 3 month symptoms -----------------------------------------------------------
psq_scale = c("psq_scale_blness_24hrs", "psq_scale_fatigue_24hrs", "psq_scale_sleep_24hrs", "psq_scale_cough_24hrs", 
              "psq_scale_pain_24hrs")
neuro = c("loss_of_sense_of_smell", "loss_of_taste", "confusion_fuzzy_head", "difficulty_with_communicat", 
          "difficulty_with_concentrat", "short_term_memory_loss", "physical_slowing_down", "slowing_down_in_your_think", "headache", 
          "altered_personality_behavi", "limb_weakness", "problems_with_balance", "can_t_move_and_or_feel_one", "problems_seeing", 
          "tingling_feeling_pins_and", "can_t_fully_move_or_contro", "tremor_shakiness", "seizures")
musculoskeletal = c("aching_in_your_muscles_pai", "joint_pain_or_swelling")
cardiorespiratory = c("leg_ankle_swelling", "chest_pain", "chest_tightness", "pain_on_breathing", "palpitations", 
                      "dizziness_or_lightheadness", "fainting_blackouts")
gastrointestinal_genitourinary = c("diarrhoea", "constipation", "nausea_vomiting", "abdominal_pain", "loss_of_appetite", 
                                   "loss_of_control_of_passing", "loss_of_control_of_opening", "weight_loss", "stomach_pain", 
                                   "psq_symp_ed", "skin_rash", "lumpy_lesions_purple_pink", "bleeding")
psq = c("psq_balance_q1_since", "psq_balance_q2_since")
tinnitus = "psq_tinnitus_since"

all_symptoms = c(neuro, musculoskeletal, cardiorespiratory, gastrointestinal_genitourinary, psq_scale, psq, tinnitus)

# NOTE, the way this works, all missing is defined for the 48 variables listed above. 
# If variables added or removed, then the code below will need modified to catch "all missing". 

phosp_3m = phosp_3m %>% 
  select(study_id, all_symptoms) %>% 
  mutate(
    across(psq_scale,  ~ if_else(. >= 3, 1, 0)),
    across(c(neuro, musculoskeletal, cardiorespiratory, gastrointestinal_genitourinary, psq), ~ if_else(. == "Yes", 1, 0)),
    psq_tinnitus_since = case_when(
      psq_tinnitus_since %in% c("Yes, most or all of the time", 
                                "Yes, a lot of the time", 
                                "Yes, some of the time") ~ 1,
      is.na(psq_tinnitus_since) ~ NA_real_,
      TRUE ~ 0
    )
  ) %>% 
  mutate(
    symptom_count = rowSums(select(., -study_id), na.rm = TRUE) %>% 
      ff_label("Symptom count"),
    symptom_missing = rowSums(select(., -study_id) %>% is.na()),
    symptom_count = if_else(symptom_missing == 48, NA_real_, symptom_count),
    symptom_any = if_else(symptom_count > 0, "Yes", "No") %>% 
      ff_label("Any symptom at 3 months (%)")) %>% 
  select(study_id, symptom_count, symptom_any) %>% 
  left_join(phosp_3m, by = "study_id") %>% 
  ff_relabel_df(phosp_3m)


# Temp out for testing -----------------------------------------------
# saveRDS(phosp, "phosp.rds")
# saveRDS(data, "data.rds")
# saveRDS(phosp_hosp, "phosp_hosp.rds")
# saveRDS(phosp_6w, "phosp_6w.rds")
# saveRDS(phosp_3m, "phosp_3m.rds")
# save(study_id_before_end_nov, file = "helpers.rda")

