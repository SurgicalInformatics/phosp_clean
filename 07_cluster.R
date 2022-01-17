# PHOSP-COVID analysis: Add cluster and predict new patients
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2022

library(tidyverse)
library(xgboost)
library(recipes)
library(rsample)
library(yardstick)

# Clusters from 06-10-2021 analysis
phosp_cluster  = readRDS("/home/common/phosp/share/phosp_cluster_2021-10-06_0400.rds")

# Patients in original cluster analysis --------------------------------------------------
study_id_cluster = phosp_cluster %>% 
  pull(study_id)

# Join cluster ----------------------------------------------------------------------------
phosp_xg = phosp %>% 
  left_join(phosp_cluster)

explanatory_hrqol = c(
  "gad7_summary",
  "phq9_summary",
  "pcl5_summary",
  "dyspnoea12_summary",
  "facit_v4_summary",
  "sppb_score",
  "mocal_total"
)

# Cluster data for 3 months only. 
phosp_xg = phosp_xg %>% 
  filter(redcap_event_name == "3 Months (1st Research Visit)") %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  select(study_id, explanatory_hrqol, y = cluster_numeric) %>% 
  mutate(y = y - 1) # Requirement for multiclass xgboost to start at zero

# XGBoost data prep
phosp_xg_prep = phosp_xg %>% 
  drop_na(y) %>% 
  recipe(y ~ .) %>% 
  update_role(study_id, new_role = "id variable") %>%
  step_normalize(all_numeric(), -y) %>% 
  prep()

phosp_xg_train = phosp_xg_prep %>% 
  bake(new_data = NULL)

# Split test/training sets
set.seed(100)
train_test_split = initial_split(phosp_xg_train, prop = 0.8)
train_test_split

## Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split) 


# Random grid search to optimise hyperparameters ---------------------------------------------------
## Takes a while so run once. 

# --
# ## xgb.DMatrix objects
# dtrain  = train_tbl %>% 
#   select(-y, -study_id) %>% 
#   as.matrix() %>% 
#   xgb.DMatrix(label = train_tbl$y)
# 
# dvalid <- test_tbl %>% 
#   select(-y, -study_id) %>% 
#   as.matrix() %>% 
#   xgb.DMatrix(label = test_tbl$y)
# 
# # Create empty lists
# lowest_error_list = list()
# parameters_list = list()
# 
# # Create 10,000 rows with random hyperparameters
# set.seed(20)
# for (iter in 1:10000){
#   param <- list(
#     objective = "multi:softmax",
#     max_depth = sample(3:10, 1),
#     eta = runif(1, .01, .3),
#     subsample = runif(1, .7, 1),
#     colsample_bytree = runif(1, .6, 1),
#     min_child_weight = sample(0:10, 1)
#   )
#   parameters <- as.data.frame(param)
#   parameters_list[[iter]] <- parameters
# }
# 
# # Create object that contains all randomly created hyperparameters
# parameters_df = do.call(rbind, parameters_list)
# 
# # Use randomly created parameters to create 10,000 XGBoost-models
# for (row in 1:nrow(parameters_df)){
#   set.seed(20)
#   mdcv <- xgb.train(data = dtrain, 
#                     objective = "multi:softmax",
#                     max_depth = parameters_df$max_depth[row],
#                     eta = parameters_df$eta[row],
#                     subsample = parameters_df$subsample[row],
#                     colsample_bytree = parameters_df$colsample_bytree[row],
#                     min_child_weight = parameters_df$min_child_weight[row],
#                     nrounds= 500,
#                     num_class = 4,
#                     eval_metric = "mlogloss",
#                     early_stopping_rounds= 30,
#                     print_every_n = 100,
#                     watchlist = list(train= dtrain, val= dvalid)
#   )
#   lowest_error <- as.data.frame(min(mdcv$evaluation_log$val_mlogloss))
#   lowest_error_list[[row]] <- lowest_error
# }
# 
# # Create object that contains all accuracy's
# lowest_error_df = map_df(lowest_error_list, bind_rows) %>% 
#   rename(logloss = 1)
# 
# # Bind columns of accuracy values and random hyperparameter values
# randomsearch = bind_cols(lowest_error_df, parameters_df)
# 
# # Quickly display highest accuracy
# parameters_random_search = randomsearch %>% 
#   dplyr::filter(logloss == min(logloss)) %>% 
#   select(-logloss) %>% 
#   as.list()
# 
# saveRDS(parameters_random_search, "parameters_random_search.rds")
# --
parameters_random_search = readRDS("/home/eharrison/phosp_clean/parameters_random_search.rds")

# Demonstrate performance --------------------------------------------------------------
## Train
xg_train = train_tbl %>% 
  select(-y, -study_id) %>% 
  as.matrix() %>% 
  xgboost(params = parameters_random_search,
          label = train_tbl$y, 
          eval_metric = "mlogloss", 
          nrounds = 1500, 
          num_class = 4,
          print_every_n = 1000,
          early_stopping_rounds = 50)

out = tibble(
  obs = factor(test_tbl$y),
  pred = predict(xg_train, test_tbl %>% 
          select(-y, -study_id) %>% 
          as.matrix()) %>% factor
)

conf_mat(out, obs, pred) %>% 
  summary()

# Final model trained on all data ---------------------------------------------------
xg_train = phosp_xg_train %>% 
  select(-y, -study_id) %>% 
  as.matrix() %>% 
  xgboost(params = parameters_random_search,
          label = phosp_xg_train$y, 
          eval_metric = "mlogloss", 
          nrounds = 1500, 
          num_class = 4,
          print_every_n = 1000,
          early_stopping_rounds = 50)

# Predict cluster membership for patients not in primary cluster analysis ----------
# Cluster data for 3 months only. 
phosp_xg_predict = phosp %>% 
  filter(redcap_event_name == "3 Months (1st Research Visit)") %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  select(study_id, explanatory_hrqol) %>% 
  filter(!study_id %in% phosp_xg_train$study_id)

phosp_xg_predict_y = phosp_xg_predict %>% 
  select(-study_id) %>% 
  bake(phosp_xg_prep, new_data = .) %>% 
  as.matrix() %>% 
  predict(xg_train, .)

# Trouble is xgboost gives classification for everyone, even when no data. 
phosp_cluster_parameters = phosp %>% 
  filter(redcap_event_name == "3 Months (1st Research Visit)") %>% 
  filter(is.na(redcap_repeat_instance)) %>%
  select(study_id, explanatory_hrqol) %>% 
  mutate(
    cluster_predicted_on_parameter_count_max_7 = rowSums(select(., -study_id) %>% 
                     mutate(across(everything(), ~ !is.na(.))))
  ) %>% 
  select(study_id, cluster_predicted_on_parameter_count_max_7)

# Bring all together
phosp_cluster = phosp_xg_predict %>% 
  select(study_id) %>% 
  mutate(cluster_numeric = phosp_xg_predict_y + 1) %>% # Back to original scale 1:4
  mutate(cluster = factor(cluster_numeric) %>% 
           fct_recode(
             "1: Very severe" = "1", 
             "2: Severe" = "2", 
             "3: Moderate/cognitive" = "3",
             "4: Mild" = "4"
           ),
         cluster_from_primary_analysis = "No"
  ) %>% 
  bind_rows(
    phosp_cluster %>%
      mutate(
        cluster_from_primary_analysis = "Yes"
      )
  ) %>% 
  left_join(phosp_cluster_parameters)

# Edit cluster assignment here - needs discussions with broader group
phosp_cluster = phosp_cluster %>% 
  mutate(
    cluster_numeric = case_when(
      cluster_from_primary_analysis == "No" & cluster_predicted_on_parameter_count_max_7 >= 4 ~ cluster_numeric,
      cluster_from_primary_analysis == "Yes" ~ cluster_numeric,
      TRUE ~ NA_real_),
    cluster = as.character(cluster), # sigh
    cluster = ifelse(is.na(cluster_numeric), NA_character_, cluster) %>% factor()
  )

# Bind new cluster object to main dataset
phosp = phosp %>% 
  left_join(phosp_cluster)

rm(out, parameters_random_search, phosp_cluster, phosp_xg, phosp_xg_predict, 
   phosp_xg_prep, phosp_xg_train, test_tbl, train_tbl, train_test_split, xg_train,
   phosp_xg_predict_y,
   study_id_cluster,
   explanatory_hrqol)


# New variables
# cluster_numeric
# cluster
# cluster_from_primary_analysis


#     
# Not used ----------------------------------------------------------------
# Crossvalidation
# xg_train_cv = phosp_xg_train %>% 
#   select(-y, -cluster, -study_id) %>% 
#   as.matrix() %>% 
#   xgb.cv(data = ., 
#          label = phosp_xg_train$y, 
#          # max_depth = 20, eta = 0.05, nthread = 24, 
#          eval_metric = "mlogloss", 
#          nrounds = 1500, 
#          nfold = 5,
#          num_class = 4,
#          objective = "multi:softmax")
# print(xg_train_cv)
# 
# elog <- as.data.frame(xg_train_cv$evaluation_log)
# nrounds <- which.min(elog$test_mlogloss_mean)


