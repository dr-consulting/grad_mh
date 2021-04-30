library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)

DATA_VERSION <- "2021-02-04"

source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)
load("{DATA_DIR}/ACHA-II/acha_grad_students_base_{DATA_VERSION}.RData" %>% glue::glue())

ovrwhlm_vars <- c('Q30B_overwhelmed_r', 'Q30C_exhausted_r') %>% 
    paste(sep = '_', '2wks')

# Explicitly choosing to drop everyone that has at least one missing - don't want to count as valid 0's in a count 
grads_model_base[['ovrwhlm_cnt']] <- rowSums(grads_model_base[,ovrwhlm_vars])

id_var <- "school_id"
y_var <- "ovrwhlm_cnt"
lv1_vars <- c('c_Time', 'quad_c_Time', 'c_Q46_age', 'Q47_gender', 'race_ethn', 'Q52_enrollment', 'Q55_international', 
              'survey_method')
lv2_int_vars <- c('school_size', 'public_schl')

# Simple tests to ensure required variables are present
testthat::expect_true(
    id_var %in% names(grads_model_base)
)

testthat::expect_true(
    y_var %in% names(grads_model_base)
)

testthat::expect_true(
    all(lv1_vars %in% names(grads_model_base))
)

testthat::expect_true(
    all(lv1_vars %in% names(grads_model_base))
)

testthat::expect_true(
    all(lv2_int_vars %in% names(grads_model_base))
)

res <- binomial_model_wrapper(
    data = grads_model_base, 
    prior_config = PRIOR_CONFIG,
    y_var = y_var, 
    trials = length(ovrwhlm_vars),
    id_var = id_var, 
    lv1_vars = lv1_vars, 
    lv1_ran_vars = 'c_Time', 
    lv2_int_vars = lv2_int_vars, 
    output_folder = POSTERIOR_OUTPUTS, 
    warmup = 5000, 
    iter = 7500, 
    chains = 3, 
    control_list = list(adapt_delta = .95), 
    model_save_name = "ovrwhlm_cnt_full_covariate"
)