# Base modeling script - starting off with simple analysis
DATA_VERSION <- "2021-01-15"

source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)
load("{DATA_DIR}/ACHA-II/acha_grad_students_base_{DATA_VERSION}.RData" %>% glue::glue())

res <- logistic_model_wrapper(
    data = grads_model_base, 
    y_var = "any_dx_nsduh", 
    id_var = "school_id", 
    lv1_vars = c('c_Time', 'quad_c_Time', 'c_Q46_age', 'Q47_gender', 'race_ethn', 'Q52_enrollment', 'Q55_international', 
                 'survey_method'), 
    lv1_ran_vars = 'c_Time', 
    lv2_int_vars = c('school_size', 'public_schl'), 
    output_folder = POSTERIOR_OUTPUTS, 
    warmup = 3000, 
    iter = 5000, 
    chains = 3, 
    future_arg = FALSE, 
    control_list = list(adapt_delta = .95), 
    model_save_name = "dx_nsduh_any_full_covariate"
)