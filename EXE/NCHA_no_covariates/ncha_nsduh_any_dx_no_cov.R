# Base modeling script - starting off with simple analysis
DATA_VERSION <- "2023-04-15"

BASE_FILE <- '~/grad_mh/project_config.R'

# If missing the config file raise early.
# Likely just opened the repo in a different file system
if(!file.exists(BASE_FILE)){
    stop(paste('ERROR: Missing project config file.', BASE_FILE))
}

source(BASE_FILE)

sapply(list.files(R_DIR, full.names = TRUE), source)

load("{DATA_DIR}/ACHA-II/acha_grad_students_base_{DATA_VERSION}.RData" %>% glue::glue())

id_var <- "school_id"
y_var <- "any_dx_nsduh"
lv1_vars <- c('c_Time', 'quad_c_Time')
lv2_int_vars <- NULL

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

res <- logistic_model_wrapper(
    data = grads_model_base, 
    prior_config = PRIOR_CONFIG,
    y_var = y_var, 
    id_var = id_var, 
    lv1_vars = lv1_vars, 
    lv1_ran_vars = 'c_Time', 
    lv2_int_vars = lv2_int_vars, 
    output_folder = POSTERIOR_OUTPUTS, 
    warmup = 5000, 
    iter = 7500, 
    chains = 3, 
    control_list = list(adapt_delta = .95), 
    model_save_name = "dx_nsduh_any_no_cov",
    chain_hyperthreading = TRUE
)
