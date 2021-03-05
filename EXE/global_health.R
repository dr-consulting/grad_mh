# Base modeling script - starting off with simple analysis
DATA_VERSION <- "2021-02-04"

source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)
load("{DATA_DIR}/ACHA-II/acha_grad_students_base_{DATA_VERSION}.RData" %>% glue::glue())

grads_model_base <- grads_model_base %>% 
    mutate(
        global_health_ord = ordered(global_health_r)
    )

id_var <- "school_id"
y_var <- "global_health_ord"
lv1_vars <- c('c_Time', 'quad_c_Time', 'c_Q46_age', 'Q47_gender', 'race_ethn', 'Q52_enrollment', 'Q55_international', 
              'survey_method')
lv1_ran_vars <- "c_Time"
lv2_int_vars <- c('school_size', 'public_schl')

model <- "
    {y_var} ~ {paste(c(1, lv1_vars), collapse=' + ')} +
    ({paste(c(1, lv1_ran_vars), collapse=' + ')} | {id_var}) +
    {paste(lv2_int_vars, collapse=' + ')} 
    " %>% glue::glue() %>% 
    brms::bf() + brms::cumulative(link = "logit", threshold = "flexible")

# Creating a set of weakly informative priors for the cumulative model 
# The intent here is provide some mild boundaries with the expectation that the size of data should definitely swamp the
# priors. Some limited experimentation yielded these weakly informative priors: 
prior_config <- brms::get_prior(model, grads_model_base)
prior_config <- prior_config %>% 
    mutate(
        prior = ifelse(class == "b", "normal(0, 3)", prior), 
        prior = ifelse(class == "Intercept", "normal(0, 3)", prior), 
        prior = ifelse(class == "sd", "lognormal(0, 1)", prior)
    )

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

res <- cumulative_model_wrapper(
    data = grads_model_base, 
    prior_config = prior_config,
    y_var = y_var, 
    id_var = id_var, 
    lv1_vars = lv1_vars, 
    lv1_ran_vars = lv1_ran_vars, 
    lv2_int_vars = lv2_int_vars, 
    output_folder = POSTERIOR_OUTPUTS, 
    warmup = 5000, 
    iter = 7500, 
    chains = 3, 
    control_list = list(adapt_delta = .95), 
    model_save_name = "global_health_full_covariate"
)
