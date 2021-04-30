# Base modeling script - starting off with simple analysis
DATA_VERSION <- "2021-02-04"

source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)
load("{DATA_DIR}/ACHA-II/acha_grad_students_base_{DATA_VERSION}.RData" %>% glue::glue())

lv1_vars <- c('c_Time', 'quad_c_Time', 'c_Q46_age', 'Q47_gender', 'race_ethn', 'Q52_enrollment', 'Q55_international', 
              'survey_method')
lv2_int_vars <- c('school_size', 'public_schl')
id_var <- "school_id"
y_var <- 'neg_emo_avg'

neg_vars <- c("Q30A_hopeless_r", "Q30D_lonely_r", "Q30E_sad_r", "Q30F_depressed_r", "Q30G_anxiety_r", "Q30H_anger_r")
neg_vars_df <- grads_model_base %>%
    select(all_of(c('school_id', neg_vars))) %>% 
    as.data.frame() %>% 
    mutate_all(as.numeric)

neg_vars_df[[y_var]] <- rowMeans(neg_vars_df[neg_vars], na.rm = TRUE)

# bind back to relevant covariates from base data set
neg_vars_df <- cbind(neg_vars_df, grads_model_base[, c(lv1_vars, lv2_int_vars)])

# Simple tests to ensure required variables are present
testthat::expect_true(
    id_var %in% names(neg_vars_df)
)

testthat::expect_true(
    y_var %in% names(neg_vars_df)
)

testthat::expect_true(
    all(lv1_vars %in% names(neg_vars_df))
)

testthat::expect_true(
    all(lv1_vars %in% names(neg_vars_df))
)

testthat::expect_true(
    all(lv2_int_vars %in% names(neg_vars_df))
)

# Setting up multiple models
base_formula <- "
    {y_var} ~ {paste(c(1, lv1_vars), collapse=' + ')} +
    ({paste(c(1, 'c_Time'), collapse=' + ')} | {id_var}) +
    {paste(lv2_int_vars, collapse=' + ')} 
    " %>% glue::glue() %>% 
    brms::bf()

neg_emo_form_lgnrml <- base_formula + brms::lognormal()
neg_emo_form_gamma <- base_formula + Gamma(link = "log")

start_time <- Sys.time()
fit_lgnrml <- brm(
    neg_emo_form_lgnrml, 
    data = neg_vars_df,
    cores = 3, 
    chains = 3, 
    iter = 7500, 
    warmup = 5000, 
    control = list(adapt_delta = .95)
)

lgnrml_run_time <- difftime(Sys.time(), start_time, units = "mins")

start_time <- Sys.time()
fit_gamma <- brm(
    neg_emo_form_gamma, 
    data = neg_vars_df,
    cores = 3, 
    chains =3, 
    iter = 7500, 
    warmup = 5000, 
    control = list(adapt_delta = .95)
)

gamma_run_time <- difftime(Sys.time(), start_time, units = "mins")

results_list <- list(model_lgnrml = fit_lgnrml, 
                     model_gamma = fit_gamma, 
                     formula_lgnrml = neg_emo_form_lgnrml, 
                     formula_gamma = neg_emo_form_gamma, 
                     run_time_lgnmrl = lgnrml_run_time, 
                     run_time_gamma = gamma_run_time)

save(list=c('results_list'), 
     file='{POSTERIOR_OUTPUTS}/negative_emotion_composite.RData' %>% glue())

