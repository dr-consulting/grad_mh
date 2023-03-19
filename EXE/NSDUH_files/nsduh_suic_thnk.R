BASE_FILE <- '~/Desktop/grad_mh/project_config.R'
# If missing the config file raise early.
# Likely just opened the repo in a different file system
if(!file.exists(BASE_FILE)){
    stop('ERROR: Missing project config file. {BASE_FILE}' %>% glue())
}

source(BASE_FILE)

load("{DATA_DIR}/NSDUH/nsduh_study_data.RData" %>% glue())

# Drop the extra data to reduce memory burden
remove_objects <- ls()[stringr::str_starts(ls(), 'NSDUH_.*_df_list')]
remove(list = remove_objects)

sapply(list.files(R_DIR, full.names = TRUE), source)

# Note may require specific transforms in the tidy data creation step below. 
y_var <- "suic_thnk_12mos"
SAVE_FILEPATH <- '{POSTERIOR_OUTPUTS}/nsduh_{y_var}.RData' %>% glue()

# Apply the adults filter, Ensure correct mapping of yes and no responses
nsduh_adults_df <- nsduh_study_df %>%
    filter(adult_mask == 1) %>% 
    mutate(
        quad_c_Time = as.numeric(c_Time)^2, 
        suic_thnk_12mos = forcats::fct_relevel(suic_thnk_12mos, 'No', 'Yes')
    )

brms_form <- '{y_var} | weights(weights) ~ c_Time + quad_c_Time' %>% 
    glue() %>% 
    brmsformula() + bernoulli()

priors_config <- c(set_prior('normal(0, 3)', class='Intercept'), 
                   set_prior('normal(0, 3)', class='b'))

start_time <- Sys.time()

brms_fit <- brm(
    brms_form, 
    data = nsduh_adults_df, 
    prior = priors_config,
    iter = 7500, 
    warmup = 5000, 
    control = list(adapt_delta = .95), 
    cores = 3, 
    chains = 3
)

results_list <- list(
    brms_formula = brms_form, 
    run_time = difftime(Sys.time(), start_time, units='hours'), 
    brms_fit = brms_fit
)

save(list = c('results_list'), 
     file = SAVE_FILEPATH)
