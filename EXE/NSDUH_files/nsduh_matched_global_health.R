library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)

BASE_FILE <- '~/Desktop/grad_mh/project_config.R'
DATA_VERSION <- '2023-01-15'
# If missing the config file raise early.
# Likely just opened the repo in a different file system
if(!file.exists(BASE_FILE)){
    stop('ERROR: Missing project config file. {BASE_FILE}' %>% glue())
}

source(BASE_FILE)
sapply(list.files(R_DIR, full.names = TRUE), source)

# Loading first so I can kick out the unneeded data sets from each year
load('{DATA_DIR}/NSDUH/nsduh_matched_study_data_{DATA_VERSION}.RData' %>% glue())

# Note may require specific transforms in the tidy data creation step below. 
y_var <- "global_health_dich"
SAVE_FILEPATH <- '{POSTERIOR_OUTPUTS}/nsduh_matched_{y_var}.RData' %>% glue()

# Create a centered time variable
nsduh_matched_df <- nsduh_matched_df %>%
    mutate(
        quad_c_Time = as.numeric(c_Time)^2, 
        global_health_dich = fct_collapse(
            global_health,
            # Note the ordering is intentional - want focal condition to be poor health
            good = c('Good', 'Very good', 'Excellent'),
            poor = c('Poor', 'Fair')
        )
    )

brms_form <- '{y_var} | weights(normalized_weights) ~ c_Time + quad_c_Time' %>% 
    glue() %>% 
    brmsformula() + bernoulli()

priors_config <- c(set_prior('normal(0, 3)', class='Intercept'), 
                   set_prior('normal(0, 3)', class='b'))

start_time <- Sys.time()

brms_fit <- brm(
    brms_form, 
    data = nsduh_matched_df, 
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