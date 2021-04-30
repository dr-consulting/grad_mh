library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)

# Loading first so I can kick out the unneeded data sets from each year
load('~/github/ATNL/grad_mh/data/NSDUH/nsduh_study_data.RData')
remove(list = ls()[ls() != 'nsduh_study_df'])

source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)

# Note may require specific transforms in the tidy data creation step below. 
y_var <- "suic_try_12mos"
SAVE_FILEPATH <- '{POSTERIOR_OUTPUTS}/nsduh_{y_var}.RData' %>% glue()

# Create a centered time variable
nsduh_adults_df <- nsduh_study_df %>%
    filter(adult_mask == 1) %>% 
    mutate(
        quad_c_Time = as.numeric(c_Time)^2, 
        suic_try_12mos = forcats::fct_relevel(suic_try_12mos, 'No', 'Yes')
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
    iter = 4000, 
    warmup = 2500, 
    control = list(adapt_delta = .99), 
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