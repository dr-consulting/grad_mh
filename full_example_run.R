# Full Analysis - cutting off at n > 4 for observations
library(tidyverse)
library(glue)
library(brms)

source("~/deyoung_thesis/rscripts/project_config.R")

start_time <- Sys.time()
load(DATA_FILEPATH)

yvar <- 'ANY_SAMHSA_diagnosis'
time_structure <- 'Time + I(Time^2)'
covariates <- 'pwno + Q52_enrollment + Q46_age + Q55_international + Q47_gender_3cat + RACE_ethn_recoded + PUBPRIV'
rand_effects <- '(1 + Time | PERMID)'

model_formula <- '{yvar} ~ 1 + {time_structure} + {covariates} + {rand_effects}' %>% 
    glue() %>% 
    brmsformula() +
    bernoulli()

dat_5_plus <- dat_grads_model %>% 
    select(Time, PERMID, ANY_SAMHSA_diagnosis) %>% 
    group_by(Time, PERMID) %>% 
    summarize(
        n_obs = n()
    ) %>% 
    inner_join(dat_grads_model, by = c('PERMID', 'Time')) %>% 
    filter(n_obs >= 5) %>% 
    select(Time, PERMID, ANY_SAMHSA_diagnosis, pwno, Q52_enrollment, Q46_age, Q55_international, Q47_gender_3cat, 
           RACE_ethn_recoded, PUBPRIV)

fit_complete <- brm(
    model_formula, 
    data = dat_5_plus, 
    cores = 2, 
    chains = 2, 
    warmup = 5000, 
    iter = 7500, 
    control = list(adapt_delta = .95), 
    save_all_pars = TRUE
)

run_time <- Sys.time() - start_time
summary(fit_complete) %>% print(5)

save(list = c('fit_complete', 'dat_5_plus', 'run_time'), 
     file = 'prelim_{yvar}_full_results_5_plus.RData' %>% glue())