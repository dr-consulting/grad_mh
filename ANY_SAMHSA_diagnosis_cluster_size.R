# Testing effects of setting a case threshold
library(doParallel)
library(magrittr)
library(brms)
source("~/deyoung_thesis/rscripts/project_config.R")

load(DATA_FILEPATH)

formula_list <- list(linear_fixed = "ANY_SAMHSA_diagnosis ~ 1 + Time + (1 | PERMID)" %>% 
                         brmsformula() + 
                         bernoulli(), 
                     linear_rand = "ANY_SAMHSA_diagnosis ~ 1 + Time + (1 + Time | PERMID)" %>% 
                         brmsformula() + 
                         bernoulli(),
                     quadratic_fixed = "ANY_SAMHSA_diagnosis ~ 1 + Time + I(Time^2) + (1 + Time | PERMID)"%>% 
                         brmsformula() + 
                         bernoulli(),
                     quadratic_rand = "ANY_SAMHSA_diagnosis ~ 1 + Time + I(Time^2) + (1 + Time + I(Time^2) | PERMID)"%>% 
                         brmsformula() + 
                         bernoulli())

cl <- makeCluster(4)
doParallel::registerDoParallel(cl)
foreach(
    i = seq_along(formula_list), 
    .export = c('formula_list', 'dat_grads_model'), 
    .packages = c('brms', 'tidyverse', 'glue')
) %dopar% {

    start <- Sys.time()
    
    # Standard - no exclusion criteria
    fit_base <- brm(
        formula_list[[i]], 
        data = dat_grads_model, 
        cores = 2, 
        chains = 2, 
        warmup = 3000, 
        iter = 4500, 
        control = list(adapt_delta = .95), 
        save_all_pars = TRUE
    )
    
    run_time_base <- Sys.time() - start
    
    dat_1_plus <- dat_grads_model %>% 
        select(Time, PERMID, ANY_SAMHSA_diagnosis) %>% 
        group_by(Time, PERMID) %>% 
        summarize(
            n_obs = n()
        ) %>% 
        inner_join(dat_grads_model, by = c('PERMID', 'Time')) %>% 
        filter(n_obs > 1) %>% 
        select(Time, PERMID, ANY_SAMHSA_diagnosis)
    
    start <- Sys.time()
    
    # Kick out all observations with just one data point
    fit_1_plus <- brm(
        formula_list[[i]], 
        data = dat_1_plus, 
        cores = 2, 
        chains = 2, 
        warmup = 3000, 
        iter = 4500, 
        control = list(adapt_delta = .95), 
        save_all_pars = TRUE
    )
    
    run_time_1_plus <- Sys.time() - start
    
    # Kick out all observations with fewer than 5 data points
    dat_5_plus <- dat_grads_model %>% 
        select(Time, PERMID, ANY_SAMHSA_diagnosis) %>% 
        group_by(Time, PERMID) %>% 
        summarize(
            n_obs = n()
        ) %>% 
        inner_join(dat_grads_model, by = c('PERMID', 'Time')) %>% 
        filter(n_obs >= 5) %>% 
        select(Time, PERMID, ANY_SAMHSA_diagnosis)

    start <- Sys.time()
    
    fit_5_plus <- brm(
        formula_list[[i]], 
        data = dat_5_plus, 
        cores = 2, 
        chains = 2, 
        warmup = 3000, 
        iter = 4500, 
        control = list(adapt_delta = .95), 
        save_all_pars = TRUE
    )
    
    run_time_5_plus <- Sys.time() - start
    
    save(list = c('fit_base', 'fit_1_plus', 'fit_5_plus', 
                  'run_time_base', 'run_time_1_plus', 'run_time_5_plus'), 
         file = '~/count_test/{names(formula_list)[i]}_ouput.RData' %>% glue())
}

stopCluster(cl)


dat_5_test <- dat_grads_model %>% 
    select(Time, PERMID, ANY_SAMHSA_diagnosis) %>% 
    group_by(Time, PERMID) %>% 
    summarize(
        n_obs = n()
    ) 

hist(dat_5_test$n_obs)

g1 <- 
dat_5_test %>% 
    ggplot(aes(x=n_obs)) +
    geom_histogram() +
    labs(title = "Histogram All Observations")
    
g2 <- 
dat_5_test %>% 
    filter(n_obs > 1) %>% 
    ggplot(aes(x=n_obs)) +
    geom_histogram() +
    labs(title = 'Histogram > 1')

g3 <- 
dat_5_test %>% 
    filter(n_obs > 4) %>% 
    ggplot(aes(x=n_obs)) +
    geom_histogram() +
    labs(title = 'Histogram 5+')

cowplot::plot_grid(g1, g2, g3, nrow =3)
