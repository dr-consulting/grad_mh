# Exploration of continuous measure distribution 
library(tidyverse)
library(brms)
library(glue)

# Base modeling script - starting off with simple analysis
DATA_VERSION <- "2021-02-04"

source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)
load("{DATA_DIR}/ACHA-II/acha_grad_students_base_{DATA_VERSION}.RData" %>% glue::glue())

neg_vars <- c("Q30A_hopeless_r", "Q30D_lonely_r", "Q30E_sad_r", "Q30F_depressed_r", "Q30G_anxiety_r", "Q30H_anger_r")
neg_vars_df <- grads_model_base %>%
    select(all_of(c('school_id', neg_vars))) %>% 
    as.data.frame() %>% 
    mutate_all(as.numeric)

neg_vars_df[["neg_emo_avg"]] <- rowMeans(neg_vars_df[neg_vars], na.rm = TRUE)

ucm_neg_emo_form_lgnrml <- "neg_emo_avg ~ 1 + (1|school_id)" %>% bf() + brms::lognormal()
ucm_neg_emo_form_gamma <- "neg_emo_avg ~ 1 + (1|school_id)" %>% bf() + Gamma(link = "log")


fit_lgnrml <- brm(
    ucm_neg_emo_form_lgnrml, 
    data = neg_vars_df,
	cores = 3, 
	chains = 3, 
    iter = 7500, 
    warmup = 5000, 
    control = list(adapt_delta = .95)
)


start_time <- Sys.time()
fit_gamma <- brm(
    ucm_neg_emo_form_gamma, 
    data = neg_vars_df,
	cores = 3, 
	chains =3, 
    iter = 7500, 
    warmup = 5000, 
    control = list(adapt_delta = .95)
)
gamma_run_time <- difftime(Sys.time(), start_time, units = "mins")
