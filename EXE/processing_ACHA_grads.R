# Be sure to update the data version to the appropriate date before running
DATA_VERSION <- "2021-01-15"

library(glue)
library(tidyverse)
library(haven)
source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)
load('{DATA_DIR}/ACHA-II/acha_study_data_{DATA_VERSION}.RData' %>% glue())

grads_model_base <- grads_only_study_df %>% 
    # filter initially to get a proper count of total grad students
    filter(Q52_enrollment == "Full-time" | Q52_enrollment == "Part-time") %>% 
    filter(school_type == "4-year") %>%
    # get grad student count per school, per survey and filter
    group_by(school_id, survey_period) %>% 
    summarize(post_filter_total = n()) %>% 
    left_join(., grads_only_study_df) %>%
    filter(post_filter_total >= 5) %>% 
    # Repeat the filter steps here after joining back to the original data.frame 
    filter(Q52_enrollment == "Full-time" | Q52_enrollment == "Part-time") %>% 
    filter(school_type == "4-year") %>%
    ungroup() %>% 
    # Create the set of 2 weeks, 30 days, and 12 months variables
    mutate(
        across(starts_with('Q30') & ends_with('_r'), 
               .fns = list(two_weeks = ~ifelse(. == 'Yes, in the last 2 weeks', 1, 0)), 
               .names = "{col}_2wks"), 
        across(starts_with('Q30') & ends_with('_r'), 
               .fns = list(thirty_days = ~ifelse(. == 'Yes, in the last 2 weeks' | 
                                                     . == 'Yes, in the last 30 days', 1, 0)), 
               .names = "{col}_30days"), 
        across(starts_with('Q30') & ends_with('_r'), 
               .fns = list(year = ~ifelse(. == 'Yes, in the last 2 weeks' | 
                                              . == 'Yes, in the last 30 days' |
                                              . == 'Yes, in the last 12 months', 1, 0)), 
               .names = "{col}_12mos")
    )

# NSDUH "total" dx and "any" dx columns
total_dx_nsduh_cols <- grads_model_base %>% 
    select(starts_with('Q31') & ends_with("_dich")) %>% 
    select(-contains("substance"), -contains("adhd")) %>% 
    names()

grads_model_base[["total_dx_nsduh"]] <- rowSums(grads_model_base[total_dx_nsduh_cols], na.rm = TRUE)
grads_model_base[["total_dx_nsduh"]] <- ifelse(rowSums(is.na(grads_model_base[total_dx_nsduh_cols])) == length(total_dx_nsduh_cols), 
                                               NA, grads_model_base[["total_dx_nsduh"]])
grads_model_base[["any_dx_nsduh"]] <- ifelse(grads_model_base[["total_dx_nsduh"]] > 0, 1, 0)

# NHCA "total" dx and "any" dx columns
total_dx_ncha_cols <- grads_model_base %>% 
    select(starts_with('Q31') & ends_with("_dich")) %>% 
    names()

grads_model_base[["total_dx_ncha"]] <- rowSums(grads_model_base[total_dx_ncha_cols], na.rm = TRUE)
grads_model_base[["total_dx_ncha"]] <- ifelse(rowSums(is.na(grads_model_base[total_dx_ncha_cols])) == length(total_dx_ncha_cols), 
                                               NA, grads_model_base[["total_dx_ncha"]])
grads_model_base[["any_dx_ncha"]] <- ifelse(grads_model_base[["total_dx_ncha"]] > 0, 1, 0)

# Add quadratic time slp 
grads_model_base[["quad_c_Time"]] <- grads_model_base[["c_Time"]]^2

# Add a centered value for age
grads_model_base[["c_Q46_age"]] <- grads_model_base[["Q46_age"]] - mean(grads_model_base[["Q46_age"]], na.rm = TRUE)

write.csv(grads_model_base,
          "{DATA_DIR}/ACHA-II/grad_students_study_base_{DATA_VERSION}.csv" %>% glue(), 
          row.names = FALSE)

write_sav(grads_model_base,
          "{DATA_DIR}/ACHA-II/grad_students_study_base_{DATA_VERSION}.sav" %>% glue())

save(list = c("grads_model_base", "DATA_VERSION"), 
     file = "{DATA_DIR}/ACHA-II/acha_grad_students_base_{DATA_VERSION}.RData" %>% glue())
