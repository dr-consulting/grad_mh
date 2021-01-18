# Be sure to update the data version to the appropriate date before running
DATA_VERSION <- "2021-01-15"

library(glue)
library(tidyverse)
library(haven)
source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)
load('{DATA_DIR}/ACHA-II/acha_study_data_{DATA_VERSION}.RData' %>% glue())

grad_only_study_df <- grads_only_study_df %>% 
    filter(Q52_enrollment == "Full-time" | Q52_enrollment == "Part-time") %>% 
    filter(school_type == "4-year") %>% 
    filter(total_obs >= 5)

# Run the code below to see how many cases get dropped. 
grad_only_study_df %>% 
    group_by(school_id, survey_period) %>% 
    summarize(post_filter_total = n()) %>% 
    select(post_filter_total) %>% 
    filter(post_filter_total < 5) %>% 
    table()

write.csv(grads_only_study_df,
          "{DATA_DIR}/ACHA-II/grad_students_study_base_{DATA_VERSION}.csv" %>% glue(), 
          row.names = FALSE)

write_sav(grads_only_study_df,
          "{DATA_DIR}/ACHA-II/grad_students_study_base_{DATA_VERSION}.sav" %>% glue())

save(list = c("grads_only_study_df", "DATA_VERSION"), 
     file = "{DATA_DIR}/ACHA-II/acha_grad_students_base_{DATA_VERSION}.RData" %>% glue())
