# Create sampling weights
library(tidyverse)
library(glue)
source('~/Desktop/grad_mh/project_config.R')

load('{DATA_DIR}/NSDUH/nsduh_study_data.RData' %>% glue())
load('{DATA_DIR}/ACHA-II/ncha_study_data.RData' %>% glue())

# Will create weights using the probability of being in the ACHA-II sample each year
# Using age and sex categories only for the creation of the weights 
# Note not able to use gender as not present in the NSDUH data

# age categories: 
#   18, 21
#   22, 23
#   24, 25
#   26, 29
#   30, 34
#   35, 49
#   50, 64
#   65, Inf

table(nsduh_study_df$age_categories)
min(grads_only_study_df$Q46_age, na.rm=TRUE)

nsduh_study_df[['age_buckets']] <- case_when(
    nsduh_study_df[['age_categories']] %in% as.character(12:17) ~ '999',
    nsduh_study_df[['age_categories']] == '18' ~ '18, 21', 
    nsduh_study_df[['age_categories']] == '19' ~ '18, 21',
    nsduh_study_df[['age_categories']] == '20' ~ '18, 21',
    nsduh_study_df[['age_categories']] == '21' ~ '18, 21',
    nsduh_study_df[['age_categories']] == '22, 23' ~ '22, 23',
    nsduh_study_df[['age_categories']] == '24, 25' ~ '24, 25',
    nsduh_study_df[['age_categories']] == '26, 29' ~ '26, 29',
    nsduh_study_df[['age_categories']] == '30, 34' ~ '30, 34',
    nsduh_study_df[['age_categories']] == '35, 49' ~ '35, 49',
    nsduh_study_df[['age_categories']] == '50, 64' ~ '50, 64',
    nsduh_study_df[['age_categories']] == '65, Inf' ~ '65, Inf'
)

nsduh_matched_df <- nsduh_study_df %>% 
    filter(age_buckets != '999' & college_adult_mask == 1)

# Generate a weights data.frame
grads_only_study_df[['age_buckets']] <- case_when(
    between(grads_only_study_df[['Q46_age']], 18, 21.999) ~ '18, 21',
    between(grads_only_study_df[['Q46_age']], 22, 23.999) ~ '22, 23',
    between(grads_only_study_df[['Q46_age']], 24, 25.999) ~ '24, 25',
    between(grads_only_study_df[['Q46_age']], 26, 29.999) ~ '26, 29',
    between(grads_only_study_df[['Q46_age']], 30, 34.999) ~ '30, 34',
    between(grads_only_study_df[['Q46_age']], 35, 49.999) ~ '35, 49',
    between(grads_only_study_df[['Q46_age']], 50, 64.999) ~ '50, 64',
    grads_only_study_df[['Q46_age']]>= 65 ~ '65, Inf'
)

# Make a data.frame with weights
grads_wght_basis <- grads_only_study_df %>%
    # removing transgender as there is no indicator for this in NSDUH
    filter(Q47_gender != "Transgender") %>% 
    filter(!is.na(age_buckets)) %>% 
    group_by(Q47_gender, age_buckets) %>% 
    summarize(obs = n()) %>%
    ungroup() %>% 
    mutate(
        grad_prop = obs/sum(obs), 
        sex = Q47_gender
    ) %>% 
    select(sex, age_buckets, grad_prop)

nsduh_wght_basis <- nsduh_matched_df %>% 
  group_by(sex, age_buckets) %>% 
  summarize(wght_sum = sum(weights)) %>% 
  ungroup() %>% 
  mutate(
      matched_prop = wght_sum / sum(wght_sum)
  ) %>% 
  select(sex, age_buckets, matched_prop)

nsduh_weights <- merge(grads_wght_basis, nsduh_wght_basis, by = c('sex', 'age_buckets'))
nsduh_weights[['matched_weights']] <- nsduh_weights[['grad_prop']] / nsduh_weights[['matched_prop']]

nsduh_matched_df <- nsduh_weights %>% 
  select(sex, age_buckets, matched_weights) %>% 
  right_join(nsduh_matched_df, by=c('sex', 'age_buckets'))

nsduh_weighted_summary_df <- nsduh_matched_df %>% 
    group_by(c_Time) %>% 
    summarize(
        anymi_12mos = sum(anymi_12mos * matched_weights) / sum(matched_weights) * 100,
        suic_thnk_12mos= sum(as.numeric(suic_thnk_12mos == 'Yes') * matched_weights, na.rm = TRUE) / sum(matched_weights) * 100, 
        suic_try_12mos = sum(as.numeric(suic_try_12mos == 'Yes') * matched_weights, na.rm = TRUE) / sum(matched_weights) * 100,
        global_health_dich = sum(as.numeric(global_health %in% c('Poor', 'Fair')) * matched_weights) / sum(matched_weights) *100,
    ) %>% 
    ungroup() %>% 
    select(c_Time, anymi_12mos, suic_thnk_12mos, suic_try_12mos, global_health_dich)

save(list=c('nsduh_matched_df', 'nsduh_weighted_summary_df'), 
     file='{DATA_DIR}/NSDUH/nsduh_matched_study_data.RData' %>% glue())
