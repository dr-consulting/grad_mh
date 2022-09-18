library(tidyverse) 
nsduh_study_df %>% 
    filter(year == 2014) %>% 
    select(qtr_date, year, educ_level_cat) %>% 
    head(30)

names(NSDUH_2015_df_list$base_df) %>% 
    grep('EDUC', .)

names(NSDUH_2015_df_list$base_df)[c(2692, 2694)]

table(NSDUH_2015_df_list$base_df$IREDUHIGHST2)

NSDUH_2008_df_list[['base_df']] %>% 
    select(ANALWT_C, weights, health, global_health) %>% 
    head(5)

df %>% 
    filter(qtr_date == '2019-09-01') %>% 
    View()
table(df$qtr_date)

# Initial plotting revealed a very large spike in anymi_12mos
# Specifically it was the Q1 weighted value for 2012 that triggered concern
# Initial inspection suggested that it was just an issue with a low number of college-educated adults per quarter

nsduh_study_df %>% 
    filter(college_adult_mask == 1) %>% 
    group_by(qtr_date) %>% 
    summarize(prop = sum(anymi_12mos == 1, na.rm = TRUE) / sum(!is.na(anymi_12mos))) %>% 
    View()

nsduh_study_df %>%  
    filter(college_adult_mask == 1) %>% 
    mutate(wght_val = ifelse(anymi_12mos == 1, 1, 0) * weights) %>% 
    group_by(qtr_date) %>% 
    summarize(numerator = sum(wght_val, na.rm = TRUE), 
              denominator = sum(ifelse(is.na(anymi_12mos), 0, 1) * weights), 
              prop = numerator / denominator) %>% 
    View()

nsduh_study_df %>% 
    filter(college_adult_mask == 1) %>% 
    group_by(qtr_date) %>%
    summarize(totals = n()) %>% 
    View()

nsduh_study_df %>% 
    filter(adult_mask == 1) %>% 
    group_by(qtr_date) %>%
    summarize(totals = n()) %>% 
    View()

nsduh_study_df %>% 
    filter(educ_level == 'Senior/16th year or Grad/Prof School (or higher)') %>% 
    group_by(qtr_date) %>%
    summarize(totals = n()) %>% 
    View()
# RESOLVED - the problem here had to do with continuing to recode the age variable to a numeric value when the code base
# has been updated to expect a character value for that variable now. 
