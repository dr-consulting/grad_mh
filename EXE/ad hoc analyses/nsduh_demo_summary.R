# Produce basic demographic summaries from NSDUH data
source('~/Desktop/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)

load('{LOCAL_REPO}/data/NSDUH/nsduh_study_data_20230114.RData' %>% glue())
load('{LOCAL_REPO}/data/NSDUH/nsduh_matched_data_20230114.RData' %>% glue())

demographic_vars <- c('sex', 'age_categories', 'educ_level', 'race_ethn', 'global_health')
nsduh_study_df[['educ_level']] <- forcats::fct_relevel(nsduh_study_df[['educ_level']], 
													   c('Fifth grade or less', 
														 'Sixth grade',
														 'Seventh grade',
														 'Eighth grade',
														 'Ninth grade',
														 'Tenth grade',
														 'Eleventh grade',
														 'Twelfth grade',
														 'Freshman/13th year',
														 'Sophomore/14th year or Junior/15th year',
														 'Senior/16th year or Grad/Prof School (or higher)'))

nsduh_study_df[['age_buckets']] <- ifelse(nsduh_study_df[['age_categories']] %in% 18:21, '18,21', nsduh_study_df[['age_categories']])
nsduh_study_df[['global_health_dich']] <- ifelse(nsduh_study_df[['global_health']] %in% c('Poor', 'Fair'), 1, 0)
nsduh_study_df[['global_health_dich']] <- ifelse(is.na(nsduh_study_df[['global_health']]), NA, nsduh_study_df[['global_health_dich']])


nsduh_matched_df[['global_health_dich']] <- ifelse(nsduh_matched_df[['global_health']] %in% c('Poor', 'Fair'), 1, 0)
nsduh_matched_df[['global_health_dich']] <- ifelse(is.na(nsduh_matched_df[['global_health']]), NA, nsduh_matched_df[['global_health_dich']])

dat_adults <- nsduh_study_df %>%
	filter(c_Time <= 10.5 & adult_mask == 1)

dat_adults[["academic_yr"]] <- case_when(
    dat_adults[["c_Time"]] %in% c(0, 0.5) ~ '2008-2009',
    dat_adults[["c_Time"]] %in% c(1, 1.5) ~ '2009-2010',
    dat_adults[["c_Time"]] %in% c(2, 2.5) ~ '2010-2011',
    dat_adults[["c_Time"]] %in% c(3, 3.5) ~ '2011-2012',
    dat_adults[["c_Time"]] %in% c(4, 4.5) ~ '2012-2013',
    dat_adults[["c_Time"]] %in% c(5, 5.5) ~ '2013-2014',
    dat_adults[["c_Time"]] %in% c(6, 6.5) ~ '2014-2015',
    dat_adults[["c_Time"]] %in% c(7, 7.5) ~ '2015-2016',
    dat_adults[["c_Time"]] %in% c(8, 8.5) ~ '2016-2017',
    dat_adults[["c_Time"]] %in% c(9, 9.5) ~ '2017-2018',
    dat_adults[["c_Time"]] %in% c(10, 10.5) ~ '2018-2019'
)

dat_matched <- nsduh_matched_df %>%
	filter(c_Time <= 10.5 & college_adult_mask == 1)

dat_matched[["academic_yr"]] <- case_when(
    dat_matched[["c_Time"]] %in% c(0, 0.5) ~ '2008-2009',
    dat_matched[["c_Time"]] %in% c(1, 1.5) ~ '2009-2010',
    dat_matched[["c_Time"]] %in% c(2, 2.5) ~ '2010-2011',
    dat_matched[["c_Time"]] %in% c(3, 3.5) ~ '2011-2012',
    dat_matched[["c_Time"]] %in% c(4, 4.5) ~ '2012-2013',
    dat_matched[["c_Time"]] %in% c(5, 5.5) ~ '2013-2014',
    dat_matched[["c_Time"]] %in% c(6, 6.5) ~ '2014-2015',
    dat_matched[["c_Time"]] %in% c(7, 7.5) ~ '2015-2016',
    dat_matched[["c_Time"]] %in% c(8, 8.5) ~ '2016-2017',
    dat_matched[["c_Time"]] %in% c(9, 9.5) ~ '2017-2018',
    dat_matched[["c_Time"]] %in% c(10, 10.5) ~ '2018-2019'
)

summarize_by_year <- function(df, var, group_var, sample_label) {
    cnt_res <- df %>% 
        group_by(!!sym(group_var)) %>% 
        count(!!sym(var)) %>% 
        pivot_wider(id_cols=group_var, names_from=var, values_from="n")
    
    per_cols <- cnt_res %>% 
        ungroup() %>% 
        select(- !!sym(group_var)) %>% 
        names()
    
    row_totals <- rowSums(cnt_res[per_cols], na.rm=TRUE)
    
    per_res <- cnt_res
    per_res[per_cols] <- round(per_res[per_cols] / row_totals * 100, 2)
    
	cnt_filename <- 'nsduh_{sample_label}_{var}_count_by_year_20230114.csv' %>% glue()
	write.csv(cnt_res, cnt_filename, row.names=FALSE)
	
    per_filename <- 'nsduh_{sample_label}_{var}_percent_by_year_20230114.csv' %>% glue()
	write.csv(per_res, per_filename, row.names=FALSE)
}

summarize_overall <- function(df, var, sample_label) {
    cnt_res <- df %>% 
        count(!!sym(var))

    per_res <- cnt_res %>% 
        mutate(
            percent = round(n / sum(n) * 100, 2)
        ) %>% 
        select(!!sym(var), percent)
	
    cnt_filename <- 'nsduh_{sample_label}_{var}_count_overall_20230114.csv' %>% glue()
	write.csv(cnt_res, cnt_filename, row.names=FALSE)
	
	per_filename <- 'nsduh_{sample_label}_{var}_percent_overall_20230114.csv' %>% glue()
	write.csv(per_res, per_filename, row.names=FALSE)
}

for (var in c(demographic_vars, 'age_buckets')){
	summarize_by_year(dat_adults, var, 'academic_yr', 'adults')
	summarize_overall(dat_adults, var, 'adults')
	
	summarize_by_year(dat_matched, var, 'academic_yr', 'matched')
	summarize_overall(dat_matched, var, 'matched')
}

key_variables <- c('anymi_12mos', 'global_health_dich', 'suic_thnk_12mos', 'suic_try_12mos', 'depression_12mos')

for (var in key_variables){
	summarize_by_year(dat_adults, var, 'academic_yr', 'adults')
	summarize_overall(dat_adults, var, 'adults')
	
	summarize_by_year(dat_matched, var, 'academic_yr', 'matched')
	summarize_overall(dat_matched, var, 'matched')
}

