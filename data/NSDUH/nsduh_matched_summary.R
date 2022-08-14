library(glue)
library(tidyverse)
load('nsduh_study_data.RData')
load('nsduh_matched_study_data.RData')

demographic_vars <- c('sex', 'age_buckets', 'educ_level', 'race_ethn', 'global_health')
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
	filter(!is.na(c_Time) & adult_mask == 1)

dat_matched <- nsduh_matched_df %>%
	filter(!is.na(c_Time) & college_adult_mask == 1)

summarize_by_year <- function(df, var, sample_label) {
	res <- df %>%
		group_by(year, !!sym(var)) %>%
		summarize(N = n()) %>%
		mutate(percentage = N/sum(N) * 100) %>%
		arrange(year, !!sym(var))
	
	filename <- 'nsduh_{sample_label}_{var}_by_year.csv' %>% glue()
	write.csv(res, filename, row.names=FALSE)
}

summarize_overall <- function(df, var, sample_label) {
	res <- df %>%
		group_by(!!sym(var)) %>%
		summarize(N=n()) %>%
		mutate(percentage = N/sum(N) * 100) %>%
		arrange(!!sym(var))

	filename <- 'nsduh_{sample_label}_{var}_overall.csv' %>% glue()
	write.csv(res, filename, row.names=FALSE)
}

for (var in demographic_vars){
	summarize_by_year(dat_adults, var, 'adults')
	summarize_overall(dat_adults, var, 'adults')
	
	summarize_by_year(dat_matched, var, 'matched')
	summarize_overall(dat_matched, var, 'matched')
}

key_variables <- c('anymi_12mos', 'global_health_dich', 'suic_thnk_12mos', 'suic_try_12mos', 'depression_12mos')

for (var in key_variables){
	summarize_by_year(dat_adults, var, 'adults')
	summarize_overall(dat_adults, var, 'adults')
	
	summarize_by_year(dat_matched, var, 'matched')
	summarize_overall(dat_matched, var, 'matched')
}

write.csv(dat_adults, 'nsduh_adults_study_variables.csv', row.names=FALSE)
haven::write_sav(dat_adults, 'nsduh_adults_study_variables.sav')

write.csv(dat_matched, 'nsduh_matched_study_variables.csv', row.names=FALSE)
haven::write_sav(dat_adults, 'nsduh_matched_study_variables.sav')

