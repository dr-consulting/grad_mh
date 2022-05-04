library(tidyverse)
load('nsduh_matched_study_data.RData')

# print(head(nsduh_matched_df))
demographic_vars <- c('sex', 'age_buckets', 'educ_level', 'race_ethn')
for (var in demographic_vars){
	res <- nsduh_matched_df %>%
		group_by(year, !!sym(var)) %>%
		summarize(N = n()) %>%
		mutate(percentage = N/sum(N) * 100) %>%
		arrange(year, !!sym(var))

	print(head(res))
	print(tail(res))
	write.csv(res, paste0('nsduh_matched_demo_', var, '.csv'), row.names=FALSE)
}
