# Verifies that NSDUH all adults weights used are correct...

library(tidyverse)
library(glue)

BASE_FILE <- '~/Desktop/grad_mh/project_config.R'

# If missing the config file raise early.
# Likely just opened the repo in a different file system
if(!file.exists(BASE_FILE)){
    stop('ERROR: Missing project config file. {BASE_FILE}' %>% glue())
}

source(BASE_FILE)
sapply(list.files(R_DIR, full.names = TRUE), source)

# Bring in the raw data - will be used to plot the weighted "empiricals"
load('{DATA_DIR}/NSDUH/nsduh_study_data_20230114.RData' %>% glue())

# These values were taken from this annual report summary table
# See table 10.1B here - https://www.samhsa.gov/data/sites/default/files/reports/rpt35323/NSDUHDetailedTabs2020v25/NSDUHDetailedTabs2020v25/NSDUHDetTabsSect10pe2020.htm
expected <- c(17.7, 18.1, 18.1, 17.8, 18.6, 18.5, 18.1, 17.9, 18.3, 18.9, 19.1, 20.6)


agg_anymi <- nsduh_study_df %>% 
    group_by(year) %>% 
    filter(!is.na(anymi_12mos) & !is.na(weights)) %>% 
    summarize(
        anymi = sum(anymi_12mos * weights) / sum(weights) * 100
    )

# Note that we should expect matching in 2008 and 2019 due to differences
# in sample size... Still there are discrepancies here - the pattern is the 
# same but the values from 2009 - 2013 
agg_anymi[['anymi']] - expected
