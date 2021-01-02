# File containing all functions needed to process and clean data used in project
source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)

# Load the NSDUH data:
load_RData_files('~/github/ATNL/grad_mh/data/NSDUH')

# Process and concatenate original and study_dfs
NSDUH_2008_df_list <- create_study_datasets(PUF2008_090718, '{MAPS_DIR}/NSDUH_map_2008.yaml' %>% glue())
NSDUH_2009_df_list <- create_study_datasets(PUF2009_090718, '{MAPS_DIR}/NSDUH_map_2009to2014.yaml' %>% glue())
NSDUH_2010_df_list <- create_study_datasets(PUF2010_090718, '{MAPS_DIR}/NSDUH_map_2009to2014.yaml' %>% glue())
NSDUH_2011_df_list <- create_study_datasets(PUF2011_090718, '{MAPS_DIR}/NSDUH_map_2009to2014.yaml' %>% glue())
NSDUH_2012_df_list <- create_study_datasets(PUF2012_090718, '{MAPS_DIR}/NSDUH_map_2009to2014.yaml' %>% glue())
NSDUH_2013_df_list <- create_study_datasets(PUF2013_090718, '{MAPS_DIR}/NSDUH_map_2009to2014.yaml' %>% glue())
NSDUH_2014_df_list <- create_study_datasets(PUF2014_090718, '{MAPS_DIR}/NSDUH_map_2009to2014.yaml' %>% glue())
NSDUH_2015_df_list <- create_study_datasets(PUF2015_021518, '{MAPS_DIR}/NSDUH_map_2015to2019.yaml' %>% glue())
NSDUH_2016_df_list <- create_study_datasets(PUF2016_022818, '{MAPS_DIR}/NSDUH_map_2015to2019.yaml' %>% glue())
NSDUH_2017_df_list <- create_study_datasets(PUF2017_100918, '{MAPS_DIR}/NSDUH_map_2015to2019.yaml' %>% glue())
NSDUH_2018_df_list <- create_study_datasets(PUF2018_100819, '{MAPS_DIR}/NSDUH_map_2015to2019.yaml' %>% glue())
NSDUH_2019_df_list <- create_study_datasets(PUF2019_100920, '{MAPS_DIR}/NSDUH_map_2015to2019.yaml' %>% glue())

# Gather the values needed for creating a study data.frame
nsduh_dfs <- ls()[grepl(glob2rx('NSDUH_*_df_list'), ls())]
nsduh_study_df <- data.frame()
for(i in seq_along(nsduh_dfs)) {
    tmp_df <- get(nsduh_dfs[[i]])[['study_df']]
    tmp_df[['year']] <- strsplit(nsduh_dfs[[i]], split = '_', fixed = TRUE)[[1]][[2]] %>% 
        as.numeric()
    nsduh_study_df <- rbind(nsduh_study_df, tmp_df)
}

# Create an adults only mask variable: 
# Cut point here is 7 and under for non-adults - and there are a total of 17 valid age buckets
# Values outside this range are expected to be NAs

# The age variable is constructed in the same way in both version of the surveys (pre-2015 and after)
ages <- yaml::read_yaml('{MAPS_DIR}/NSDUH_map_2009to2014.yaml' %>% glue())[['AGE2']][['responses']] %>% 
    unlist()
valid_ages <- ages[7:17]
invalid_ages <- ages[-7:-17]

# Create a mask for selecting adults
nsduh_study_df[['adult_mask']] <- case_when(
    nsduh_study_df[['age_categories']] %in% valid_ages ~ 1, 
    nsduh_study_df[['age_categories']] %in% invalid_ages ~ 0
)

# Create a mask for selecting college-educated adults
nsduh_study_df[['college_adult_mask']] <- case_when(
    nsduh_study_df[['adult_mask']] == 0 ~ 0, 
    nsduh_study_df[['adult_mask']] == 1 & nsduh_study_df[['educ_level']] != 'Senior/16th year or Grad/Prof School (or higher)' ~ 0,
    nsduh_study_df[['adult_mask']] == 1 & nsduh_study_df[['educ_level']] == 'Senior/16th year or Grad/Prof School (or higher)' ~ 1
)

# Create a date variable based on survey year and quarter variable
nsduh_study_df[['qtr_date']] <- 
    paste(
        nsduh_study_df[['year']], 
        str_pad(nsduh_study_df[['qtr']] * 3, 2, "0", side = 'left'), 
        '01', 
        sep = '-') %>% 
    as.Date(., format = '%Y-%m-%d')

# Build back in the skip logic for `suic_try_12mos`
# The question was skipped over if the respondent indicated no to the previous think and plan questions
nsduh_study_df[['suic_try_12mos']] <- ifelse(is.na(nsduh_study_df[['suic_try_12mos']]) & 
                                                 nsduh_study_df[['suic_thnk_12mos']] == 'No', 
                                             'No', nsduh_study_df[['suic_try_12mos']])


save(list = c('nsduh_study_df', nsduh_dfs), 
     file = '{DATA_DIR}/NSDUH/nsduh_study_data.RData' %>% glue())
