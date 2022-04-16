# Acquiring summary statistics for ACHA-NCHA II
library(tidyverse)
library(glue)
# options()
# Project vars 
DATA_VERSION <- '2021-02-04'
PROJECT_DIR <- '~/github/ATNL/grad_mh'
DATA_DIR <- glue('{PROJECT_DIR}/data/ACHA-II')
RDATA_PATH <- glue('{DATA_DIR}/acha_grad_students_base_{DATA_VERSION}.RData')

# load data
load(RDATA_PATH)

#----------------------------------------------------------------------------------------------------------------------
# Table 2 - Demographic characteristics: 
#----------------------------------------------------------------------------------------------------------------------
grp_var <- 'academic_year'
table(grads_model_base[[grp_var]]) # Simple count of respondents per year

grads_model_base %>% # AY breakdown of summary stats for gender
    group_by(!!sym(grp_var)) %>% 
    summarize(female = sum(Q47_gender == "Female", na.rm = TRUE), 
              male = sum(Q47_gender == "Male", na.rm = TRUE), 
              trans = sum(Q47_gender == "Transgender", na.rm = TRUE), 
              n_nonmiss = sum(!is.na(Q47_gender)), 
              female_perc = female/n_nonmiss * 100, 
              male_perc = male/n_nonmiss * 100,
              trans_perc = trans/n_nonmiss * 100) %>% 
    print.data.frame()

# Grand total percentages of genders
table(grads_model_base[['Q47_gender']]) / sum(!is.na(grads_model_base[['Q47_gender']])) * 100

grads_model_base %>% # AY breakdown of race / ethnicity
    group_by(!!sym(grp_var)) %>% 
    summarize(white = sum(race_ethn == "white", na.rm = TRUE), 
              black = sum(race_ethn == "black", na.rm = TRUE), 
              hispanic = sum(race_ethn == "hispanic", na.rm = TRUE), 
              asian = sum(race_ethn == "asian", na.rm = TRUE), 
              native = sum(race_ethn == "native", na.rm = TRUE), 
              multi = sum(race_ethn == "multi", na.rm = TRUE), 
              other = sum(race_ethn == "other", na.rm = TRUE), 
              n_nonmiss = sum(!is.na(race_ethn)), 
              white_perc = white/n_nonmiss * 100, 
              black_perc = black/n_nonmiss * 100, 
              hispanic_perc = hispanic/n_nonmiss * 100, 
              asian_perc = asian/n_nonmiss * 100, 
              native_perc = native/n_nonmiss * 100, 
              multi_perc = multi/n_nonmiss * 100, 
              other_perc = other/n_nonmiss * 100) %>% 
    select(!!sym(grp_var), ends_with('_perc')) %>% 
    print.data.frame()

# grand perc of race/ethnicity: 
table(grads_model_base[['race_ethn']]) / sum(!is.na(grads_model_base[['race_ethn']])) * 100


grads_model_base %>% # AY breakdown of international status
    group_by(!!sym(grp_var)) %>% 
    summarize(intl = sum(Q55_international == "Yes", na.rm = TRUE), 
              n_nonmiss = sum(!is.na(Q55_international)), 
              intl_perc = intl/n_nonmiss * 100) %>% 
    print.data.frame()

# grand perc of international status: 
table(grads_model_base[['Q55_international']]) / sum(!is.na(grads_model_base[['Q55_international']])) * 100

grads_model_base %>% # AY breakdown of enrollment status
    group_by(!!sym(grp_var)) %>% 
    summarize(full = sum(Q52_enrollment == "Full-time", na.rm = TRUE), 
              part = sum(Q52_enrollment == "Part-time", na.rm = TRUE), 
              n_nonmiss = sum(!is.na(Q52_enrollment)), 
              full_perc = full / n_nonmiss * 100,
              part_perc = part / n_nonmiss * 100) %>% 
    print.data.frame()

# grand perc of enrollment status
table(grads_model_base[['Q52_enrollment']]) / sum(!is.na(grads_model_base[['Q52_enrollment']])) * 100

#------------------
# Table 2 - additional missingness info
vars <- c('Q46_age', 'Q47_gender', 'race_ethn', 'Q55_international', 'Q52_enrollment')
colSums(is.na(grads_model_base[vars]) / nrow(grads_model_base)) * 100

#----------------------------------------------------------------------------------------------------------------------
# Table 3 - Institutional Characteristics
#----------------------------------------------------------------------------------------------------------------------
grp_var <- 'academic_year'
grads_model_base %>% # AY breakdown of respondents per school type
    group_by(!!sym(grp_var)) %>% 
    summarize(pub = sum(public_schl == 1, na.rm = TRUE), 
              priv = sum(public_schl == 0, na.rm = TRUE), 
              n_nonmiss = sum(!is.na(public_schl)), 
              pub_perc = pub / n_nonmiss * 100,
              priv_perc = priv / n_nonmiss * 100) %>% 
    print.data.frame()

# global perc of public / private school attendance
table(grads_model_base[['public_schl']]) / sum(!is.na(grads_model_base[['public_schl']]))

grads_model_base %>% # AY breakdown of respondents per school type
    group_by(!!sym(grp_var)) %>% 
    summarize(val_1 = sum(school_size == "< 2,500 students", na.rm = TRUE), 
              val_2 = sum(school_size == "2,500 - 4,999 students", na.rm = TRUE), 
              val_3 = sum(school_size == "5,000 - 9,999 students", na.rm = TRUE), 
              val_4 = sum(school_size == "10,000 - 19,999 students", na.rm = TRUE), 
              val_5 = sum(school_size == "20,000 students or more", na.rm = TRUE), 
              n_nonmiss = sum(!is.na(school_size)), 
              val_1_perc = val_1 / n_nonmiss * 100,
              val_2_perc = val_2 / n_nonmiss * 100,
              val_3_perc = val_3 / n_nonmiss * 100,
              val_4_perc = val_4 / n_nonmiss * 100,
              val_5_perc = val_5 / n_nonmiss * 100) %>% 
    select(academic_year, ends_with('_perc')) %>% 
    print.data.frame()

# global perc of public / private school attendance
table(grads_model_base[['school_size']]) / sum(!is.na(grads_model_base[['school_size']])) * 100

#------------------
# Table 3 - additional missingness info
vars <- c('public_schl', 'school_size')
colSums(is.na(grads_model_base[vars]) / nrow(grads_model_base)) * 100

#----------------------------------------------------------------------------------------------------------------------
# Table 4 - Emotional Distress
#----------------------------------------------------------------------------------------------------------------------
neg_vars <- c("Q30A_hopeless_r", "Q30D_lonely_r", "Q30E_sad_r", "Q30F_depressed_r", "Q30G_anxiety_r", "Q30H_anger_r") %>% 
    paste(sep = '_', '2wks')

# Explicitly choosing to drop everyone that has at least one missing - don't want to count as valid 0's in a count 
grads_model_base[['neg_emo_cnt']] <- rowSums(grads_model_base[,neg_vars])

grp_var <- 'academic_year'

for(var in neg_vars){
    print(glue("AY breakdown for {var}"))
    print("")
    print("-------------------------------------------------------")
    grads_model_base %>% # AY breakdown of neg emotion variables
        group_by(!!sym(grp_var)) %>% 
        summarize(cnt = sum(!!sym(var) == 1, na.rm = TRUE), 
                  n_nonmiss = sum(!is.na(Q55_international)), 
                  perc = cnt/n_nonmiss * 100) %>% 
        print.data.frame()
    print("-------------------------------------------------------")
}

for(var in neg_vars){
    # global perc of neg_vars
    print("Global percentage for {var}" %>% glue())
    tab <- table(grads_model_base[[var]]) / sum(!is.na(grads_model_base[[var]])) * 100 
    print(tab)
    perc_miss <- sum(is.na(grads_model_base[[var]])) / nrow(grads_model_base) *100
    print('Percentage missing{round(perc_miss, 2)}%' %>% glue())
}

grads_model_base %>% # AY breakdown of average counts
    group_by(!!sym(grp_var)) %>% 
    summarize(neg_emo_mean = mean(neg_emo_cnt, na.rm = TRUE), 
              neg_emo_sd = sd(neg_emo_cnt, na.rm = TRUE)) %>% 
    print.data.frame()

mean(grads_model_base[["neg_emo_cnt"]], na.rm = TRUE)
sd(grads_model_base[["neg_emo_cnt"]], na.rm = TRUE)

ovrwhlm_vars <- c('Q30B_overwhelmed_r', 'Q30C_exhausted_r') %>% 
    paste(sep = '_', '2wks')

# Explicitly choosing to drop everyone that has at least one missing - don't want to count as valid 0's in a count 
grads_model_base[['ovrwhlm_cnt']] <- rowSums(grads_model_base[,ovrwhlm_vars])

for(var in ovrwhlm_vars){
    print(glue("AY breakdown for {var}"))
    print("")
    print("-------------------------------------------------------")
    grads_model_base %>% # AY breakdown of overwhelmed variables
        group_by(!!sym(grp_var)) %>% 
        summarize(cnt = sum(!!sym(var) == 1, na.rm = TRUE), 
                  n_nonmiss = sum(!is.na(Q55_international)), 
                  perc = cnt/n_nonmiss * 100) %>% 
        print.data.frame()
    print("-------------------------------------------------------")
}

for(var in ovrwhlm_vars){
    # global perc of overwhelmed vars
    print("Global percentage for {var}" %>% glue())
    tab <- table(grads_model_base[[var]]) / sum(!is.na(grads_model_base[[var]])) * 100 
    print(tab)
    perc_miss <- sum(is.na(grads_model_base[[var]])) / nrow(grads_model_base) *100
    print('Percentage missing{round(perc_miss, 2)}%' %>% glue())
}

grads_model_base %>% # AY breakdown of average counts
    group_by(!!sym(grp_var)) %>% 
    summarize(ovrwhlm_mean = mean(ovrwhlm_cnt, na.rm = TRUE), 
              ovrwhlm_sd = sd(ovrwhlm_cnt, na.rm = TRUE)) %>% 
    print.data.frame()

mean(grads_model_base[["ovrwhlm_cnt"]], na.rm=TRUE)
sd(grads_model_base[["ovrwhlm_cnt"]], na.rm=TRUE)

#----------------------------------------------------------------------------------------------------------------------
# Table 5 - Suicidality, Past Year
#----------------------------------------------------------------------------------------------------------------------
suic_vars <- c("Q30J_suic_thnk_r_12mos", "Q30K_suic_try_r_12mos")
grp_var <- 'academic_year'

for(var in suic_vars){
    print(glue("AY breakdown for {var}"))
    print("")
    print("-------------------------------------------------------")
    grads_model_base %>% # AY breakdown of neg emotion variables
        group_by(!!sym(grp_var)) %>% 
        summarize(cnt = sum(!!sym(var) == 1, na.rm = TRUE), 
                  n_nonmiss = sum(!is.na(Q55_international)), 
                  perc = cnt/n_nonmiss * 100) %>% 
        print.data.frame()
    print("-------------------------------------------------------")
}

for(var in suic_vars){
    # global perc of neg_vars
    print("Global percentage for {var}" %>% glue())
    tab <- table(grads_model_base[[var]]) / sum(!is.na(grads_model_base[[var]])) * 100 
    print(tab)
    perc_miss <- sum(is.na(grads_model_base[[var]])) / nrow(grads_model_base) *100
    print('Percentage missing{round(perc_miss, 2)}%' %>% glue())
}

#----------------------------------------------------------------------------------------------------------------------
# Table 6 - Mental Health Diagnosis or Treatment, Past Year
#----------------------------------------------------------------------------------------------------------------------
dx_cols <- grads_model_base %>% 
    select(ends_with('_dich')) %>% 
    colnames()

dx_cols <- c(dx_cols, 'any_dx_nsduh')
for(var in dx_cols){
    print(glue("AY breakdown for {var}"))
    print("")
    print("-------------------------------------------------------")
    grads_model_base %>% # AY breakdown of dx and tx variables
        group_by(!!sym(grp_var)) %>% 
        summarize(cnt = sum(!!sym(var) == 1, na.rm = TRUE), 
                  n_nonmiss = sum(!is.na(Q55_international)), 
                  perc = cnt/n_nonmiss * 100) %>% 
        print.data.frame()
    print("-------------------------------------------------------")
}

for(var in dx_cols){
    # global perc of diagnosis and treatment variables
    print("Global percentage for {var}" %>% glue())
    tab <- table(grads_model_base[[var]]) / sum(!is.na(grads_model_base[[var]])) * 100 
    print(tab)
    perc_miss <- sum(is.na(grads_model_base[[var]])) / nrow(grads_model_base) *100
    print('Percentage missing{round(perc_miss, 2)}%' %>% glue())
}
