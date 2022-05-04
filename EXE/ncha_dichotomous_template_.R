library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)

source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)

yvars <- list('Anxiety' = c(var_name='Q31A_anxiety_dich', data_prefix='dx_anxiety_full_covariate'),
              'Any Psychiatric Disorder' = c(var_name='any_dx_nsduh', data_prefix='dx_nsduh_any_full_covariate'),
              'Bipolar Disorder' = c(var_name='Q31A_bipolar_dich', data_prefix='bipolar_full_covariate'),
              'Depression' = c(var_name='Q31A_depression_dich', data_prefix='dx_depression_full_covariate'), 
              'Panic Attacks' = c(var_name='Q31B_panic_dich', data_prefix='panic_full_covariate'), 
              'Schizophrenia' = c(var_name='Q31B_schizo_dich', data_prefix='dx_schizo_full_covariate'), 
              'Suicidal Thoughts' = c(var_name='Q30J_suic_thnk_r_12mos', data_prefix='suic_thnk_full_covariate'), 
              'Suicide Attempt' = c(var_name='Q30K_suic_try_r_12mos', data_prefix='suic_try_full_covariate'),
              'Poor Health' = c(var_name='global_health_dich', data_prefix='global_health_full_covariate'), 
              'Emotional Distress' = c(var_name='neg_emo_any', data_prefix='neg_emo_logit'),
              'Overwhelmed' = c(var_name='Q30B_overwhelmed_r_2wks', data_prefix='ovrwhlm_bin'),
              'Exhausted' = c(var_name='Q30C_exhausted_r_2wks', data_prefix='exhausted_bin'),
              'Overwhelmed or Exhausted' = c(var_name='ovrwhlm_any', data_prefix='ovrwhlm_any_logit'), 
              'Sad'= c(var_name="Q30E_sad_r_2wks", data_prefix='sad_full_covariate'),
              'Anger'= c(var_name="Q30H_anger_r_2wks", data_prefix='anger_full_covariate'),
              'Too Depressed to Function' = c(var_name="Q30F_depressed_r_2wks", data_prefix='depressed_full_covariate'), 
              'Overwhelming Anxiety' = c(var_name="Q30G_anxiety_r_2wks", data_prefix='anxious_full_covariate'), 
              'Lonely' = c(var_name="Q30D_lonely_r_2wks", data_prefix='lonely_full_covariate'), 
              'Hopeless' = c(var_name="Q30A_hopeless_r_2wks", data_prefix='hopeless_full_covariate')
              )

time_vars <- c('c_Time', 'quad_c_Time')
covariates <- c('Intercept'='Intercept', 'Time'='c_Time', 'Time^2'='quad_c_Time',
                'Age'='c_Q46_age', 
                # Gender labels
                'Male v. Female'='Q47_genderMale', 'Transgender v. Female'='Q47_genderTransgender', 
                # Race labels
                'Black v. White'='race_ethnblack', 'Hispanic v. White'='race_ethnhispanic', 
                'Asian v. White'='race_ethnasian', 'Native v. White'='race_ethnnative', 
                'Multiracial v. White'='race_ethnmulti', 'Other v. White'='race_ethnother', 
                # Enrollment, International, and survey method
                'Part-Time'='Q52_enrollmentPartMtime', 
                'International'='Q55_internationalYes', 
                'Web Survey'='survey_methodWeb',
                # School properties
                'Size: 2,500-4,999 v. <2,500'='school_size2500M4999students', 
                'Size: 5,000-9,999 v. <2,500'='school_size5000M9999students', 
                'Size: 10,000-19,999 v. <2,500'='school_size10000M19999students',
                'Size: >20,000 v. <2,500'='school_size20000studentsormore',
                'Public School'='public_schl'
                )

xvar <- 'c_Time'

plotlist <- list()

for(yvar in names(yvars)){
    var_name <- yvars[[yvar]]['var_name']
    names(var_name) <- NULL
    data_prefix <- yvars[[yvar]]['data_prefix']
    names(data_prefix) <- NULL
    
    data_filepath <- "{POSTERIOR_OUTPUTS}/{data_prefix}.RData" %>% glue()
    
    if(file.exists(data_filepath)) {
        load(data_filepath)
        
        # Test that results_list is present after the load operation
        testthat::expect_true(exists("results_list"))
        
        # Set base pipeline configuration here
        data <- results_list[["brms_fit"]]$data
        model <- results_list[["brms_fit"]]
        
        # Write full coefficients table to .csv
        fixef_tbl <- fixef(model)
        for(row in 1:nrow(fixef_tbl)) {
            rownames(fixef_tbl)[row] <- names(covariates)[which(covariates == rownames(fixef_tbl)[row])]
        }
        fixef_tbl %>% 
            write.csv("{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_coefficients_table.csv" %>% glue(), row.names = TRUE)
        
        # Create group df - only for schools with at least 30 respondents 
        grp_df <- data %>% 
            group_by(school_id, !!sym(xvar)) %>% 
            summarize(
                count = n(), 
                perc = mean(!!sym(var_name), na.rm = TRUE) * 100
            ) %>% 
            filter(count >= 30)
        
        load_data_path <- '{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_summary_data.RData' %>% glue()
        
        if(file.exists(load_data_path)) {
            load(load_data_path)
        }
        else{
            post_df <- posterior_samples(model, pars = c('b_Intercept', 'b_c_Time', 'b_quad_c_Time'))
            names(post_df) <- c('b0', 'b1', 'b2')
            
            gender_prob <- table(data[["Q47_gender"]]) / nrow(data)
            race_ethn_prob <- table(data[["race_ethn"]]) / nrow(data)
            enrollment_prob <- table(data[["Q52_enrollment"]]) / nrow(data)
            international_prob <- table(data[["Q55_international"]]) / nrow(data)
            survey_method_prob <- table(data[["survey_method"]]) / nrow(data)
            school_size_prob <- table(data[["school_size"]]) / nrow(data)
            public_schl_prob <- table(data[["public_schl"]]) / nrow(data)
            
            post_df['b0'] <- post_df['b0'] + 
                gender_prob['Male'] * fixef_tbl['Male v. Female', 'Estimate'] +
                gender_prob['Transgender'] * fixef_tbl['Transgender v. Female', 'Estimate'] +
                race_ethn_prob['black'] * fixef_tbl['Black v. White', 'Estimate'] +
                race_ethn_prob['hispanic'] * fixef_tbl['Hispanic v. White', 'Estimate'] +
                race_ethn_prob['asian'] * fixef_tbl['Asian v. White', 'Estimate'] +
                race_ethn_prob['native'] * fixef_tbl['Native v. White', 'Estimate'] +
                race_ethn_prob['multi'] * fixef_tbl['Multiracial v. White', 'Estimate'] +
                race_ethn_prob['other'] * fixef_tbl['Other v. White', 'Estimate'] +
                enrollment_prob['Part-time'] * fixef_tbl['Part-Time', 'Estimate'] +
                international_prob['Yes'] * fixef_tbl['International', 'Estimate'] +
                survey_method_prob['Web'] * fixef_tbl['Web Survey', 'Estimate'] +
                school_size_prob['2,500 - 4,999 students'] * fixef_tbl['Size: 2,500-4,999 v. <2,500', 'Estimate'] +
                school_size_prob['5,000 - 9,999 students'] * fixef_tbl['Size: 5,000-9,999 v. <2,500', 'Estimate'] +
                school_size_prob['10,000 - 19,999 students'] * fixef_tbl['Size: 10,000-19,999 v. <2,500', 'Estimate'] +
                school_size_prob['20,000 students or more'] * fixef_tbl['Size: >20,000 v. <2,500', 'Estimate'] +
                public_schl_prob['1'] * fixef_tbl['Public School', 'Estimate']
            
            design_matrix <- data.frame(
                intrcpt = 1,
                time = seq(0, 10.5, by = .125), 
                time_sq = seq(0, 10.5, by = .125)^2
            )
            
            plot_df <- data.frame()
            for(r in 1:nrow(post_df)) {
                pred_y <- design_matrix[['intrcpt']] * post_df[['b0']][r] + 
                    design_matrix[['time']] * post_df[['b1']][r] +
                    design_matrix[['time_sq']] * post_df[['b2']][r]
                
                tmp_df <- data.frame(
                    c_Time = design_matrix[['time']], 
                    perc = logits_to_prob(pred_y) * 100
                )
                plot_df <- rbind(plot_df, tmp_df)
            }
        }
        
        plotlist[[yvar]] <- create_percent_summary_plot(
            plot_df, 
            grp_df, 
            title = yvar,
            x_breaks = seq(0.5, 10.5, by = 2),
            x_labels = seq(2009, 2019, by = 2),
            y_breaks = PERC_PLOT_CONFIG[[yvar]][['y_breaks']],
            y_labels = PERC_PLOT_CONFIG[[yvar]][['y_labels']],
            y_limits = PERC_PLOT_CONFIG[[yvar]][['y_limits']],
            color_pal = "Blues", 
        )
        
        ggsave(
            plotlist[[yvar]],
            filename = "{PLOT_OUTPUT}/ACHA-NCHA_{yvar}_summary_plot_new.png" %>% glue(), 
            device = "png", 
            units = "in", 
            height = 5,
            width = 9,
            dpi = 600
        )
        
        # Create summary table of observed and fitted effects
        create_bin_summary_table(data, plot_df, yvar = var_name) %>% 
            write.csv("{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_summary_stats.csv" %>% glue(), row.names = FALSE)
        
        summary_plot <- plotlist[[yvar]]
        
        # Save and remove contents from this run - add in checking logic above later 
        save(list = c('data', 'model', 'plot_df', 'summary_plot'), 
             file = '{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_summary_data.RData' %>% glue())
        remove(list=c('data', 'model', 'plot_df', 'summary_plot'))
        gc()
    }
}
