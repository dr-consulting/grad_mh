library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)

source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)

yvars <- list('Anxiety' = c(var_name='Q31A_anxiety_dich', data_prefix='dx_anxiety'),
              'Any Psychiatric Disorder' = c(var_name='any_dx', data_prefix='dx_any'),
              'Bipolar Disorder' = c(var_name='Q31A_bipolar_dich', data_prefix='bipolar'),
              'Depression' = c(var_name='Q31A_depression_dich', data_prefix='dx_depression'), 
              'Panic Attacks' = c(var_name='Q31B_panic_dich', data_prefix='panic'), 
              'Phobia' = c(var_name='Q31B_phobia_dich', data_prefix='dx_phobia'), 
              'Schizophrenia' = c(var_name='Q31B_schizo_dich', data_prefix='dx_schizo'), 
              'Suicidal Thoughts' = c(var_name='Q30J_suic_thnk_r_12mos', data_prefix='suic_thnk'), 
              'Suicidal Attempt' = c(var_name='Q30K_suic_try_r_12mos', data_prefix='suic_try'))

time_vars <- c('c_Time', 'quad_c_Time')
covariates <- c('Intercept'='Intercept', 'Time'='c_Time', 'Time^2'='quad_c_Time',
                'Age'='c_Q46_age', 
                # Gender labels
                'Male v. Female'='Q47_genderMale', 'Male v. Female'='Q47_genderTransgender', 
                # Race labels
                'Black v. White'='race_ethnblack', 'Hispanic v. White'='race_ethnhispanic', 
                'Asian v. White'='race_ethnasian', 'Native v. White'='race_ethnnative', 
                'Multiracial v. White'='race_ethnmulti', 'Other v. White'='race_ethnother', 
                # Enrollment, International, and survey method
                'Part-Time'='Q52_enrollmentPartMtime', 
                'Interational'='Q55_internationalYes', 
                'Web Survey'='survey_methodWeb',
                # School properties
                'Size: 2,500-4,999 v. <2,500'='school_size2500M4999students', 
                'Size: 5,000-9,999 v. <2,500'='school_size5000M9999students', 
                'Size: 10,000-19,999 v. <2,500'='school_size10000M19999students',
                'Size: >20,000 v. <2,500'='school_size20000studentsormore',
                'Public School'='public_schl'
                )

plotlist <- list()

for(yvar in names(yvars)){
    var_name <- yvars[[yvar]]['var_name']
    names(var_name) <- NULL
    data_prefix <- yvars[[yvar]]['data_prefix']
    names(data_prefix) <- NULL
    
    data_filepath <- "{POSTERIOR_OUTPUTS}/{data_prefix}_full_covariate.RData" %>% glue()
    
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
                perc = mean(!!sym(yvar), na.rm = TRUE) * 100
            ) %>% 
            filter(count >= 30)
        
        # Create plot - 500 per school and 1000 samples is close to the upper limit here 
        # These functions use the data and model objects above in place as "global variables"
        base_df <- create_base_pred_df(500)
        full_pred_df <- create_full_pred_df(base_df, 44, 0, 10.5)
        plot_df <- create_plot_df(full_pred_df, 1000)
        
        plotlist[[yvar]] <- create_percent_summary_plot(
            plot_df, 
            grp_df, 
            title = yvar,
            y_breaks = seq(0, 60, by=10), 
            y_labels = paste0(seq(0, 60, by=10), "%"), 
            x_breaks = seq(0.5, 10.5, by = 2),
            x_labels = seq(2009, 2019, by = 2),
            color_pal = "Blues", 
        )
        
        ggsave(
            plotlist[[yvar]],
            filename = "{PLOT_OUTPUT}/ACHA-NCHA_{yvar}_summary_plot.png" %>% glue(), 
            device = "png", 
            units = "in", 
            height = 5,
            width = 9,
            dpi = 600
        )
        
        # Create summary table of observed and fitted effects
        create_dx_summary_table(data, plot_df, yvar = var_name) %>% 
            write.csv("{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_summary_stats.csv" %>% glue(), row.names = FALSE)
    }
}


