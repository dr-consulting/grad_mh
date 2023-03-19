library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)
library(GGally)

BASE_FILE <- '~/Desktop/grad_mh/project_config.R'

# If missing the config file raise early.
# Likely just opened the repo in a different file system
if(!file.exists(BASE_FILE)){
    stop('ERROR: Missing project config file. {BASE_FILE}' %>% glue())
}

source(BASE_FILE)

sapply(list.files(R_DIR, full.names = TRUE), source)

VERSION <- Sys.Date()

yvars <- list('Suicide Attempt' = c(var_name='Q30K_suic_try_r_12mos', data_prefix='dx_suic_try_all_cov_linear'),
              'Exhausted' = c(var_name='Q30C_exhausted_r_2wks', data_prefix='exhausted_item_all_cov_linear'),
              'Overwhelmed' = c(var_name='Q30B_overwhelmed_r_2wks', data_prefix='ovrwhlm_item_all_cov_linear'),
              'Overwhelmed or Exhausted' = c(var_name='ovrwhlm_any', data_prefix='ovrwhlm_any_all_cov_linear'))

time_vars <- c('c_Time')
covariates <- c('Intercept'='Intercept', 'Time'='c_Time',
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
            write.csv("{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_coefficients_table_all_cov_linear_{VERSION}.csv" %>% glue(), row.names = TRUE)
        
        # Create group df - only for schools with at least 30 respondents 
        grp_df <- data %>% 
            group_by(school_id, !!sym(xvar)) %>% 
            summarize(
                count = n(), 
                perc = mean(!!sym(var_name), na.rm = TRUE) * 100
            ) %>% 
            filter(count >= 30)
        
        load_data_path <- '{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_summary_data_all_cov_linear.RData' %>% glue()
        
        if(file.exists(load_data_path)) {
            load(load_data_path)
        }
        else{
            par_names <- paste0('b_', covariates)
            post_df <- posterior_samples(model, pars = par_names)
            names(post_df)[1:2] <- c('b0', 'b1')
            # May generate a meaningless warning            
            diagnostic_plot <- ggpairs(post_df[,c('b0', 'b1')],
                                       upper = list(continuous = wrap('cor', size = 4, color='black', 
                                                                      stars=FALSE)),
                                       diag = list(continuous = wrap("densityDiag",
                                                                     fill='#426EBD')),
                                       lower = list(continuous = wrap('points', color='#426EBD')), 
                                       display_grid=FALSE) +
                labs(title='{yvar} Posterior Parameter Distributions' %>% glue(), 
                     subtitle='All covariates',
                     caption = paste('b0 = Model Intercept', 
                                     'b1 = Slope for linear effect of time',
                                     sep='\n'))
            
            ggsave(
                diagnostic_plot,
                filename = "{PLOT_OUTPUT}/ACHA-NCHA_{yvar}_parameter_diagnostic_pairsplot_all_cov_linear.png" %>% glue(), 
                device = "png", 
                units = "in", 
                height = 9,
                width = 8,
                dpi = 600
            )
            
            gender_prob <- table(data[["Q47_gender"]]) / nrow(data)
            race_ethn_prob <- table(data[["race_ethn"]]) / nrow(data)
            enrollment_prob <- table(data[["Q52_enrollment"]]) / nrow(data)
            international_prob <- table(data[["Q55_international"]]) / nrow(data)
            survey_method_prob <- table(data[["survey_method"]]) / nrow(data)
            school_size_prob <- table(data[["school_size"]]) / nrow(data)
            public_schl_prob <- table(data[["public_schl"]]) / nrow(data)
            
            lv2_post <- posterior_samples(model, pars = c('b_Intercept', 'b_c_Time'))
            
            # An individual's risk is determined by a set of individual factors + 
            # when in time they were surveyed. 
            post_df['b0'] <- post_df['b0'] + 
                gender_prob['Male'] * post_df[["b_Q47_genderMale"]] +
                gender_prob['Transgender'] * post_df[["b_Q47_genderTransgender"]] +
                race_ethn_prob['black'] * post_df[["b_race_ethnblack"]] +
                race_ethn_prob['hispanic'] * post_df[["b_race_ethnhispanic"]] +
                race_ethn_prob['asian'] * post_df[["b_race_ethnasian"]] +
                race_ethn_prob['native'] * post_df[["b_race_ethnnative"]] +
                race_ethn_prob['multi'] * post_df[["b_race_ethnmulti"]] +
                race_ethn_prob['other'] * post_df[["b_race_ethnother"]] +
                enrollment_prob['Part-time'] * post_df[["b_Q52_enrollmentPartMtime"]] +
                international_prob['Yes'] * post_df[["b_Q55_internationalYes"]] +
                survey_method_prob['Web'] * post_df[["b_survey_methodWeb"]] +
                school_size_prob['2,500 - 4,999 students'] * post_df[["b_school_size2500M4999students"]] +
                school_size_prob['5,000 - 9,999 students'] * post_df[["b_school_size5000M9999students"]] +
                school_size_prob['10,000 - 19,999 students'] * post_df[["b_school_size10000M19999students"]] +
                school_size_prob['20,000 students or more'] * post_df[["b_school_size20000studentsormore"]] +
                public_schl_prob['1'] * post_df[["b_public_schl"]]
            
            design_matrix <- data.frame(
                intrcpt = 1,
                time = seq(0, 10.5, by = .125), 
                time_sq = seq(0, 10.5, by = .125)^2
            )
            
            plot_df <- data.frame()
            for(r in 1:nrow(post_df)) {
                pred_y <- design_matrix[['intrcpt']] * post_df[['b0']][r] + 
                    design_matrix[['time']] * post_df[['b1']][r]

                tmp_df <- data.frame(
                    '.draw' = r,
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
        
        max_y <- max(PERC_PLOT_CONFIG[[yvar]][['y_breaks']])
        
        ggsave(
            plotlist[[yvar]],
            filename = "{PLOT_OUTPUT}/ACHA-NCHA_{yvar}_summary_plot_all_cov_linear_{max_y}.png" %>% glue(), 
            device = "png", 
            units = "in", 
            height = 5,
            width = 9,
            dpi = 600
        )
        
        # Create summary table of observed and fitted effects
        create_bin_summary_table(data, plot_df, yvar = var_name) %>% 
            write.csv("{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_summary_stats_all_cov_linear_{VERSION}.csv" %>% glue(), row.names = FALSE)
        
        summary_plot <- plotlist[[yvar]]
        
        # Save and remove contents from this run - add in checking logic above later 
        save(list = c('data', 'model', 'plot_df', 'summary_plot'), 
             file = '{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_summary_data_all_cov_linear.RData' %>% glue())
        remove(list=c('data', 'model', 'plot_df', 'summary_plot'))
        gc()
    }
}
