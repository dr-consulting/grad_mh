library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)

source('~/github/ATNL/grad_mh/project_config.R')
sapply(list.files(R_DIR, full.names = TRUE), source)

# Bring in the raw data - will be used to plot the weighted "empiricals"
load('{DATA_DIR}/NSDUH/nsduh_matched_study_data.RData' %>% glue())

# Create Variable list - Note the expectation is that the "Matched" samples are listed second
yvars <- list(
    'Any Psychiatric Disorder' = c(var_name='anymi_12mos', data_prefix='nsduh'),
    'Matched Any Psychiatric Disorder' = c(var_name='anymi_12mos', data_prefix='nsduh_matched'),
    'Suicidal Thoughts' = c(var_name='suic_thnk_12mos', data_prefix='nsduh'), 
    'Matched Suicidal Thoughts' = c(var_name='suic_thnk_12mos', data_prefix='nsduh_matched'),
    'Suicide Attempt' = c(var_name='suic_try_12mos', data_prefix='nsduh'),
    'Matched Suicide Attempt' = c(var_name='suic_try_12mos', data_prefix='nsduh_matched'),
    'Poor Health' = c(var_name='global_health_dich', data_prefix='nsduh'),
    'Matched Poor Health' = c(var_name='global_health_dich', data_prefix='nsduh_matched')
)

time_vars <- c('c_Time', 'quad_c_Time')

covariates <- c(
    'Intercept'='Intercept', 
    'Time'='c_Time', 
    'Time^2'='quad_c_Time'
)

plotlist <- list()

plot_palette <- c('black', analogous_palette)
names(plot_palette) <- c('Fitted All Adults', 'ACHA-NCHA', 'Weighted Matched Adults', 'Fitted Matched Adults')

for(yvar in names(yvars)){
    var_name <- yvars[[yvar]]['var_name']
    names(var_name) <- NULL
    data_prefix <- yvars[[yvar]]['data_prefix']
    names(data_prefix) <- NULL
    
    data_filepath <- "{POSTERIOR_OUTPUTS}/{data_prefix}_{var_name}.RData" %>% glue()
    
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
        
        if(grepl('Matched', yvar)) {
            filename_base <- 'NSDUH_matched_{yvar}' %>% glue()
            base_color <- analogous_palette[2]
            color_aes <- 'Matched'
        } else {
            filename_base <- 'NSDUH_{yvar}' %>% glue()
        }
        
        fixef_tbl %>% 
            write.csv("{SUMMARY_OUTPUT}/{filename_base}_coefficients_table.csv" %>% glue(), row.names = TRUE)
        
        plot_df <- data %>% 
            data_grid(weights = 1, c_Time=seq_range(c_Time, 22)) %>% 
            mutate(
                quad_c_Time = c_Time^2
            ) %>% 
            add_fitted_draws(model) %>% 
            mean_qi() %>% 
            mutate(
                perc = .value * 100, 
                perc_lb = .lower * 100, 
                perc_ub = .upper * 100
            )
        
        create_bin_summary_table(data, plot_df, yvar = var_name, weights = TRUE) %>% 
            write.csv("{SUMMARY_OUTPUT}/{filename_base}_summary_stats.csv" %>% glue(), row.names = FALSE)
        
        plt <- plot_df %>% 
            ggplot(aes(x = c_Time, y=.value * 100)) 
        
        if(grepl('Matched', yvar)) {
            plt <- plt + 
                geom_line(lwd = 1.25, aes(color = 'Fitted Matched Adults'))
        } else {
            plt <- plt + 
                geom_line(lwd = 1.25, aes(color = 'Fitted All Adults'))
        }
        
        plt <- plt +
            scale_x_continuous(breaks = seq(0.5, 10.5, by = 2), 
                               labels = seq(2009, 2019, by = 2)) +
            scale_y_continuous(breaks = NSDUH_PLOT_CONFIG[[yvar]][['y_breaks']],
                               labels = NSDUH_PLOT_CONFIG[[yvar]][['y_labels']], 
                               limits = NSDUH_PLOT_CONFIG[[yvar]][['y_limits']]) +
            scale_color_manual(values = plot_palette) +
            labs(title = paste0('NSDUH Outcomes: ', yvar), 
                 x = 'Year', 
                 y = 'Percentage') +
            theme_bw()
        
        if(grepl('Matched', yvar)) {
            plt <- plt + 
                geom_ribbon(aes(ymax=.upper * 100, ymin=.lower * 100, fill='Fitted Matched Adults'), alpha=.2) +
                scale_fill_manual(values = plot_palette)
        }
        
        plotlist[[yvar]] <- plt
        
        ggsave(
            plotlist[[yvar]],
            filename = "{PLOT_OUTPUT}/{filename_base}_summary_plot.png" %>% glue(), 
            device = "png", 
            units = "in", 
            height = 5,
            width = 9,
            dpi = 600
        )

        # Save and remove contents from this run - add in checking logic above later 
        save(list = c('data', 'model', 'plt'), 
             file = '{SUMMARY_OUTPUT}/{filename_base}_summary_data.RData' %>% glue())
        remove(list=c('data', 'model', 'plt'))
        gc()
    }
}

# make combined plots
base_vars <- c('Any Psychiatric Disorder', 'Suicidal Thoughts', 'Suicide Attempt', 'Poor Health')
ncha_yvars <- list('Any Psychiatric Disorder' = c(var_name='any_dx_nsduh', data_prefix='dx_nsduh_any'),
                   'Suicidal Thoughts' = c(var_name='Q30J_suic_thnk_r_12mos', data_prefix='suic_thnk'), 
                   'Suicide Attempt' = c(var_name='Q30K_suic_try_r_12mos', data_prefix='suic_try'), 
                   'Poor Health' = c(var_name='global_health_dich', data_prefix='global_health'))

for(var in base_vars) {
    if(var %in% names(ncha_yvars)){
        load('{SUMMARY_OUTPUT}/ACHA-NCHA_{var}_no_cov_summary_data.RData' %>% glue())
        list_pos <- which(grepl(var, names(plotlist)))
        acha_plot_df <- plot_df %>% 
            group_by(c_Time) %>% 
            summarize(lb = quantile(perc, .025), 
                      ub = quantile(perc, .975),
                      .value = mean(perc))
        
        tmp_plot <- plotlist[[max(list_pos)]] +
            geom_line(data=plotlist[[min(list_pos)]]$data, aes(color='Fitted All Adults'), lwd=1.25) +
#            geom_line(data = nsduh_weighted_summary_df, aes_string(x='c_Time', y = yvars[[list_pos[1]]]['var_name']), 
#                      lty='dashed', alpha = .5) +
#            geom_point(data = nsduh_weighted_summary_df, 
#                       aes(x=c_Time, y = !!sym(yvars[[list_pos[1]]]['var_name']), color="Weighted Matched Adults")) +
            geom_line(data = acha_plot_df, aes(x = c_Time, y = .value, color='ACHA-NCHA'), lwd = 1.25) +
            geom_ribbon(data = acha_plot_df, aes(x = c_Time, ymin = lb, ymax = ub, fill='ACHA-NCHA'), alpha=.2) +
            labs(title = '{var}: Comparing ACHA-NCHA, U.S. Adults, and Matched Population of U.S. Adults' %>% glue(), 
                 color = 'Comparison Group') +
            guides(fill=FALSE)
        
        ggsave(
            # cowplot::plot_grid(summary_plot + 
            #                        theme(legend.position = c(.25, .8), 
            #                              legend.title = element_text(size = 11), 
            #                              legend.text = element_text(size = 9), 
            #                              legend.background = element_rect(fill = 'lightgrey', 
            #                                                               color = 'darkgrey', 
            #                                                               size = .5)) +
            #                        labs(title = 'NCHA: {var}' %>% glue()), 
            #                    tmp_plot + 
            #                        theme(legend.position = c(.25, .9), 
            #                              legend.background = element_rect(fill = 'lightgrey', 
            #                                                               color = 'darkgrey', 
            #                                                               size = .5)), 
            #                    ncol = 2, axis = 'hv', align='tblr'),
            tmp_plot,
            filename = "{PLOT_OUTPUT}/NSDUH_{var}_combined_summary_plot.png" %>% glue(), 
            device = "png", 
            units = "in", 
            height = 8,
            width = 11,
            dpi = 600
        )
    }
}


