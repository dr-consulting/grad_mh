library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)
library(Cairo)


BASE_FILE <- '~/Desktop/grad_mh/project_config.R'
DATA_VERSION <- "2023-01-15"
VERSION <- 'v20220319'

# If missing the config file raise early.
# Likely just opened the repo in a different file system
if(!file.exists(BASE_FILE)){
    stop('ERROR: Missing project config file. {BASE_FILE}' %>% glue())
}

source(BASE_FILE)
sapply(list.files(R_DIR, full.names = TRUE), source)

# Bring in the raw data - will be used to plot the weighted "empiricals"
load('{DATA_DIR}/NSDUH/nsduh_matched_study_data_{DATA_VERSION}.RData' %>% glue())

# Create Variable list - Note the expectation is that the "Matched" samples are listed second
yvars <- list(
    'Suicide Attempt' = c(var_name='suic_try_12mos', data_prefix='nsduh'),
    'Matched Suicide Attempt' = c(var_name='suic_try_12mos', data_prefix='nsduh_matched')
)

time_vars <- c('c_Time')

covariates <- c(
    'Intercept'='Intercept', 
    'Time'='c_Time' 
)

plotlist <- list()

# color_palette <- c('black', analogous_palette[1:2])
# names(color_palette) <- c('Fitted All Adults', 'Fitted ACHA-NCHA', 'Fitted Matched Adults')
# 
# fill_palette <- c("#C2CCDD", "#C8C1D8")
# names(fill_palette) <- c('Fitted ACHA-NCHA', 'Fitted Matched Adults')

color_palette <- c('#324961', analogous_palette[1], '#AD0037')
names(color_palette) <- c('Fitted All Adults', 'Fitted ACHA-NCHA', 'Fitted Matched Adults')

fill_palette <- c("#C2CCDD", "#e2979a")
names(fill_palette) <- c('Fitted ACHA-NCHA', 'Fitted Matched Adults')

for(yvar in names(yvars)){
    var_name <- yvars[[yvar]]['var_name']
    names(var_name) <- NULL
    data_prefix <- yvars[[yvar]]['data_prefix']
    names(data_prefix) <- NULL
    
    data_filepath <- "{POSTERIOR_OUTPUTS}/{data_prefix}_{var_name}_linear.RData" %>% glue()
    
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
            filename_base <- 'NSDUH_matched_{yvar}_linear' %>% glue()
        } else {
            filename_base <- 'NSDUH_{yvar}_linear' %>% glue()
        }
        
        fixef_tbl %>% 
            write.csv("{SUMMARY_OUTPUT}/{filename_base}_linear_coefficients_table.csv" %>% glue(), row.names = TRUE)
        
        post_df <- posterior_samples(model, pars = c('b_Intercept', 'b_c_Time'))
        names(post_df) <- c('b0', 'b1')
        
        design_matrix <- data %>% 
            data_grid(c_Time=seq_range(c_Time, 22)) %>% 
            mutate(
                intrcpt = 1
            )
        
        fitted_df <- data.frame()
        for(r in 1:nrow(post_df)) {
            pred_y <- design_matrix[['intrcpt']] * post_df[['b0']][r] + 
                design_matrix[['c_Time']] * post_df[['b1']][r]
            
            pred_y <- logits_to_prob(pred_y)
            
            tmp_df <- data.frame(
                .draw = r,
                c_Time = design_matrix[['c_Time']],
                prob = pred_y
            )
            
            fitted_df<- rbind(fitted_df, tmp_df)
        }
        
        fitted_df <- fitted_df %>% 
            mutate(perc = prob * 100)
        
        plot_df <- fitted_df %>% 
            group_by(c_Time) %>% 
            summarize(
                perc = mean(perc),
                perc_lb = quantile(prob * 100, .025),
                perc_ub = quantile(prob * 100, .975)
            )
        
        create_bin_summary_table(data, fitted_df, yvar = var_name, weights = TRUE) %>% 
            write.csv("{SUMMARY_OUTPUT}/{filename_base}_linear_summary_stats_{VERSION}.csv" %>% glue(), row.names = FALSE)
        
        plt <- plot_df %>% 
            ggplot(aes(x = c_Time, y=perc)) 
        
        if(grepl('Matched', yvar)) {
            plt <- plt + 
                geom_ribbon(aes(ymax=perc_ub, ymin=perc_lb, 
                                fill='Fitted Matched Adults'), alpha=.5) +
                geom_line(lwd = 1.25, aes(color = 'Fitted Matched Adults')) +
                scale_fill_manual(values = fill_palette)
            
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
            scale_color_manual(values = color_palette) +
            labs(title = paste0('NSDUH Outcomes: ', yvar), 
                 x = 'Year', 
                 y = 'Percentage') +
            theme_bw()
        
        plotlist[[yvar]] <- plt
        
        ggsave(
            plotlist[[yvar]],
            filename = "{PLOT_OUTPUT}/{filename_base}_linear_summary_plot_{VERSION}.png" %>% glue(), 
            device = "png", 
            units = "in", 
            height = 5,
            width = 9,
            dpi = 600
        )

        # Save and remove contents from this run - add in checking logic above later 
        save(list = c('data', 'model', 'plt'), 
             file = '{SUMMARY_OUTPUT}/{filename_base}_linear_summary_data.RData' %>% glue())
        remove(list=c('data', 'model', 'plt'))
        gc()
    }
}

# make combined plots
base_vars <- c('Suicide Attempt')
ncha_yvars <- list('Suicide Attempt' = c(var_name='Q30K_suic_try_r_12mos', data_prefix='dx_suic_try_no_cov_linear'))

for(var in base_vars) {
    # Loop through the variables provided above - can be a list of any length provided it contains the necessary information
    if(var %in% names(ncha_yvars)){
        load('{SUMMARY_OUTPUT}/ACHA-NCHA_{var}_no_cov_linear_summary_data.RData' %>% glue())
        list_pos <- which(grepl(var, names(plotlist)))
        acha_plot_df <- plot_df %>% 
            group_by(c_Time) %>% 
            summarize(perc_lb = quantile(perc, .025), 
                      perc_ub = quantile(perc, .975),
                      perc = mean(perc))
        
        # The "base" plot layer hill will be the matched sample plot.
        # We layer the all adult line and the fitted ACHA line plus its mean inferential interval below
        tmp_plot <- plotlist[[max(list_pos)]] +
            # Add a layer for the "All adults population"
            geom_line(data=plotlist[[min(list_pos)]]$data, aes(color='Fitted All Adults'), lwd=1.25) +
            # Add a layer with credibility interval for the ACHA-NCHA results
            geom_ribbon(data = acha_plot_df, aes(x = c_Time, ymin = perc_lb, ymax = perc_ub, fill='Fitted ACHA-NCHA'), 
                        alpha=.5) +
            geom_line(data = acha_plot_df, aes(x = c_Time, y = perc, color='Fitted ACHA-NCHA'), lwd = 1.25) +
            # Override existing fill aesthetic to include just the HDIs
            scale_fill_manual(values=fill_palette) +
            # Standard tile and caption content
            labs(title = '{var}: Comparing ACHA-NCHA, U.S. Adults, and Matched Population of U.S. Adults' %>% glue(), 
                 color = 'Comparison Group',
                 fill = "95% HDIs",
                 caption = "HDI = Highest Density Interval around the model-implied population mean", 
                 x="", 
                 y="") + 
            theme(axis.text.x = element_text(size=16), 
                  axis.text.y = element_text(size=16))

        # Save to generic output location
        ggsave(
            tmp_plot,
            filename = "{PLOT_OUTPUT}/NSDUH_{var}_linear_combined_summary_plot_{VERSION}.png" %>% glue(), 
            device = "png", 
            units = "in", 
            height = 8,
            width = 11,
            dpi = 600
        )
        
        # Save to generic output location
        ggsave(
            tmp_plot,
            filename = "{PLOT_OUTPUT}/NSDUH_{var}_linear_combined_summary_plot_{VERSION}.eps" %>% glue(), 
            device = cairo_ps, 
            units = "in", 
            height = 8,
            width = 11,
            dpi = 600
        )
    }
}


