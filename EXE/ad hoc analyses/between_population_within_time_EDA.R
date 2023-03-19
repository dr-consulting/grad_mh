# -----------------------------------------------------------------------------
# Produce views and summaries of between group odds ratios  
# -----------------------------------------------------------------------------

# This file was used to produce comparisons of odds and risk ratios 
# for the matched NSDUH sample and the ACHA-NCHA sample

library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)

BASE_FILE <- '~/Desktop/grad_mh/project_config.R'
DATA_VERSION <- "2023-01-15"

# If missing the config file raise early.
# Likely just opened the repo in a different file system
if(!file.exists(BASE_FILE)){
    stop('ERROR: Missing project config file. {BASE_FILE}' %>% glue())
}

source(BASE_FILE)
sapply(list.files(R_DIR, full.names = TRUE), source)

# -----------------------------------------------------------------------------
# Configure series of script-level variables 
#   This section defines a set of variables used by the custom functions and for
#   loops below.
# -----------------------------------------------------------------------------

nsduh_yvars <- list(
    'Any Psychiatric Disorder' = c(var_name='anymi_12mos', data_prefix='nsduh_matched'),
    'Suicidal Thoughts' = c(var_name='suic_thnk_12mos', data_prefix='nsduh_matched'),
    'Suicide Attempt' = c(var_name='suic_try_12mos', data_prefix='nsduh_matched'),
    'Poor Health' = c(var_name='global_health_dich', data_prefix='nsduh_matched')
)

acha_yvars <- list(
    'Any Psychiatric Disorder' = c(var_name='any_dx_nsduh', data_prefix='dx_nsduh_any'),
    'Suicidal Thoughts' = c(var_name='Q30J_suic_thnk_r_12mos', data_prefix='dx_suic_thnk'),
    'Suicide Attempt' = c(var_name='Q30K_suic_try_r_12mos', data_prefix='dx_suic_try'),
    'Poor Health' = c(var_name='global_health_dich', data_prefix='global_health_dich')
)

time_vars <- c('c_Time', 'quad_c_Time')

covariates <- c(
    'Intercept'='Intercept', 
    'Time'='c_Time', 
    'Time^2'='quad_c_Time'
)

plotlist <- list()

plot_palette <- analogous_palette[1:2]
names(plot_palette) <- c('ACHA-NCHA', 'Matched Adults')
plot_df <- data.frame()

for(yvar in names(nsduh_yvars)){
    # Extract nsduh configuration elements 
    nsduh_var_name <- nsduh_yvars[[yvar]]['var_name']
    names(nsduh_var_name) <- NULL
    nsduh_data_prefix <- nsduh_yvars[[yvar]]['data_prefix']
    names(nsduh_data_prefix) <- NULL
    
    # Extract ACHA configuration elements
    acha_var_name <- acha_yvars[[yvar]]['var_name']
    names(acha_var_name) <- NULL
    acha_data_prefix <- acha_yvars[[yvar]]['data_prefix']
    names(acha_data_prefix) <- NULL
    
    nsduh_data_filepath <- "{POSTERIOR_OUTPUTS}/{nsduh_data_prefix}_{nsduh_var_name}.RData" %>% glue()
    acha_data_filepath <- "{POSTERIOR_OUTPUTS}/{acha_data_prefix}_no_cov.RData" %>% glue()
    
    load(nsduh_data_filepath)
    nsduh_results <- results_list
    
    load(acha_data_filepath)
    acha_results <- results_list

    post_df <- posterior_samples(nsduh_results$brms_fit, pars = c('b_Intercept', 'b_c_Time', 'b_quad_c_Time'))
    names(post_df) <- c('b0', 'b1', 'b2')
    
    design_matrix <- data.frame(
        intrcpt = 1,
        time = c(0, 10.5), 
        time_sq = c(0, 10.5)^2
    )
    
    nsduh_plot_df <- data.frame()
    for(r in 1:nrow(post_df)) {
        pred_y <- design_matrix[['intrcpt']] * post_df[['b0']][r] + 
            design_matrix[['time']] * post_df[['b1']][r] +
            design_matrix[['time_sq']] * post_df[['b2']][r]
        
        pred_y <- logits_to_prob(pred_y)
        
        tmp_df <- data.frame(
            .draw = r,
            yhat1 = pred_y[1],
            yhat2 = pred_y[2],
            risk_ratio = pred_y[2] / pred_y[1],
            odds_ratio = (pred_y[2] / (1 - pred_y[2])) / (pred_y[1] / (1 - pred_y[1])),
            population = "Matched Adults", 
            variable = yvar
        )
        
        nsduh_plot_df <- rbind(nsduh_plot_df, tmp_df)
    }
    
    post_df <- posterior_samples(acha_results$brms_fit, pars = c('b_Intercept', 'b_c_Time', 'b_quad_c_Time'))
    names(post_df) <- c('b0', 'b1', 'b2')
    
    design_matrix <- data.frame(
        intrcpt = 1,
        time = c(0, 10.5), 
        time_sq = c(0, 10.5)^2
    )
    
    acha_plot_df <- data.frame()
    for(r in 1:nrow(post_df)) {
        pred_y <- design_matrix[['intrcpt']] * post_df[['b0']][r] + 
            design_matrix[['time']] * post_df[['b1']][r] +
            design_matrix[['time_sq']] * post_df[['b2']][r]
        
        pred_y <- logits_to_prob(pred_y)
        
        tmp_df <- data.frame(
            .draw = r,
            yhat1 = pred_y[1],
            yhat2 = pred_y[2],
            risk_ratio = pred_y[2] / pred_y[1],
            odds_ratio = (pred_y[2] / (1 - pred_y[2])) / (pred_y[1] / (1 - pred_y[1])),
            population = "ACHA-NCHA", 
            variable = yvar
        )
        
        acha_plot_df <- rbind(acha_plot_df, tmp_df)
    }
    
    plot_df <- plot_df %>% 
        rbind(nsduh_plot_df) %>% 
        rbind(acha_plot_df)
}

plot_df %>% 
    ggplot(aes(y=variable, x=risk_ratio, fill=population, group=population)) +
    stat_dist_halfeye(
        alpha=.5, 
        point_interval = mode_hdci,
        .width = .95, 
        normalize='groups',
        slab_type = 'pdf'
    ) +            
    scale_fill_manual(
        values=plot_palette,
        na.translate = FALSE
    ) + 
    geom_vline(xintercept = 1, lty='dashed') +
    theme_minimal() +
    labs(title="Relative change in risk from fall 2008 to Spring 2019 (Risk ratios)",
         x='Risk ratio', 
         fill='Population', 
         y="")

ggsave(
    filename = "~/Desktop/Comparison_risk_ratios.png" %>% glue(), 
    device = "png", 
    units = "in", 
    height = 5,
    width = 9,
    dpi = 600
)
