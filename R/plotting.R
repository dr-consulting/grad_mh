# Utilities for creating project plots - exploratory and final
library(brms)
library(corrr)
library(ggraph)
library(modelr)
library(tidybayes)
library(tidyverse)

#' Creates exploratory plots for NSDUH raw and weighted outputs
create_nsduh_trend_plots <- function(df, xvar, yvar, yvar_hits, weight_var, mask_var, y_breaks,
                                     ylab = 'Proportion', xlab = 'Year') {
    df_raw <- df %>% 
        filter(!!sym(mask_var) == 1) %>% 
        group_by(!!sym(xvar)) %>% 
        summarize(prop = sum(!!sym(yvar) %in% yvar_hits, na.rm = TRUE) / sum(!is.na(!!sym(yvar)))) %>% 
        mutate(type = 'Raw') %>% 
        select(!!sym(xvar), prop, type)
        
    df_comb<- df %>%  
        filter(!!sym(mask_var) == 1) %>% 
        mutate(wght_val = ifelse(!!sym(yvar) %in% yvar_hits, 1, 0) * !!sym(weight_var)) %>% 
        group_by(!!sym(xvar)) %>% 
        summarize(numerator = sum(wght_val, na.rm = TRUE), 
                  denominator = sum(ifelse(is.na(!!sym(yvar)), 0, 1) * !!sym(weight_var)), 
                  prop = numerator / denominator) %>%
        mutate(type = "Weighted") %>% 
        select(!!sym(xvar), prop, type) %>% 
        rbind(., df_raw)
        
        
    g <- ggplot(data = df_comb, aes(x = !!sym(xvar), y = prop, group = type, color = type)) +
        geom_point() +
        geom_line() +
        scale_color_manual(values = c("Raw" = analogous_palette[[1]], "Weighted" = analogous_palette[[2]])) +
        scale_y_continuous(breaks = y_breaks, limits = c(min(y_breaks), max(y_breaks))) + 
        labs(y = ylab, x = xlab) +
        theme_bw() +
        theme(legend.position = 'bottom', 
              legend.title = element_blank())
    
    return(g)
}

#' Creates correlation network graph
#'
#' @param df \code{data.frame} containing target variables
#' 
#' @param vars character vector of variables to generate correlation networks from
#' 
#' @param cor_method character indicating correlation type (e.g. "spearman", "pearson"). Value is passed to 
#' \code{corrr:correlate()}
#' 
#' @param cor_cutoff value between 0 and 1. Applies an absolute threshold when creating the correlation network. Strong
#' negative correlations and strong positive correlations will be part of the same network. 
#' 
#' @param label_map (optional) a named vector in which the names are the variable names passed in via the \code{vars} 
#' parameter and the values are the preferred renames. Helpful from translating confusing variable names to labels that
#' contain a more obvious meaning. 

create_cor_network_plot <- function(df, vars, cor_method, label_map, color_map, cor_cutoff) {
    if(is.null(cor_cutoff)) cor_cutoff <- 0
    
    cor_matrix <- df %>% 
        select(all_of(vars)) %>% 
        mutate_all(as.numeric) %>% 
        correlate(method = cor_method, use = 'pairwise.complete.obs') %>% 
        shave() %>% 
        stretch(na.rm = TRUE) %>% 
        mutate(
            x = str_replace_all(x, pattern = label_map), 
            y = str_replace_all(y, pattern = label_map)
        ) %>% 
        filter(abs(r) >= cor_cutoff)
    
    cor_tbl_graph<- cor_matrix %>% 
        as_tbl_graph(directed = FALSE)
    
    node_from <- cor_tbl_graph %>%
        as_tibble() %>%
        mutate(
            from = row_number(), 
            from_name = name
        ) %>% 
        select(from_name, from)
    
    node_to <- cor_tbl_graph %>%
        as_tibble() %>%
        mutate(
            to = row_number(), 
            to_name = name
        ) %>% 
        select(to_name, to)
    
    p <- 
    cor_tbl_graph %>% 
        activate(edges) %>% 
        left_join(node_from) %>% 
        left_join(node_to) %>% 
    ggraph(layout = 'linear', circular = TRUE) +
        geom_edge_arc(aes(width = r, color = from_name, alpha = r), show.legend = FALSE) +
        geom_edge_arc(aes(width = r, color = to_name, alpha = r), show.legend = FALSE) +
        theme_graph() +
        scale_color_manual(values = color_map) +
        scale_edge_color_manual(values = color_map) +
        scale_alpha_continuous(range = c(0.05, 0.40))
    
    p <- p + 
        geom_node_text(
        aes(label = name), size = 4, repel = TRUE, check_overlap = TRUE, 
        nudge_x = p$data$x *.35, nudge_y = p$data$y *.2)
    
    return(p)
}


#' Utility for create a base predictive data frame for use in generating model-based graphics and summaries
#' 
#' The function assumes the existence of a "data" object in the parent environment. This function is written for the 
#' creation of a base data frame specifically for the models used in this study. The hard coding is intentional and 
#' makes the function brittle by design. 
#' 
#' @param n_per_school number of observations to produce in the base prediction data.frame for the "hypothetical" 
#' school. 
#' 
#' @examples 
#' load("/path/to/fitted_data.RData")
#' data <- results_list[["brms_fit"]]$data
#' base_pred_df <- create_base_pred_df(5000)
#' 
#' @return \code{data.frame} that sets responses to predictors based on prevalence in the observed data
#' 

create_base_pred_df <- function(n_per_school) {
    # Hard coded and assumes a data.frame named data with these variables in the parent environment
    # For now the goal is to make this brittle the use of a function is just convenience 
    # See the examples section for intended use 
    gender_prob <- table(data[["Q47_gender"]]) / nrow(data)
    race_ethn_prob <- table(data[["race_ethn"]]) / nrow(data)
    enrollment_prob <- table(data[["Q52_enrollment"]]) / nrow(data)
    international_prob <- table(data[["Q55_international"]]) / nrow(data)
    survey_method_prob <- table(data[["survey_method"]]) / nrow(data)
    school_size_prob <- table(data[["school_size"]]) / nrow(data)
    public_schl_prob <- table(data[["public_schl"]]) / nrow(data)
    
    df <- tibble(
        school_id = rep(-9999, n_per_school), 
    ) %>%
        mutate(
            c_Q46_age = 0,
            Q47_gender = sample(names(gender_prob), prob = gender_prob, replace = TRUE, size = n()), 
            race_ethn = sample(names(race_ethn_prob), prob = race_ethn_prob, replace = TRUE, size = n()),
            Q52_enrollment = sample(names(enrollment_prob), prob = enrollment_prob, replace = TRUE, size = n()), 
            Q55_international = sample(names(international_prob), prob = international_prob, replace = TRUE, size = n()),
            survey_method = sample(names(survey_method_prob), prob = survey_method_prob, replace = TRUE, size = n()),
            school_size = sample(names(school_size_prob), prob = school_size_prob, replace = TRUE, size = n()), 
            public_schl = sample(names(public_schl_prob), prob = public_schl_prob, replace = TRUE, size = n()) %>% 
                as.numeric(), 
            school_id = -9999
        )
}


#' Takes as the main input the base prediction data frame to make a longer data frame that hold constant the base_df
#' properties / variables while extending the data frame based on number of additional time points required for plotting
#' 
#' The idea here is that our predictions and intervals are based on a "hypothetical" school that was not in the data. 
#' We explicitly do not want to create a fit line for an existing school. Instead, our goal is to generate a line for 
#' an "out of sample" school - what would the model predict without a starting random intercept and slope for a 
#' particular school id. To obtain accurate estimates at the population level we fix the prediction data at each 
#' interval to the same values. Those values are generated above using the observed probabilities of each response. 
#' 
#' @param base_df \code{data.frame} the output of \code{create_base_pred_df()}
#' 
#' @param n_time_points an integer reprsenting the number of time points to create between the min and max values
#' 
#' @param time_min the lowest value for the time variable used in the model - in this case \code{c_Time}
#' 
#' @param time_max the highest value for the time variable used in the model. 
#' 
#' @examples 
#' load("/path/to/fitted_data.RData")
#' data <- results_list[["brms_fit"]]$data
#' base_pred_df <- create_base_pred_df(5000)
#' full_pred_df <- create_full_pred_df(base_pred_df, 44, 0, 10.5)
#'  
#' @return \code{data.frame} that can be fed into \code{create_plot_df()}
#'  

create_full_pred_df <- function(base_df, n_time_points, time_min, time_max) {
    comb_df <- data.frame()
    for(t in seq(time_min, time_max, length.out = n_time_points)) {
        tmp_df <- base_df
        tmp_df[["c_Time"]] <- t
        tmp_df[["quad_c_Time"]] <- t^2
        comb_df <- rbind(comb_df, tmp_df)
    }
    
    return(comb_df)
}


#' Generates the data needed to create a lineribbon plot of the model predictions over time
#' 
#' @param df \code{data.frame} the result of \code{create_full_pred_df()}
#' 
#' @param n_samples integer number of draws of fitted values from the posterior. Caution as large values can exceed 
#' memory resources
#' 
#' @return \code{data.frame}
#' 

create_plot_df <- function(df, n_samples) {
    df %>% 
        add_fitted_draws(model, n = n_samples, re_formula = NA) %>% 
        ungroup() %>% 
        mutate(
            perc = .value*100, 
        ) %>% 
        select(c_Time, perc)
}

#' Generates posterior plot using fitted df, group df and several parameters that can be passed down into the ggplot
#' object to affect display. 
#' 
#' @param fitted_df \code{data.frame} returned from \code{create_plot_df} 
#' 
#' @param group_df \code{data.frame} with percentages of affirmative responses to the target variable as a function of 
#' both school id and time point. Recommend setting a threshold at something like n >= 30 to prevent very high or very
#' low values from distorting the scale of the y-axis. 
#' 
#' @param title plot title
#' 
#' @param y_breaks values at which to create breaks on the y-axis
#' 
#' @param y_labels labels displayed on the y-axis
#' 
#' @param x_breaks values at which to create breaks on the x-axis
#' 
#' @param x_labels labels displayed on the x-axis
#' 
#' @param color_pal RColorBrewer color palette name - prefer "Blues"
#' 
#' @param caption (optional) string to display at the bottom of the plot. 
#' 
#' @param subtitle (optional) string to include as a subtitle
#' 
#' @retrun \code{ggplot} object
#' 

create_percent_summary_plot <- function(fitted_df, group_df, title, y_breaks, y_labels, y_limits, x_breaks, x_labels, 
                                        color_pal, caption=NULL, subtitle=NULL) {
    fitted_df %>% 
        ggplot(aes(x = c_Time, y = perc)) +
        stat_lineribbon(.width = .95, 
                        aes(fill = "95% Credibility Interval", 
                            color = "MLM Fit"), 
                        alpha = .85, point_interval = mean_qi) + 
        scale_color_manual("MLM Model Posterior", aesthetics = c("color", "fill"),
                           breaks = c("MLM Fit", "95% Credibility Interval"), 
                           values = c("95% Credibility Interval" = RColorBrewer::brewer.pal(9, color_pal)[2], 
                                      "MLM Fit" = RColorBrewer::brewer.pal(9, color_pal)[9])) +
        scale_x_continuous(breaks = x_breaks, labels = x_labels) +
        scale_y_continuous(breaks = y_breaks, labels = y_labels) +
        coord_cartesian(ylim = y_limits) +
        geom_jitter(data=grp_df, aes(x = !!sym(xvar), y = perc, size = count), 
                    alpha = .1875, width = .075, color = RColorBrewer::brewer.pal(9, color_pal)[8], 
                    fill = RColorBrewer::brewer.pal(9, color_pal)[3]) +
        theme_bw() +
        labs(x = "Year", 
             y = "Percentage", 
             size = "Respondents per Institution", 
             color = "Posterior Predictions", 
             title = title, 
             subtitle = subtitle, 
             caption = caption) + 
        scale_size(breaks = c(500, 1000, 2000, 4000), labels = c("500", "1,000", "2,000", "4,000")) +
        guides(color = guide_legend(order = 1, 
                                    override.aes = list(lwd = c(1, 5))),
               size = guide_legend(order = 2),
               fill = FALSE) +
        theme(legend.title = element_text(size = 14), 
              legend.text = element_text(size = 12))
}
