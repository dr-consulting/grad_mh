# Utilities for creating project plots - exploratory and final
library(corrr)
library(ggraph)
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
