# Utilities for creating project plots - exploratory and final
library(tidyverse)

#' Creates exploratory plots for NSDUH raw and weighted outputs
create_nsduh_trend_plots <- function(df, xvar, yvar, yvar_hits, weight_var, mask_var, y_breaks,
                                     ylab = 'Proportion', xlab = 'Year') {
    g_raw <- df %>% 
        filter(!!sym(mask_var) == 1) %>% 
        group_by(!!sym(xvar)) %>% 
        summarize(prop = sum(!!sym(yvar) %in% yvar_hits, na.rm = TRUE) / sum(!is.na(!!sym(yvar)))) %>% 
        ggplot(aes_string(x = xvar, y = 'prop')) +
        geom_point(color = base_palette['line']) +
        geom_line(color = base_palette['line']) + 
        scale_y_continuous(breaks = y_breaks, limits = c(min(y_breaks), max(y_breaks))) + 
        labs(y = ylab, x = xlab, title = 'Raw') +
        theme_bw()
    
    g_wght <- df %>%  
        filter(!!sym(mask_var) == 1) %>% 
        mutate(wght_val = ifelse(!!sym(yvar) %in% yvar_hits, 1, 0) * !!sym(weight_var)) %>% 
        group_by(!!sym(xvar)) %>% 
        summarize(numerator = sum(wght_val, na.rm = TRUE), 
                  denominator = sum(ifelse(is.na(!!sym(yvar)), 0, 1) * !!sym(weight_var)), 
                  prop = numerator / denominator) %>% 
        ggplot(aes_string(x = xvar, y = 'prop')) +
        geom_point(color = base_palette['line']) +
        geom_line(color = base_palette['line']) + 
        scale_y_continuous(breaks = y_breaks, limits = c(min(y_breaks), max(y_breaks))) + 
        labs(y = ylab, x = xlab, title = 'Weighted') +
        theme_bw()
    
    return(list(raw = g_raw, 
                weigthed = g_wght))
}