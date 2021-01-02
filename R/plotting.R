# Utilities for creating project plots - exploratory and final
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