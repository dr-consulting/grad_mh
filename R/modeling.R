library(tidyverse)

#' Script containing the the basic modeling functions
#' 

logistic_model_wrapper <- function(data, y_var, id_var, lv1_vars, lv1_ran_vars, lv2_int_vars,
                                   output_folder, warmup, iter, chains, future_arg, control_list, 
                                   model_save_name=NULL){
    start_time <- Sys.time()
    results_list <- list()
    results_list[["model_data"]] <- data
    
    model <- "
    {y_var} ~ {paste(c(1, lv1_vars), collapse=' + ')} +
    ({paste(c(1, lv1_ran_vars), collapse=' + ')} | {id_var}) +
    {paste(lv2_int_vars, collapse=' + ')} 
    " %>% glue::glue() %>% 
        brms::bf() + brms::bernoulli()
    
    results_list[["brms_formula"]] <- model
    results_list[["brms_priors"]] <- brms::get_prior(model, data)
    
    fit <- brms::brm(
        model, 
        data = data, 
        iter = iter, 
        warmup = warmup,
        chains = chains, 
        cores = chains, 
        control = control_list,
        future = future_arg, 
        save_model = ifelse(!is.null(model_save_name), model_save_name, NULL)
    )
    
    run_time <- difftime(Sys.time(), start_time, units = "hours")
    results_list[["run_time"]] <- run_time
    results_list[["brms_fit"]] <- fit
    
    if(!is.null(model_save_name)) {
        save(list = c('results_list'), 
             file = "{output_folder}/{model_save_name}.RData" %>% glue::glue())
        results_list[["model_name"]] <- model_save_name
    }
    
    return(results_list)
} 

