library(tidyverse)

#' logistic model wrapper is a function that wraps brms functions
#' 
#' In addition to running the model and returning the output, the function also accepts several arguments that govern 
#' the saving of the model object and output files (required for post-processing)
#' 

logistic_model_wrapper <- function(data, prior_config, y_var, id_var, lv1_vars, lv1_ran_vars, lv2_int_vars,
                                   output_folder, warmup, iter, chains, control_list, future_arg = FALSE, 
                                   model_save_name=NULL){
    # TODO: update or remove prior_config parameter (currently not used)
    start_time <- Sys.time()
    results_list <- list()
    results_list[["model_data"]] <- data
    
    
    model <- "
    {y_var} ~ {paste(c(1, lv1_vars), collapse=' + ')} +
    ({paste(c(1, lv1_ran_vars), collapse=' + ')} | {id_var}) 
    " %>% glue::glue() 
    
    if(!is.null(lv2_int_vars)) {
        model <- "{model} + {paste(lv2_int_vars, collapse=' + ')}" %>% glue::glue()
    }
    
    model <- model %>% brms::bf() + brms::bernoulli()
    
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


#' ordinal regression using a cumulative log-odds model structure that wraps brms functions.
#' 
#' In addition to running the model and returning the output, the function also accepts several arguments that govern 
#' the saving of the model object and output files (required for post-processing). Currently this patricular function
#' is only relevant for the global health variable - which is a single item 5-response measure tapping overall health.
#' 
#' The function uses a logit function for the log-odds and employs a flexible threshold (which ensures that not all 
#' transitions in rank from a lower to a higher value need to maintain the same log-odds). 

cumulative_model_wrapper <- function(data, prior_config, y_var, id_var, lv1_vars, lv1_ran_vars, lv2_int_vars,
                                   output_folder, warmup, iter, chains, control_list, future_arg = FALSE, 
                                   model_save_name=NULL){
    start_time <- Sys.time()
    results_list <- list()
    results_list[["model_data"]] <- data
    
    model <- "
    {y_var} ~ {paste(c(1, lv1_vars), collapse=' + ')} +
    ({paste(c(1, lv1_ran_vars), collapse=' + ')} | {id_var}) +
    {paste(lv2_int_vars, collapse=' + ')} 
    " %>% glue::glue() %>% 
        brms::bf() + brms::cumulative(link = "logit", threshold = "flexible")
    
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

#' ordinal regression using a cumulative log-odds model structure that wraps brms functions.
#' 
#' In addition to running the model and returning the output, the function also accepts several arguments that govern 
#' the saving of the model object and output files (required for post-processing). Currently this patricular function
#' is only relevant for the global health variable - which is a single item 5-response measure tapping overall health.
#' 
#' The function uses a logit function for the log-odds and employs a flexible threshold (which ensures that not all 
#' transitions in rank from a lower to a higher value need to maintain the same log-odds). 

poisson_model_wrapper <- function(data, prior_config, y_var, id_var, lv1_vars, lv1_ran_vars, lv2_int_vars,
                                  output_folder, warmup, iter, chains, control_list, future_arg = FALSE, 
                                  model_save_name=NULL){
    start_time <- Sys.time()
    results_list <- list()
    results_list[["model_data"]] <- data
    
    model <- "
    {y_var} ~ {paste(c(1, lv1_vars), collapse=' + ')} +
    ({paste(c(1, lv1_ran_vars), collapse=' + ')} | {id_var}) +
    {paste(lv2_int_vars, collapse=' + ')} 
    " %>% glue::glue() %>% 
        brms::bf() + poisson()
    
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


#' ordinal regression using a cumulative log-odds model structure that wraps brms functions.
#' 
#' In addition to running the model and returning the output, the function also accepts several arguments that govern 
#' the saving of the model object and output files (required for post-processing). Currently this patricular function
#' is only relevant for the global health variable - which is a single item 5-response measure tapping overall health.
#' 
#' The function uses a logit function for the log-odds and employs a flexible threshold (which ensures that not all 
#' transitions in rank from a lower to a higher value need to maintain the same log-odds). 

binomial_model_wrapper <- function(data, prior_config, y_var, trials, id_var, lv1_vars, lv1_ran_vars, lv2_int_vars,
                                   output_folder, warmup, iter, chains, control_list, future_arg = FALSE, 
                                   model_save_name=NULL){
    start_time <- Sys.time()
    results_list <- list()
    results_list[["model_data"]] <- data
    
    model <- "
    {y_var} | trials({trials}) ~ {paste(c(1, lv1_vars), collapse=' + ')} +
    ({paste(c(1, lv1_ran_vars), collapse=' + ')} | {id_var}) +
    {paste(lv2_int_vars, collapse=' + ')} 
    " %>% glue::glue() %>% 
        brms::bf() + binomial()
    
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


# A set of project specific utilities
logits_to_prob <- function(logits){
    odds <- exp(logits)
    prob <- odds / (1 + odds)
    return(prob)
}
