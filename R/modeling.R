library(tidyverse)

#' logistic model wrapper is a function that wraps brms functions
#' 
#' In addition to running the model and returning the output, the function also accepts several arguments that govern 
#' the saving of the model object and output files (required for post-processing). This function is the primary model
#' function used for the ACHA-NCHA graduate student analyses
#' 
#' @param data data.frame containing the training data
#' 
#' @param data list containing configuration for the model parameter's priors
#' 
#' @param y_var character label for the y variable in the data.frame
#' 
#' @param id_var character label for the group identity variable - used to define the levels of random effects
#' 
#' @param lv1_vars character vector of labels for the predictors in the model at the "observation" level of the model. 
#' sometimes also referred to as the level-1 variables
#' 
#' @param lv1_ran_vars character vector of labels for the predictors free to vary across the id_var variable levels.
#' 
#' @param lv2_int_vars character vector of labels for predictors of the random intercept for each level of the id_var. 
#' These are variables measured at the level of the id_var.
#' 
#' @param output_folder character filepath to location where outputs should be stored from the model
#' 
#' @param warmup numeric integer value for the number of warmup iterations. Passed to the brms model API.
#' 
#' @param iter numeric integer value for the number of total iterations. Passed to the brms model API.
#' 
#' @param chains numeric integer value for total number of MCMC chains to sample. Passed to the brms model API.
#' 
#' @param control_list list of additional arguments to configure the model fitting process. 
#' 
#' @param future_arg boolean, optional. If set to TRUE attempts to leverage additional parallelization of the 
#' MCMC computations. Default is set to FALSE.
#' 
#' @param model_save_name character filename to save the compiled Stan model produced when fitting a model with brms.
#' 
#' @param chain_hyper_threading boolean, optional. Default is FALSE which parallelizes computations for each chain
#' on a single core. If TRUE, the computations for each chain will be threaded - adding additional parallelization and speeding
#' compute times. TRUE is only recommended for multicore machines (recommend at least 12 cores). See this url for 
#' additional information: https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html.
#' 
#' @param max_threads integer, optional. The number of threads to use for each chain when chain_hyperthreading is TRUE.
#' Default value is 6 - adjust to what makes sense for your machine (running 3 chains, with a max number of 4 threads per
#' chain requires 12 cores.)

logistic_model_wrapper <- function(data, prior_config, y_var, id_var, lv1_vars, lv1_ran_vars, lv2_int_vars,
                                   output_folder, warmup, iter, chains, control_list, future_arg = FALSE, 
                                   model_save_name=NULL, chain_hyperthreading=FALSE, max_threads=6){
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
    
    if(chain_hyperthreading){
        fit <- brms::brm(
            model, 
            data = data, 
            iter = iter, 
            warmup = warmup,
            chains = chains, 
            cores = chains,
            backend = "cmdstanr",
            threads = threading(max_threads),
            control = control_list,
            future = future_arg, 
            save_model = ifelse(!is.null(model_save_name), model_save_name, NULL)
        )
    }
    else {
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
    }
    
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
#' 
#' @param data data.frame containing the training data
#' 
#' @param data list containing configuration for the model parameter's priors
#' 
#' @param y_var character label for the y variable in the data.frame
#' 
#' @param id_var character label for the group identity variable - used to define the levels of random effects
#' 
#' @param lv1_vars character vector of labels for the predictors in the model at the "observation" level of the model. 
#' sometimes also referred to as the level-1 variables
#' 
#' @param lv1_ran_vars character vector of labels for the predictors free to vary across the id_var variable levels.
#' 
#' @param lv2_int_vars character vector of labels for predictors of the random intercept for each level of the id_var. 
#' These are variables measured at the level of the id_var.
#' 
#' @param output_folder character filepath to location where outputs should be stored from the model
#' 
#' @param warmup numeric integer value for the number of warmup iterations. Passed to the brms model API.
#' 
#' @param iter numeric integer value for the number of total iterations. Passed to the brms model API.
#' 
#' @param chains numeric integer value for total number of MCMC chains to sample. Passed to the brms model API.
#' 
#' @param control_list list of additional arguments to configure the model fitting process. 
#' 
#' @param future_arg boolean, optional. If set to TRUE attempts to leverage additional parallelization of the 
#' MCMC computations. Default is set to FALSE.
#' 
#' @param model_save_name character filename to save the compiled Stan model produced when fitting a model with brms.

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

#' Poisson regression using a log-link model structure that wraps brms functions.
#' 
#' In addition to running the model and returning the output, the function also accepts several arguments that govern 
#' the saving of the model object and output files (required for post-processing). Currently this particular function
#' is only relevant for exploring alternative models for cumulative negative emotion ratings.
#' 
#' @param data data.frame containing the training data
#' 
#' @param data list containing configuration for the model parameter's priors
#' 
#' @param y_var character label for the y variable in the data.frame
#' 
#' @param id_var character label for the group identity variable - used to define the levels of random effects
#' 
#' @param lv1_vars character vector of labels for the predictors in the model at the "observation" level of the model. 
#' somtimes also referred to as the level-1 variables
#' 
#' @param lv1_ran_vars character vector of labels for the predictors free to vary across the id_var variable levels.
#' 
#' @param lv2_int_vars character vector of labels for predictors of the random intercept for each level of the id_var. 
#' These are variables measured at the level of the id_var.
#' 
#' @param output_folder character filepath to location where outputs should be stored from the model
#' 
#' @param warmup numeric integer value for the number of warmup iterations. Passed to the brms model API.
#' 
#' @param iter numeric integer value for the number of total iterations. Passed to the brms model API.
#' 
#' @param chains numeric integer value for total number of MCMC chains to sample. Passed to the brms model API.
#' 
#' @param control_list list of additional arguments to configure the model fitting process. 
#' 
#' @param future_arg boolean, optional. If set to TRUE attempts to leverage additional parallelization of the 
#' MCMC computations. Default is set to FALSE.
#' 
#' @param model_save_name character filename to save the compiled Stan model produced when fitting a model with brms.

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


#' binomial regression model that wraps brms functions.
#' 
#' This is an alternative specification of a logstic model
#' 
#' Note that this is not the primary modeling function. We use the logistic_model_wrapper throughout
#' 
#' 
#' @param data data.frame containing the training data
#' 
#' @param data list containing configuration for the model parameter's priors
#' 
#' @param y_var character label for the y variable in the data.frame
#' 
#' @param id_var character label for the group identity variable - used to define the levels of random effects
#' 
#' @param lv1_vars character vector of labels for the predictors in the model at the "observation" level of the model. 
#' somtimes also referred to as the level-1 variables
#' 
#' @param lv1_ran_vars character vector of labels for the predictors free to vary across the id_var variable levels.
#' 
#' @param lv2_int_vars character vector of labels for predictors of the random intercept for each level of the id_var. 
#' These are variables measured at the level of the id_var.
#' 
#' @param output_folder character filepath to location where outputs should be stored from the model
#' 
#' @param warmup numeric integer value for the number of warmup iterations. Passed to the brms model API.
#' 
#' @param iter numeric integer value for the number of total iterations. Passed to the brms model API.
#' 
#' @param chains numeric integer value for total number of MCMC chains to sample. Passed to the brms model API.
#' 
#' @param control_list list of additional arguments to configure the model fitting process. 
#' 
#' @param future_arg boolean, optional. If set to TRUE attempts to leverage additional parallelization of the 
#' MCMC computations. Default is set to FALSE.
#' 
#' @param model_save_name character filename to save the compiled Stan model produced when fitting a model with brms.

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


#' Converts values on a logit scale to probablities
#' 
#' @param logits a numeric vector of values on a logit scale that require transformation to probabilities.

logits_to_prob <- function(logits){
    odds <- exp(logits)
    prob <- odds / (1 + odds)
    return(prob)
}
