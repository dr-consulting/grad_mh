# File containing project-specific utilities
library(magrittr)

#' Loads .RData files in a given directory.
#' 
#' By default the funciton loads all .RData files in a given directory. The user can override this behavior by providing
#' A single file or vector of file names. There is no need to include .RData in the file name when identifying a subset 
#' of files (or a single file). Note that this function assumes that the only period in the fil
#' 
#' @param dir_path character filepath to a directory containing as set of .RData files to load
#' 
#' @param filename (optional) character filename. If no specific file is provided, the function 
#' loads all .RData files in the provided directory path.
#' 
#' @importFrom tools file_ext
#' @importFrom magrittr %>%

load_RData_files <- function(dir_path, filename = NULL) {
    
    if(is.null(filename)) {
        files <- list.files(dir_path)
        file_extensions <- lapply(
            files, 
            FUN = tools::file_ext
        ) %>% unlist()
        
        rdata_files <- files[which(tolower(file_extensions) == 'rdata')]
        
        for(f in rdata_files) {
            load(paste0(dir_path, '/', f), envir = .GlobalEnv)
        }
    }
    
    else {
        for(f in filename) {
            load(paste0(dir_path, '/', f), envir = .GlobalEnv)
        }
    }
}


#' Extracts and processes data, returning both a full data set with appended values and a modeling specific data set 
#' based on the variables and recipes in the provided data map
#' 
#' @param df data.frame with the raw data set values.
#' 
#' @param map_filepath character filepath name to a yaml file that contains original variable names and 
#' re-coding value maps.
#' 
#' @importFrom yaml read_yaml

create_study_datasets <- function(df, map_filepath) {
    # load map
    data_map <- yaml::read_yaml(map_filepath)
    study_cols <- names(data_map)
    
    # create simple local function to identify which have "responses" that need recoding 
    recode_mask_func <- function(x) !is.null(x[['responses']])
    recode_cols_mask <- data_map %>%
        map(~pluck(., recode_mask_func)) %>% 
        unlist()
    
    # Get the study names from the yaml file
    get_study_names <- function(x) x[['study_name']]
    study_names <- data_map %>%
        map(~pluck(., get_study_names)) %>% 
        unlist()
    
    # Identify variables that need recoding
    recode_names <- study_cols[recode_cols_mask]

    output_list <- helper_recode_func(df, data_map, recode_names, study_names)
    return(output_list)
}


#' Recodes dataframe using info pulled from the yaml file in the main function \code{create_study_datasets}
#' 
#' @param df target data.frame
#' 
#' @param data_map list read in from yaml file containing data renames and recoding recipes
#' 
#' @param recode_names a character vector of names derived from the data_map in pre-processing steps that take place in 
#' the parent function. These are the fields that need to be re-coded in some fashion. 
#' 
#' @param study_names a character vector of names derived from the data_map in pre-processing steps that take place in
#' the parent function. These are all of the fields that should be returned - whether they are recoded and renamed or 
#' just renamed according to the data_map. 
#' 
#' @seealso create_study_datasets 

helper_recode_func <- function(df, data_map, recode_names, study_names) {
    # Apply the recodes as appropriate from the data map
    # browser()
    df <- df %>% 
        imap_dfc(~ if (hasName(data_map, .y) && hasName(data_map[[.y]], "responses"))
            recode(.x, !!! data_map[[.y]][["responses"]], .default = NA_character_)
            ) %>% 
        setNames(map_chr(data_map, "study_name")[names(.)]) %>% 
        bind_cols(df, .)
    
    # Apply passthrough/name-change for variables that do not need recoding
    df <- df %>% 
        select_if((names(.) %in% setdiff(names(data_map), recode_names))) %>% 
        setNames(map_chr(data_map, "study_name")[names(.)]) %>% 
        bind_cols(df, .)

    study_df <- df[study_names]
    
    return(list(base_df = df, 
                study_df = study_df))
}


#' Strips attributes read in from read_spss function. Adapted from sjlabelled package
#' 
#' @param df a data.frame - usually imported from SPSS using the haven package.

strip_df_attributes <- function(df) {

    is_labelled <- function(x){
        inherits(x, c("labelled", "haven_labelled"))
    } 
    
    attr(df, "label") <- NULL
    attr(df, "labels") <- NULL 
    attr(df, "format.spss") <- NULL
    attr(df, 'display_width') <- NULL
    
    if (is_labelled(df)) df <- unclass(df)
    
    return(df)
}


#' Loads and preps ncha data from SPSS file format 
#' 
#' @param file_path absolute path to the target file - should be in .sav format
#' 
#' @importFrom haven read_spss

load_and_prep_ACHA_file <- function(file_path) {
    if(!file.exists(file_path)) {
        warning(paste("Unable to find:", file_path))
    }
    
    df <- haven::read_spss(file_path, user_na = FALSE)
    
    df_t <- lapply(df, strip_df_attributes) %>% 
        as.data.frame()
    
    names(df_t) <- names(df)
    
    return(df_t)
}


#' Prints latex tables for comparing recoded variables. 
#' 
#' This function is used in the .Rmd QA files to verify the correctness of transforms.
#' 
#' @param df data.frame that contains the two variables to construct the cross tabulations from.
#' 
#' @param row_var character label for the variable to place on the rows of the table.
#' 
#' @param col_var character label for the variable to place on the columns of the table.
#' 
#' @param col_sep character value that specifies column separation width. Passed to stargazer API.
#' 
#' @param font_size character value that specifies font size. Passed to the stargazer API. 

create_latex_crosstabs <- function(df, row_var, col_var, col_sep = "1pt", font_size = "footnotesize") {
    table(df[[row_var]], df[[col_var]]) %>% 
        as.matrix() %>% 
        as.data.frame.matrix() %>% 
        stargazer::stargazer(
            column.sep.width = col_sep, 
            font.size = font_size, 
            summary = FALSE, 
            header = FALSE
        )
}


#' Runs a principal components analysis with parallel analysis for detection of latent factors
#' 
#' @param df data.frame that contains only the numeric variables that will be used in a factor analysis
#' 
#' @param tbl_cap_base character text that is inserted at the start of the table caption to allow for
#' some customization.
#' 
#' @param n_sims integer representing the number of simulated uncorrelated data sets to generate. 
#' Passed to the nFactors API.
#' 
#' @param perc numeric value that is similar to an alpha threshold in null hypothesis testing. Used 
#' to identify thresholds for the parallel analysis when selecting factors. Passed to the nFactors API.
#' 
#' @param rotation character value with a valid rotation label that can be passed to the nFactors API.
#' 
#' @param suppress numeric value below which values are suppressed when the table is displayed. Passed
#' to the stargazer API.
#' 
#' @param col_palette vector of valid colors that can be passed to ggplot API.

pca_w_horns_pa <- function(df, tbl_cap_base, n_sims = 1000, perc = .05, rotation = "oblimin", suppress = .3, 
                           col_palette = c("#42bd92", "#426EBD")) {
    require(nFactors)
    df_nas_removed <- na.omit(df)
    cases_dropped <- nrow(df) - nrow(df_nas_removed)
    message("Dropped {cases_dropped} out of {nrow(df)} observations due to missing data" %>% glue::glue())
    
    ev1 <- eigen(cor(df_nas_removed)) # get eigenvalues
    ap1 <- parallel(subject=nrow(df_nas_removed),var=ncol(df_nas_removed),
                    rep=n_sims, cent=perc)
    nS1 <- nScree(x=ev1$values, aparallel=ap1$eigen$qevpea)
    
    plot_samp1<-data.frame(Component = 1:ncol(df_nas_removed), 
                           Eigenvalue = nS1$Analysis$Eigenvalues, 
                           PA = nS1$Analysis$Par.Analysis)
    
    res_list <- list()
    res_list[["scree_plot"]] <- plot_samp1 %>% 
        dplyr::select(Component, Eigenvalue, PA) %>% 
        gather(key = "Eigenvalue_src", value = "Eigenvalue", Eigenvalue, PA) %>%
        mutate(Eigenvalue_src = recode(Eigenvalue_src, "Eigenvalue"="Obtained Eigenvalues", 
                                       "PA"="Horn's Parallel Analysis")) %>% 
        ggplot(aes(group = Eigenvalue_src, x = Component, y = Eigenvalue, color = Eigenvalue_src, 
                   shape = Eigenvalue_src)) +
        geom_point(size = 1.5) +
        geom_line() + 
        scale_color_manual(values = c("Horn's Parallel Analysis" = col_palette[2], 
                                      "Obtained Eigenvalues" = col_palette[1]), name = "") +
        scale_shape_manual(values = c("Horn's Parallel Analysis" = 17, 
                                      "Obtained Eigenvalues" = 16), name = "") +
        theme_bw() +
        theme(legend.position = "bottom")
    
    retain_cnt <- which(plot_samp1$Eigenvalue > plot_samp1$PA)
    fit <- psych::principal(df_nas_removed, nfactors = length(retain_cnt), rotate = rotation)
    load_tbl <- unclass(fit$loadings)
    load_tbl <- round(load_tbl, digits = 3)
    load_tbl[abs(load_tbl) < suppress]<-""
    colnames(load_tbl) <- paste0("F", 1:length(retain_cnt))
    
    caption_text <- stringr::str_wrap("{tbl_cap_base} Number of factors retained determined using parallel analysis 
                                      - retaining only those factors that exceeded the {(1 - perc)*100}th percentile. 
                                      Loadings reported in the table based on {rotation} rotation. All loadings with an
                                      absolute value less than {suppress} are suppressed." %>% glue::glue())
    
    res_list[["pca"]] <- fit 
    res_list[["latex_table"]] <- knitr::kable(load_tbl, caption = caption_text, format = "latex", booktabs = TRUE)
    res_list[["pa_results"]] <- plot_samp1
    
    return(res_list)
}


#' One factor error covariance model generation helper
#' 
#' This function loops through a model and attempts to fit error covariances, using the
#' p_thresh value as a guide to determine whether to keep an added covariance. 
#' 
#' @param base_model character model specified in lavaan syntax.
#' 
#' @param data data.frame containing all model variables
#' 
#' @param ordered_vars vector of variables that should be treated as ordinal when 
#' fitting the model.
#' 
#' @param p_thresh probability threshold for model fit likelihood ratio tests. If the likelihood 
#' ratio test yields a p-value less than the threshold the added path covariance path is retained.

find_single_factor_final_model <- function(base_model, data, ordered_vars, p_thresh = .01) {
    message("Fitting base model")
    fit_base <- lavaan::cfa(base_model, data=data, ordered=ordered_vars, std.lv=TRUE)
    mod_index_base <- lavaan::modificationindices(fit_base) %>% 
        arrange(desc(mi)) %>% 
        filter(op == "~~")
    new_covar <- paste(mod_index_base[1, c('lhs', 'op', 'rhs')], collapse = "")
    
    message("Fitting first covariate model")
    covar_model <- paste0(base_model, "\n", new_covar)
    fit_covar <- lavaan::cfa(covar_model, data=data, ordered=ordered_vars, std.lv=TRUE)
    p_val <- anova(fit_base, fit_covar)[["Pr(>Chisq)"]][2]
    
    while(p_val < p_thresh) {
        if(exists('fit_covar_i')) {
            fit_covar <- fit_covar_i
        }
        
        mod_index_covar <- lavaan::modificationindices(fit_covar) %>% 
            arrange(desc(mi))  %>% 
            filter(op == "~~")
        new_covar <- paste(mod_index_covar[1, c('lhs', 'op', 'rhs')], collapse = "")
        
        message(paste("Fitting model with added covariance:", new_covar))
        
        covar_model <- paste0(covar_model, "\n", new_covar)
        fit_covar_i <- lavaan::cfa(covar_model, data=data, ordered=ordered_vars, std.lv=TRUE)
        p_val <- anova(fit_covar_i, fit_covar)[["Pr(>Chisq)"]][2]
    }
    result_list <- list()
    result_list[["base_fit"]] <- fit_base
    result_list[["fit_covar"]] <- NULL
    if(exists("fit_covar_i")) {
        result_list[["fit_covar"]] <- fit_covar
    }
    
    return(result_list)
}


#' Two factor error covariance model generator 
#' 
#' This function loops through a model and attempts to fit error covariances, using the
#' p_thresh value as a guide to determine whether to keep an added covariance. 
#' 
#' @param base_model character model specified in lavaan syntax.
#' 
#' @param data data.frame containing all model variables
#' 
#' @param ordered_vars vector of variables that should be treated as ordinal when 
#' fitting the model.
#' 
#' @param valid_covars a list of variables to include covariances between
#' 
#' @param invalid_covars a list of lavaan covariance elements that should not be included in 
#' any of the attempts to fit addtional covariances. 
#' 
#' @param p_thresh probability threshold for model fit likelihood ratio tests. If the likelihood 
#' ratio test yields a p-value less than the threshold the added path covariance path is retained.

find_dual_factor_final_model <- function(base_model, data, ordered_vars, valid_covars, invalid_covars, p_thresh = .01) {
    `%notin%` <- Negate(`%in%`)
    message("Fitting base model")
    fit_base <- lavaan::cfa(base_model, data=data, ordered=ordered_vars, std.lv=TRUE)
    mod_index_base <- lavaan::modificationindices(fit_base) %>% 
        arrange(desc(mi)) %>% 
        filter(op == "~~" & lhs %in% valid_covars & rhs %in% valid_covars)
    new_covars <- paste(mod_index_base[['lhs']], mod_index_base[['rhs']], sep = "~~")
    new_covar <- new_covars[new_covars %notin% invalid_covars][1]
    
    message("Fitting first covariate model")
    covar_model <- paste0(base_model, "\n", new_covar)
    fit_covar <- lavaan::cfa(covar_model, data=data, ordered=ordered_vars, std.lv=TRUE)
    p_val <- anova(fit_base, fit_covar)[["Pr(>Chisq)"]][2]
    
    if(is.null(invalid_covars)) invalid_covars <- c()
    
    while(p_val < p_thresh) {
        if(exists('fit_covar_i')) {
            fit_covar <- fit_covar_i
        }
        mod_index_covar <- lavaan::modificationindices(fit_covar) %>% 
            arrange(desc(mi)) %>% 
            filter(op == "~~" & lhs %in% valid_covars & rhs %in% valid_covars)
        new_covars <- paste(mod_index_covar[['lhs']], mod_index_covar[['rhs']], sep = "~~")
        new_covar <- new_covars[new_covars %notin% invalid_covars][1]
        
        if(is.na(new_covar)) {
            # Force a stop
            p_val <- 999
        } else {
            message(paste("Fitting model with added covariance:", new_covar))
            
            covar_model <- paste0(covar_model, "\n", new_covar)
            fit_covar_i <- lavaan::cfa(covar_model, data=data, ordered=ordered_vars, std.lv=TRUE)
            p_val <- anova(fit_covar_i, fit_covar)[["Pr(>Chisq)"]][2]
        }
    }
    
    result_list <- list()
    result_list[["base_fit"]] <- fit_base
    result_list[["fit_covar"]] <- NULL
    if(exists("fit_covar_i")) {
        result_list[["fit_covar"]] <- fit_covar
    }
    result_list[["invalid_covars"]] <- invalid_covars
    return(result_list)
}


#' Calculates the H coefficient given a lambda matrix returned from a fitted 
#' lavaan object - or any other factor loading matrix formatted in a similar 
#' structure
#' 
#' The H coefficient is a measure of latent variable reliability. 
#' 
#' @param lambda a matrix of factor loadings that can be used to calculate 
#' coefficient H. Assumes that the columns represent the latent factors. 

coefficient_H <- function(lamdba){
    lambda <- as.matrix(lamdba)
    H_result <- c()
    for(i in 1:ncol(lambda)){
        l <- lambda[,i][lambda[,i] != 0]
        numerator <- sum(l^2/(1-l^2))
        denominator <- 1 + numerator
        H <- numerator/denominator
        H_result <- c(H_result, H)
        names(H_result)[i] <- paste0("F", i)
    }
    return(H_result)
}


#' Generate model fit and empirical summary data. 
#' 
#' This is the primary summarization function used to calculate posterior 
#' distributions for the key metrics of interest (e.g., absolute change, risk ratios, etc.)
#' 
#' @param original_df data.frame containing the original values used to fit the model. Used
#' in the calculation of empirical summary stats.
#' 
#' @param fitted_df data.frame containing the fitted results from the model.
#' 
#' @param yvar character label identifying the target variable for summarization.
#' 
#' @param begin time index value to use as the starting point (defaults to 0 - the first time point).
#' 
#' @param end time inded value to use as the end point (defaults to 10.5 - the final time point).
#' 
#' @param time_var character label identifying the variable that represents "time". begin and end 
#' values must correspond to this variable.

create_bin_summary_table <- function(original_df, fitted_df, yvar, begin=0, end=10.5, time_var = "c_Time", 
                                     trials = NULL, weights=FALSE) {
    if(weights) {
        if(is.factor(original_df[[yvar]])) {
            original_df[[yvar]] <- as.numeric(original_df[[yvar]]) - 1
        }
        if('weights' %in% names(original_df)) {
            empirical_res <- original_df %>%
                filter(!!sym(time_var) %in% c(begin, end)) %>%
                group_by(!!sym(time_var)) %>%
                summarize(empirical_prop = sum(!!sym(yvar) * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE))
        }
        else if('normalized_weights' %in% names(original_df)) {
            empirical_res <- original_df %>% 
                filter(!!sym(time_var) %in% c(begin, end)) %>% 
                group_by(!!sym(time_var)) %>% 
                summarize(empirical_prop = sum(!!sym(yvar) * normalized_weights, na.rm = TRUE) / sum(normalized_weights, na.rm = TRUE) )
        }
        else{
            stop('ERROR could not find expected weights variable')
        }
    }
  
    else{
        empirical_res <- original_df %>% 
            filter(!!sym(time_var) %in% c(begin, end)) %>% 
            group_by(!!sym(time_var)) %>% 
            summarize(empirical_prop = mean(!!sym(yvar), na.rm = TRUE))
    }
    
    # if trials is provided - divide the mean by the number to get a proportion
    if(!is.null(trials)) {
        empirical_res <- empirical_res %>% 
            mutate(empirical_prop = empirical_prop / trials)
    }
    
    # Continue with the processing. 
    empirical_res <- empirical_res %>%  
        mutate(
            AY = c("2008-09", "2018-19"), 
            empirical_OR = c(NA, 
                             (empirical_prop[AY=="2018-19"] / (1 - empirical_prop[AY=="2018-19"])) /
                                 (empirical_prop[AY=="2008-09"] / (1 - empirical_prop[AY=="2008-09"]))
                             ),
            empirical_abs_diff = c(NA, 
                                   empirical_prop[AY=="2018-19"] - empirical_prop[AY=="2008-09"]),
            empirical_RR = c(NA, 
                             empirical_prop[AY=="2018-19"] / empirical_prop[AY=="2008-09"])
        )

    fitted_res <- fitted_df %>% 
        filter(!!sym(time_var) %in% c(begin, end))
    
    if (is.null(trials)) {
        fitted_res <- fitted_res %>% 
            mutate(fitted_prop = perc / 100)
    }
    else {
        fitted_res <- fitted_res %>% 
            mutate(fitted_prop = perc)
    }
    
    # Temporary hack to enable correct estimation of CIs
    # if('.draw' %in% names(fitted_res)){
    fitted_res <- fitted_res %>% 
        mutate(
            risk_ratio = rep(NA, nrow(.)),
            odds_ratio = rep(NA, nrow(.)), 
            abs_diff = rep(NA, nrow(.))
        )
    
    draws <- unique(fitted_res$.draw)
    
    for (draw in draws) {
        pred_y <- fitted_res[['fitted_prop']][fitted_res[['.draw']] == draw]
        fitted_res[['risk_ratio']][fitted_res[['.draw']] == draw] <- pred_y[2] / pred_y[1]
        fitted_res[['odds_ratio']][fitted_res[['.draw']] == draw] <- (pred_y[2] / (1 - pred_y[2])) / (pred_y[1] / (1 - pred_y[1]))
        fitted_res[['abs_diff']][fitted_res[['.draw']] == draw] <- pred_y[2] - pred_y[1]
    }

    fitted_res <- fitted_res %>% 
        mutate(
            AY = ifelse(!!sym(time_var) == begin, "2008-09", "2018-19")
        ) %>%
        # temporary recode of prop to disambiguate source from summary
        select(AY, prop=fitted_prop, risk_ratio, odds_ratio, abs_diff) %>% 
        group_by(AY) %>% 
        summarize(
            # Fitted prop stats
            fitted_prop = mean(prop),
            ub_prop_95 = quantile(prop, .975), 
            lb_prop_95 = quantile(prop, .025),
            # Fitted OR stats
            fitted_OR = mean(odds_ratio),
            fitted_OR_ub = quantile(odds_ratio, .975), 
            fitted_OR_lb = quantile(odds_ratio, .025),
            # Fitted RR Stats
            fitted_RR = mean(risk_ratio),
            fitted_RR_ub = quantile(risk_ratio, .975), 
            fitted_RR_lb = quantile(risk_ratio, .025),
            # Fitted AR Stats
            fitted_abs_diff= mean(abs_diff),
            fitted_abs_diff_ub = quantile(abs_diff, .975), 
            fitted_abs_diff_lb = quantile(abs_diff, .025)
        )

    final_df <- empirical_res %>%
        select(-!!sym(time_var)) %>% 
        left_join(fitted_res, by='AY') %>% 
        mutate(
            Variable = yvar
        ) %>% 
        select(Variable, AY, 
               empirical_prop, empirical_abs_diff, empirical_RR, empirical_OR, 
               fitted_prop, fitted_abs_diff, fitted_RR, fitted_OR, 
               lb_prop_95, ub_prop_95, 
               fitted_abs_diff_lb, fitted_abs_diff_ub,
               fitted_RR_lb, fitted_RR_ub,
               fitted_OR_lb, fitted_OR_ub)
    
    colnames(final_df) <- c("Variable", "Academic Year", 
                            paste("Observed", c("Probability", "Absolute Change", "Relative Change", "Odds Ratio")), 
                            paste("Fitted", c("Probability", "Absolute Change", "Relative Change", "Odds Ratio")), 
                            paste("CI -", c("Probability, LB", "Probability, UB", 
                                            "Absolute Change, LB", "Absolute Change, UB", 
                                            "Relative Change, LB", "Relative Change, UB", 
                                            "Odds Ratio, LB", "Odds Ratio, UB")))
    
    change_cols <- final_df %>% 
        select(contains('Odds Ratio'), contains('Relative Change'), contains('Absolute Change')) %>% 
        colnames()
    
    final_df[final_df[["Academic Year"]] == "2008-09", change_cols] <- NA
    return(final_df)
} 
