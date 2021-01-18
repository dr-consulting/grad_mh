# File containing project-specific utilities
library(magrittr)

#' Loads .RData files in a given directory.
#' 
#' By default the funciton loads all .RData files in a given directory. The user can override this behavior by providing
#' A single file or vector of file names. There is no need to include .RData in the file name when identifying a subset 
#' of files (or a single file). Note that this function assumes that the only period in the fil
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
#' the parent function. These are the fields that need to be recoded in some fashion. 
#' 
#' @param study_names a character vector of names derived from the data_map in pre-processing steps that take place in
#' the parent function. These are all of the fields that should be returned - whether they are recoded and renamed or 
#' just renamed according to the data_map. 
#' 
#' @seealso create_study_datasets 

helper_recode_func <- function(df, data_map, recode_names, study_names) {
    # Apply the recodes as appropriate from the data map

    df <- df %>% 
        imap_dfc(~ if (hasName(data_map, .y) && hasName(data_map[[.y]], "responses"))
            recode(.x, !!! data_map[[.y]][["responses"]])) %>% 
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
#' @param df a \code{data.frame} - usually imported from SPSS using the haven package

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

#' Prints latex tables for comparing recoded variables
#' 

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




