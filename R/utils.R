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