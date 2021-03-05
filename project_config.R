# File containing basic configurations and settings for project
library(glue)
library(magrittr)

LOCAL_REPO <- '~/github/ATNL/grad_mh'
DATA_DIR <- paste0(LOCAL_REPO, '/', 'data')
R_DIR <- paste0(LOCAL_REPO, '/', 'R')
MAPS_DIR <- paste0(LOCAL_REPO, '/', 'data_maps')
POSTERIOR_OUTPUTS <- paste0(DATA_DIR, "/", "OUTPUTS")

# Create a directory at the location above if one does not already exist
if(!dir.exists(POSTERIOR_OUTPUTS)) {
    dir.create(POSTERIOR_OUTPUTS)
}

NSDUH_MISSING_CODES <- c(85, 89, 94, 97, 98, 99)

base_palette <- c(line = '#426EBD', 
                  fill = '#F7FBFF')

analogous_palette <- c(blue = '#426EBD', 
                       purple = "#6942bd", 
                       green = "#42bd92")

PRIOR_CONFIG <- c(
    brms::set_prior(
        'normal(0, 2)',
        class = 'b'
    ), 
    brms::set_prior(
        'lognormal(-.5, .5)',
        class = 'sd'
    ), 
    brms::set_prior(
        'lkj(2)',
        class = 'cor'
    )
)