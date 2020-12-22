# File containing basic configurations and settings for project
library(glue)
library(magrittr)

LOCAL_REPO <- '~/github/ATNL/grad_mh'
DATA_DIR <- paste0(LOCAL_REPO, '/', 'data')
R_DIR <- paste0(LOCAL_REPO, '/', 'R')
MAPS_DIR <- paste0(LOCAL_REPO, '/', 'data_maps')
POSTERIOR_OUTPUTS <- '/media/dr-owner/HDD1/ATNL_grad_mh_Posteriors'

# Create a directory at the location above if one does not already exist
if(!dir.exists(POSTERIOR_OUTPUTS)) {
    dir.create(POSTERIOR_OUTPUTS)
}

NSDUH_MISSING_CODES <- c(85, 89, 94, 97, 98, 99)

base_palette <- c(line = '#426EBD', 
                  fill = '#F7FBFF')
