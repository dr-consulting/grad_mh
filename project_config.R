# File containing basic configurations and settings for project

LOCAL_REPO <- '~/github/ATNL/grad_mh'
POSTERIOR_OUTPUTS <- '/media/dr-owner/HDD1/ATNL_grad_mh_Posteriors'

# Create a directory at the location above if one does not already exist
if(!dir.exists(POSTERIOR_OUTPUTS)) {
    dir.create(POSTERIOR_OUTPUTS)
}

