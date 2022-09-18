# File containing basic configurations and settings for project
library(glue)
library(magrittr)

LOCAL_REPO <- '~/Desktop/grad_mh'
DATA_DIR <- paste0(LOCAL_REPO, '/', 'data')
R_DIR <- paste0(LOCAL_REPO, '/', 'R')
MAPS_DIR <- paste0(LOCAL_REPO, '/', 'data_maps')
POSTERIOR_OUTPUTS <- paste0(DATA_DIR, "/", "OUTPUTS")
PLOT_OUTPUT <- paste0(LOCAL_REPO, '/outputs/figures')
SUMMARY_OUTPUT <- paste0(LOCAL_REPO, '/outputs/tables')

# Create a directory at the location above if one does not already exist
for(fldr in c(POSTERIOR_OUTPUTS, PLOT_OUTPUT, SUMMARY_OUTPUT)) {
    if(!dir.exists(fldr)) {
        dir.create(fldr, recursive = TRUE)
    }
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

POIS_PRIOR_CONFIG <- c(
    brms::set_prior(
        'normal(0, 2)',
        class = 'b'
    ), 
    brms::set_prior(
        'lkj(2)',
        class = 'cor'
    )
)

PERC_PLOT_CONFIG <- list(
    'Anxiety' = list(
        y_breaks=seq(0, 50, by=10), 
        y_labels=paste0(seq(0, 50, by=10), "%"), 
        y_limits=c(0, 51)
    ),
    'Any Psychiatric Disorder' = list(
        y_breaks=seq(0, 60, by=10), 
        y_labels=paste0(seq(0, 60, by=10), "%"), 
        y_limits=c(0,61)
    ),
    'Bipolar Disorder' = list(
        y_breaks=seq(0, 5, by=1), 
        y_labels=paste0(seq(0, 5, by=1), "%"), 
        y_limits=c(0,5.1)
    ),
    'Depression' = list(
        y_breaks=seq(0, 40, by=10), 
        y_labels=paste0(seq(0, 40, by=10), "%"), 
        y_limits=c(0, 41)
    ), 
    'Panic Attacks' = list(
        y_breaks=seq(0, 25, by=5), 
        y_labels=paste0(seq(0, 25, by=5), "%"), 
        y_limits=c(0, 26)
    ), 
    'Schizophrenia' = list(
        y_breaks=seq(0, 2.5, by=.5), 
        y_labels=paste0(seq(0, 2.5, by=.5), "%"), 
        y_limits=c(0, 2.6)
    ), 
    'Suicidal Thoughts' = list(
        y_breaks=seq(0, 15, by=5), 
        y_labels=paste0(seq(0, 15, by=5), "%"), 
        y_limits=c(0, 16)
    ), 
    'Suicide Attempt' = list(
        y_breaks=seq(0, 5, by=1), 
        y_labels=paste0(seq(0, 5, by=1), "%"), 
        y_limits=c(0, 5.1)
    ), 
    'Emotional Distress' = list(
        y_breaks=seq(0, 75, by=25), 
        y_labels=paste0(seq(0, 75, by=25), "%"), 
        y_limits=c(0, 76)
    ),
    'Overwhelmed or Exhausted' = list(
        y_breaks=seq(0, 100, by=25), 
        y_labels=paste0(seq(0, 100, by=25), "%"), 
        y_limits=c(0, 101)
    ), 
    'Poor Health' = list(
        y_breaks=seq(0, 30, by=10), 
        y_labels=paste0(seq(0, 30, by=10), "%"), 
        y_limits=c(0, 31)
    ), 
    'Too Depressed to Function' = list(
        y_breaks=seq(0, 50, by=10), 
        y_labels=paste0(seq(0, 50, by=10), "%"), 
        y_limits=c(0, 51)
    ), 
    'Overwhelming Anxiety' = list(
        y_breaks=seq(0, 50, by=10), 
        y_labels=paste0(seq(0, 50, by=10), "%"), 
        y_limits=c(0, 51)
    ), 
    'Lonely' = list(
        y_breaks=seq(0, 50, by=10), 
        y_labels=paste0(seq(0, 50, by=10), "%"), 
        y_limits=c(0, 51)
    ), 
    'Hopeless' = list(
        y_breaks=seq(0, 50, by=10), 
        y_labels=paste0(seq(0, 50, by=10), "%"), 
        y_limits=c(0, 51)
    )
)

NSDUH_PLOT_CONFIG <- list(
    'Any Psychiatric Disorder' = list(
        y_breaks=seq(0, 40, by=10), 
        y_labels=paste0(seq(0, 40, by=10), "%"), 
        y_limits=c(0,42)
    ),
    'Suicidal Thoughts' = list(
        y_breaks=seq(0, 10, by=2.5), 
        y_labels=paste0(seq(0, 10, by=2.5), "%"), 
        y_limits=c(0, 10.5)
    ), 
    'Suicide Attempt' = list(
        y_breaks=seq(0, 1, by=.25), 
        y_labels=paste0(seq(0, 1, by=.25), "%"), 
        y_limits=c(0, 1.05)
    ), 
    'Poor Health' = list(
        y_breaks=seq(0, 20, by=5), 
        y_labels=paste0(seq(0, 20, by=5), "%"), 
        y_limits=c(0,21)
    ), 
    'Matched Any Psychiatric Disorder' = list(
        y_breaks=seq(0, 40, by=10), 
        y_labels=paste0(seq(0, 40, by=10), "%"), 
        y_limits=c(0,42)
    ),
    'Matched Suicidal Thoughts' = list(
        y_breaks=seq(0, 10, by=2.5), 
        y_labels=paste0(seq(0, 10, by=2.5), "%"), 
        y_limits=c(0, 10.5)
    ), 
    'Matched Suicide Attempt' = list(
        y_breaks=seq(0, 1, by=.25), 
        y_labels=paste0(seq(0, 1, by=.25), "%"), 
        y_limits=c(0, 1.05)
    ), 
    'Matched Poor Health' = list(
        y_breaks=seq(0, 20, by=5), 
        y_labels=paste0(seq(0, 20, by=5), "%"), 
        y_limits=c(0,21)
    )
)
