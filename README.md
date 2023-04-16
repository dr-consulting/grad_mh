# Graduate student mental health

This repository contains the code supporting DeYoung et al.'s manuscript comparing trends in U.S. graduate student mental health levels with those of the general U.S. adult population and a sample of education-matched adults. 

Some of the data used in these analyses are publicly available. Specifically, the comparison samples were created from the ongoing National Survey on Drug Use and Health available from the U.S. government's [Substance Abuse & Mental Health Data Archive](https://www.datafiles.samhsa.gov/data-sources) (SAMHDA).  

Some of the data are not publicly available and need to be requested from the data producers. Specifically our sample of graduate students was created from the [National College Health Assessment](https://www.acha.org/NCHA/About_ACHA_NCHA/Survey/NCHA/About/Survey.aspx?hkey=7e9f6752-2b47-4671-8ce7-ba7a529c9934) - conducted twice a year by the American College Health Association.

## Data setup

As noted above, some of the data used are publicly available and some are not. For colleagues working on this project or extensions, please reach out to the research team directly. 

## Modeling setup

The following instructions assume access to a Linux machine. Most of the steps will probably work inside a MacOS terminal (with some variations - if you say use the homebrew package manager). I am unsure of the equivalents in a Windows setting (Git Bash may be an option that allows some of this to run but I am skeptical it works out of the box).

1. Make sure you have various compilers installed (instructions assume Ubuntu-like Linux system)

```bash
$ sudo apt install gfortran
$ sudo apt install build-essential
$ sudo apt install libx11-dev
$ sudo apt install libfontconfig1-dev
$ sudo apt install libharfbuzz-dev
$ sudo apt install libfribidi-dev
$ sudo apt install cargo
```

1. Check to see if you have all packages required to run the code in the project repository. Start by opening the `grad_mh/project_config.R` file.

1. After opening the project_config.R file, update the `LOCAL_REPO` variable to point to the correct file location. Making manual edits to point to a specific location is the most common type of edit you will have to make in this project's codebase. It is annoyingly repetitive, but it isn't too complicated at least.

1. Once you have made the required edit, attempt to run the `project_config.R` file. You can execute it from a terminal using the command below.

```bash
$ Rscript path/to/grad_mh/project_config.R
```

(Assumes that your command line is configured to execute `.R` files). 

... or alternatively you can open the file in an `R` session and run the code directly. However you execute the code, the goal here is to have all packages required for the codebase to properly install. Note that this workflow has only been tested on a Linux machine (Ubuntu 22.04) and it is possible that different compilers or dependencies are required when installing a compatible environment in another operating system.

## Re-creating a model

First, make sure that you have the raw data stored in an accessible location on your computer. Contact the primary research team to discuss data access as we are not in a position to publish all data used. 

Once you have the raw data start by executing the data processing pipeline. The "pipeline" is effectively 3 `R` scripts designed to be run in sequence. The scripts load the raw data obtained from the organizations that originally collected and collated it. The scripts produce several date-versioned clean data files that are used by the modeling scripts. 

After all the data required have been produced, navigate to the `grad_mh/EXE` folder that contains the model you want to re-train. There are three main model training folders - one for ACHA-NHCA (graduate student sample) models that include school- and respondent-level covariates, one for the ACHA-NCHA without covariates (primary analysis), and one for the NSDUH U.S. adults and matched peers samples. 
```
| - grad_mh/
    | - EXE/
    | - NCHA_no_covariates/
        | - my_model_script.R
        | - my_other_model_script.R
```

Open the file you want to fit and change the `BASE_FILE` variable [at the top to point](https://github.com/dr-consulting/grad_mh/blob/e5d3b725a72d5985e3f2febf1741d0f285273c95/EXE/NCHA_no_covariates/ncha_anxiety_dx_no_cov.R#L4) to the correct location for the local `project_config.R` file. 

The model scripts are designed to be initiated from a terminal. So you can do the following once the data is in place and the file paths have been updated in the `project_config.R` as well as the model `.R` file of interest...

```bash
$ Rscript path/to/grad_mh/EXE/NCHA_no_covariates/my_model_script.R
```

Executing the command should result in fitting the model and storing its outputs (according to the filepaths in the `project_config.R`). Errors are most likely driven by an incompatible environment (missing packages), mismatched filepaths, or lack of data files. 

## Fitting a model fast

These models can take a long time to fit given the sample sizes involved. This scripts were designed to be executed on a single multi-core machine - the default settings require 18 cores (3 chains with computations hyper-threaded on 6 cores each). The computer used had 32 cores available, which is uncommon for most users. You can prevent this additional parallelization (at the cost of runtime) if it is not well-suited for your computer. 

To prevent chain hyperthreading, set `chain_hyperthreading=FALSE` in the model fit wrapper function in the model `.R`. Alternatively, you can lower the number of cores used to distribute within-chain computations by setting the parameter `max_threads=<something lower that 6>`. So if you had 12 cores and were okay with the model fitting process using all 12 you could provide `max_threads=4`. The difference on my Linux home workstation between using 3 cores (1 core for each chain + single-threaded computations) and 18 cores (1 core for each chain + 6 cores per chain for parallel computation) is the difference between 7.5 hours and 1.5 hours. (If you are unsure how many cores you have access to run `parallel::detectCores()` in an `R` terminal).
