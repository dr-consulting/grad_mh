# Graduate student mental health

This repository contains the code supporting DeYoung et al.'s manuscript comparing trends in U.S. graduate student mental health levels with those of the general U.S. adult population and a sample of education-matched adults. 

Some of the data used in these analyses are publicly available. Specifically, the comparison samples were created from the ongoing National Survey on Drug Use and Health available from the U.S. government's [Substance Abuse & Mental Health Data Archive](https://www.datafiles.samhsa.gov/data-sources) (SAMHDA).  

Some of the data are not publicly available and need to be requested from the data producers. Specifically our sample of graduate students was created from the [National College Health Assessment](https://www.acha.org/NCHA/About_ACHA_NCHA/Survey/NCHA/About/Survey.aspx?hkey=7e9f6752-2b47-4671-8ce7-ba7a529c9934) - conducted twice a year by the American College Health Association.

## Data setup

As noted above, some of the data used are publicly available and some are not. For colleagues working on this project or extensions, please reach out to the research team directly. 

## Modeling setup

1. Clone the repository 
1. Using conda - you can create a starter environment. Note that this environment file does not contain all necessary packages needed to run the model code, but it does contain many of them.

```bash
$ conda env create --file=grad_mh/environment.yml --name=my_env
```

1. Then activate your `conda` environment

```bash
$ conda activate my_env
```

1. Then, assuming you have downloaded the data and updated the necessary base file locations in the project files `.R` files (see below), you can begin training models and storing their outputs. 

**Note.** The `project_config.R` file [does attempt to install](https://github.com/dr-consulting/grad_mh/blob/e5d3b725a72d5985e3f2febf1741d0f285273c95/project_config.R#L7-L17) the core packages needed if they are not present in the environment. Performance is not stable across operating system (specifically MacOS users with M1 chips will need to perform custom setup to create a compatible environment). 

## Re-creating a model

First, make sure that you have the data stored in an accesible location on your computer. Then navigate to the `grad_mh/EXE` folder that contains the model you want to re-train. There are three main model training folders - one for ACHA-NHCA (graduate student sample) models that include school- and respondent-level covariates, one for the ACHA-NCHA without covariates (primary analysis), and one for the NSDUH U.S. adults and matched peers samples. 

```
| - grad_mh/
    | - EXE/
    | - NCHA_no_covariates/
        | - my_model_script.R
        | - my_other_model_script.R
```

Open the file you want to fit and change the `BASE_FILE` variable [at the top to point](https://github.com/dr-consulting/grad_mh/blob/e5d3b725a72d5985e3f2febf1741d0f285273c95/EXE/NCHA_no_covariates/ncha_anxiety_dx_no_cov.R#L4) to the correct location for the local `project_config.R` file. 

Note that you will also likely have to make [some changes](https://github.com/dr-consulting/grad_mh/blob/e5d3b725a72d5985e3f2febf1741d0f285273c95/project_config.R#L23) to the `project_config.R` file as well to get things working as expected.

The model scripts are designed to be initiated from a terminal. So you can do the following once the data is in place and the file paths have been udpated in the `project_config.R` as well as the model `.R` file of interest...

```bash
$ Rscript path/to/grad_mh/EXE/NCHA_no_covariates/my_model_script.R
```

Executing the command should result in fitting the model and storing its outputs (according to the filepaths in the `project_config.R`). Errors are most likely driven by an imcompatible environment (missing packages), mismatched filepaths, or lack of data files. 
