# Toy Data

Because the authors of the manuscript are not the primary owners of the ACHA data used in these analyses, we have created a simple toy model example that relies on mocked data to demonstrate the core functioning of our code. The `toy_model.R` file in this directory is modeled after the individual scripts for each target analyses present in our top-level `EXE` folder.

## Setup

Follow instructions on the main `README.md` in this repository. It is impossible to fully guarantee code compatbility across systems and environments. This repository is designed to attempt to install a compatible set of versions in your local environment. It has been extensively tested and developed using Macintosh and Linux (Ubuntu specifically) operating systems.

The `toy_model.R` file contained within was executed within the following local environment: 

```
R version 4.3.3 (2024-02-29)
Platform: aarch64-apple-darwin22.6.0 (64-bit)
Running under: macOS Ventura 13.3.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /opt/homebrew/Cellar/r/4.3.3/lib/R/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/New_York
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] simr_1.0.7         lme4_1.1-35.2      Matrix_1.6-5       tidybayes_3.0.6   
 [5] modelr_0.1.11      corrr_0.4.4        lubridate_1.9.3    forcats_1.0.0     
 [9] stringr_1.5.1      dplyr_1.1.4        purrr_1.0.2        readr_2.1.5       
[13] tidyr_1.3.1        tibble_3.2.1       tidyverse_2.0.0    magrittr_2.0.3    
[17] glue_1.7.0         cmdstanr_0.7.1     Cairo_1.6-2        lavaan_0.6-17     
[21] tidygraph_1.3.1    nFactors_2.4.1.1   lattice_0.22-5     tufte_0.13        
[25] GGally_2.2.1       ggraph_2.2.1       ggplot2_3.5.0      brms_2.21.0       
[29] Rcpp_1.0.12        shinystan_2.6.0    shiny_1.8.1.1      rstan_2.32.6      
[33] StanHeaders_2.32.6

loaded via a namespace (and not attached):
  [1] svUnit_1.0.6         shinythemes_1.2.0    splines_4.3.3        later_1.3.2         
  [5] polyclip_1.10-6      xts_0.13.2           lifecycle_1.0.4      processx_3.8.4      
  [9] vroom_1.6.5          MASS_7.3-60.0.1      crosstalk_1.2.1      ggdist_3.3.2        
 [13] backports_1.4.1      plotrix_3.8-4        httpuv_1.6.15        pkgbuild_1.4.4      
 [17] minqa_1.2.6          RColorBrewer_1.1-3   abind_1.4-5          quadprog_1.5-8      
 [21] tensorA_0.36.2.1     tweenr_2.0.3         ggrepel_0.9.5        inline_0.3.19       
 [25] pbkrtest_0.5.2       testthat_3.2.1       bridgesampling_1.1-2 codetools_0.2-19    
 [29] DT_0.33              ggforce_0.4.2        tidyselect_1.2.1     bayesplot_1.11.1    
 [33] farver_2.1.1         viridis_0.6.5        matrixStats_1.2.0    stats4_4.3.3        
 [37] base64enc_0.1-3      jsonlite_1.8.8       iterators_1.0.14     emmeans_1.10.1      
 [41] tools_4.3.3          binom_1.1-1.1        mnormt_2.1.1         gridExtra_2.3       
 [45] xfun_0.43            mgcv_1.9-1           distributional_0.4.0 loo_2.7.0           
 [49] withr_3.0.0          fastmap_1.1.1        boot_1.3-29          fansi_1.0.6         
 [53] shinyjs_2.1.0        digest_0.6.35        timechange_0.3.0     R6_2.5.1            
 [57] mime_0.12            estimability_1.5     colorspace_2.1-0     gtools_3.9.5        
 [61] markdown_1.12        threejs_0.3.3        utf8_1.2.4           generics_0.1.3      
 [65] data.table_1.15.4    graphlayouts_1.1.1   htmlwidgets_1.6.4    ggstats_0.6.0       
 [69] pkgconfig_2.0.3      dygraphs_1.1.1.6     gtable_0.3.4         brio_1.1.4          
 [73] htmltools_0.5.8.1    carData_3.0-5        scales_1.3.0         posterior_1.5.0     
 [77] knitr_1.46           rstudioapi_0.16.0    tzdb_0.4.0           reshape2_1.4.4      
 [81] coda_0.19-4.1        checkmate_2.3.1      nlme_3.1-164         curl_5.2.1          
 [85] nloptr_2.0.3         cachem_1.0.8         zoo_1.8-12           RLRsim_3.1-8        
 [89] parallel_4.3.3       miniUI_0.1.1.1       pillar_1.9.0         grid_4.3.3          
 [93] vctrs_0.6.5          promises_1.3.0       car_3.1-2            arrayhelpers_1.1-0  
 [97] xtable_1.8-4         waldo_0.5.2          pbivnorm_0.6.0       mvtnorm_1.2-4       
[101] cli_3.6.2            compiler_4.3.3       rlang_1.1.3          crayon_1.5.2        
[105] rstantools_2.4.0     ps_1.7.6             plyr_1.8.9           stringi_1.8.3       
[109] psych_2.4.3          viridisLite_0.4.2    QuickJSR_1.1.3       munsell_0.5.1       
[113] colourpicker_1.3.0   Brobdingnag_1.2-9    V8_4.4.2             hms_1.1.3           
[117] bit64_4.0.5          igraph_2.0.3         broom_1.0.5          memoise_2.0.1       
[121] RcppParallel_5.1.7   bit_4.0.5
```

If script execution fails to create a viable environment, the information above can be used to manually create one. This code base has never been executed on a Windows operating system, and we have no expectations of compatibility. 

Installation time can vary depending on your local set up. If you do not have many of the required compilers or other packages sourced and built on the machine in question, expect installation through running the `toy_model.R` file to take several hours. The runtime for the code will also likely be 2-3 hours, again depending on your compute resources.

## Execution

The `toy_model.R` file is designed as a standalone analysis file/artifact. With the exception of generating simulated data for the purposes of demonstrating functionality, the `toy_model.R` file mirrors the core analysis files found in the main `EXE/` folder. You can execute the file in a terminal via `$ Rscript path/to/toy_model.R`. You can also open the file in Rstudio or an R session and execute it - which may allow for a little more interaction with the inputs and outputs.

### Inputs

The expected input for the core modeling code is a `data.frame` with the required features and grouping variable(s), a formula defining the model, along with settings and hyperameters that are defined in a global `project_config.R` file at the main level of the repo. 

### Outputs

An `.RData` file stored in `data/OUTPUTS` in the repository. The file contains a single `list` object called `results_list` which holds the input data (`results_list$model_data`), the model formula (`results_list$brms_formula`), the priors (`results_list$brms_priors`), the run_time (`results_list$run_time`), and the fitted model object(`results_list$brms_fit`). 