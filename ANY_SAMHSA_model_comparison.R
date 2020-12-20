
create_model_list <- function(filepath, base_name) {
    require(glue)
    require(magrittr)
    model_list <- list()
    load(filepath)
    model_list[['{base_name}_fit_base' %>% glue()]] <- fit_base
    model_list[['{base_name}_fit_2_plus' %>% glue()]] <- fit_1_plus
    model_list[['{base_name}_fit_5_plus' %>% glue()]] <- fit_5_plus
    return(model_list)
}


linear_int <- create_model_list("~/count_test/linear_fixed_ouput.RData", 'linear_int')
linear_int_slp <- create_model_list("~/count_test/linear_rand_ouput.RData", 'linear_int_slp')
quad_int <- create_model_list("~/count_test/quadratic_fixed_ouput.RData", 'quad_int')
quad_int_slp <- create_model_list("~/count_test/quadratic_rand_ouput.RData", 'quad_int_slp')


# may come back and cleanup later - but quickly here we have a simple model comparison
linear_int[['linear_int_fit_base']] <- linear_int[['linear_int_fit_base']] %>% 
    brms::add_criterion('waic')

linear_int_slp[['linear_int_slp_fit_base']] <- linear_int_slp[['linear_int_slp_fit_base']] %>% 
    brms::add_criterion('waic')

quad_int[['quad_int_fit_base']] <- quad_int[['quad_int_fit_base']] %>% 
    brms::add_criterion('waic')

quad_int_slp[['quad_int_slp_fit_base']] <- quad_int_slp[['quad_int_slp_fit_base']] %>% 
    brms::add_criterion('waic')

brms::loo_compare(linear_int_slp[['linear_int_slp_fit_base']], 
                  linear_int[['linear_int_fit_base']], 
                  quad_int[['quad_int_fit_base']], 
                  quad_int_slp[['quad_int_slp_fit_base']],
                  criterion = 'waic')

# -- 
linear_int[['linear_int_fit_2_plus']] <- linear_int[['linear_int_fit_2_plus']] %>% 
    brms::add_criterion('waic')

linear_int_slp[['linear_int_slp_fit_2_plus']] <- linear_int_slp[['linear_int_slp_fit_2_plus']] %>% 
    brms::add_criterion('waic')

quad_int[['quad_int_fit_2_plus']] <- quad_int[['quad_int_fit_2_plus']] %>% 
    brms::add_criterion('waic')

quad_int_slp[['quad_int_slp_fit_2_plus']] <- quad_int_slp[['quad_int_slp_fit_2_plus']] %>% 
    brms::add_criterion('waic')

brms::loo_compare(linear_int_slp[['linear_int_slp_fit_2_plus']], 
                  linear_int[['linear_int_fit_2_plus']], 
                  quad_int[['quad_int_fit_2_plus']], 
                  quad_int_slp[['quad_int_slp_fit_2_plus']],
                  criterion = 'waic')

# -- 
linear_int[['linear_int_fit_5_plus']] <- linear_int[['linear_int_fit_5_plus']] %>% 
    brms::add_criterion('waic')

linear_int_slp[['linear_int_slp_fit_5_plus']] <- linear_int_slp[['linear_int_slp_fit_5_plus']] %>% 
    brms::add_criterion('waic')

quad_int[['quad_int_fit_5_plus']] <- quad_int[['quad_int_fit_5_plus']] %>% 
    brms::add_criterion('waic')

quad_int_slp[['quad_int_slp_fit_5_plus']] <- quad_int_slp[['quad_int_slp_fit_5_plus']] %>% 
    brms::add_criterion('waic')

brms::loo_compare(linear_int_slp[['linear_int_slp_fit_5_plus']], 
                  linear_int[['linear_int_fit_5_plus']], 
                  quad_int[['quad_int_fit_5_plus']], 
                  quad_int_slp[['quad_int_slp_fit_5_plus']],
                  criterion = 'waic')

# Basic conclusion is that the quadratic model with a random linear slope bu
summary(quad_int$quad_int_fit_5_plus) %>% print(digits=5)
summary(quad_int_slp$quad_int_slp_fit_5_plus) %>% print(digits=5)

# model convergence for the quadratic with random slopes was an issue
# would likely need more iterations or better priors