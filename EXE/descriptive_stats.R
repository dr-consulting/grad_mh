grads_model_base %>% 
    group_by(academic_year) %>% 
    summarize(mean_age = mean(Q46_age, na.rm = TRUE), 
              sd_age = sd(Q46_age, na.rm = TRUE)) %>% as.data.frame()

grads_model_base %>% 
    select(Q46_age, Q47_gender, race_ethn, Q55_international, Q52_enrollment) %>% 
    psych::describe() %>% 
    select(n) %>% 
    mutate(perc_miss = (1 - n / nrow(grads_model_base)) * 100)

grads_model_base %>% 
    group_by(academic_year) %>% 
    summarize(m_any_distress = mean(neg_emo_any, na.rm = TRUE) * 100) %>% 
    as.data.frame()

grads_model_base %>% 
    filter(neg_emo_any == 1) %>% 
    summarize(count = n(), 
              m_distress_cnt = mean(neg_emo_cnt, na.rm = TRUE), 
              sd_distress_cnt = sd(neg_emo_cnt, na.rm = TRUE))

grads_model_base %>% 
    select(all_of(neg_vars)) %>% 
    psych::describe() %>% 
    select(n) %>% 
    mutate(perc_miss = (1 - n / nrow(grads_model_base)) * 100)

grads_model_base %>% 
    group_by(academic_year) %>% 
    summarize(m_any_distress = mean(ovrwhlm_any, na.rm = TRUE) * 100) %>% 
    as.data.frame()

grads_model_base %>% 
    filter(neg_emo_any == 1) %>% 
    summarize(count = n(), 
              m_distress_cnt = mean(neg_emo_cnt, na.rm = TRUE), 
              sd_distress_cnt = sd(neg_emo_cnt, na.rm = TRUE))

grads_model_base %>% 
    select(all_of(ovrwhlm_vars)) %>% 
    psych::describe() %>% 
    select(n) %>% 
    mutate(perc_miss = (1 - n / nrow(grads_model_base)) * 100)

haven::write_sav(grads_model_base, path = "~/github/ATNL/grad_mh/data/ACHA-II/grad_students_modeling_2021-02-04_v2.sav")

