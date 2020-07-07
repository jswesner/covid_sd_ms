library(tidyverse)
library(brms)
library(janitor)
library(here)
library(ggridges)

#past datasets
# cum_hosp_state70 <- cum_hosp_state %>% filter(date_num <= 70)
cum_hosp_state90 <- cum_hosp_state %>% filter(date_num <= 90)
cum_hosp_state80 <- cum_hosp_state %>% filter(date_num <= 80)
cum_hosp_state60 <- cum_hosp_state %>% filter(date_num <= 60)
cum_hosp_state40 <- cum_hosp_state %>% filter(date_num <= 40)
cum_hosp_state20 <- cum_hosp_state %>% filter(date_num <= 20)

#re-run model
fit_weib_state <- readRDS(file = here("outputs/fit_weib_state.rds"))
# fit_weib_state <- update(fit_weib_statelast, new_data = cum_hosp_state)
fit_weib_state90 <- update(fit_weib_state, newdata = cum_hosp_state90)
fit_weib_state80 <- update(fit_weib_state, newdata = cum_hosp_state80)
fit_weib_state60 <- update(fit_weib_state, newdata = cum_hosp_state60)
fit_weib_state40 <- update(fit_weib_state, newdata = cum_hosp_state40)
fit_weib_state20 <- update(fit_weib_state, newdata = cum_hosp_state20)

#extract posts
posts_state_today <- posterior_samples(fit_weib_state) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = max(cum_hosp_state$date_num), method = "same_prior")
posts_state90 <- posterior_samples(fit_weib_state90) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 90, method = "same_prior")
posts_state80 <- posterior_samples(fit_weib_state80) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 80, method = "same_prior")
posts_state60 <- posterior_samples(fit_weib_state60) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 60, method = "same_prior")
posts_state40 <- posterior_samples(fit_weib_state40) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 40, method = "same_prior")
posts_state20 <- posterior_samples(fit_weib_state20) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 20, method = "same_prior")

#combine posts
posts_all <- bind_rows(posts_state_today,
                       posts_state20,
                       posts_state40,
                       posts_state60,
                       posts_state80,
                       posts_state90)


posts_all_g <- posts_all %>% select(-contains(c("r_group", "lp"))) %>% 
  gather(key, value, c(-day, -iter,-method)) %>% 
  mutate(parameter = case_when(grepl("ult", key) ~ "max",
                               grepl("theta", key) ~ "slope",
                               grepl("omega", key) ~ "inflection"),
         prior_post = case_when(grepl("prior", key) ~ "prior", TRUE ~ "posterior"),
         model = "Model 1",
         day = case_when(prior_post == "prior" ~ 0, TRUE ~ day),
         group = "South Dakota") %>% 
  filter(iter <= 1000) 






# Model 2 updating --------------------------------------------------------


#past datasets
# cum_hosp_state70 <- cum_hosp_state %>% filter(date_num <= 70)
cum_hosp_group90 <- cum_hosp %>% filter(date_num <= 90)
cum_hosp_group80 <- cum_hosp %>% filter(date_num <= 80)
cum_hosp_group60 <- cum_hosp %>% filter(date_num <= 60)
cum_hosp_group40 <- cum_hosp %>% filter(date_num <= 40)
cum_hosp_group20 <- cum_hosp %>% filter(date_num <= 20)

#re-run model
fit_weib_group <- readRDS(file = here("outputs/fit_weib_group_f.rds"))
# fit_weib_group <- update(fit_weib_grouplast, new_data = cum_hosp_group)
fit_weib_group90 <- update(fit_weib_group, newdata = cum_hosp_group90)
fit_weib_group80 <- update(fit_weib_group, newdata = cum_hosp_group80, control = list(adapt_delta = 0.95))
fit_weib_group60 <- update(fit_weib_group, newdata = cum_hosp_group60)
fit_weib_group40 <- update(fit_weib_group, newdata = cum_hosp_group40)
fit_weib_group20 <- update(fit_weib_group, newdata = cum_hosp_group20)

#extract posts
posts_group_today <- posterior_samples(fit_weib_group) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = max(cum_hosp$date_num), method = "same_prior")
posts_group90 <- posterior_samples(fit_weib_group80) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 90, method = "same_prior")
posts_group80 <- posterior_samples(fit_weib_group80) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 80, method = "same_prior")
posts_group60 <- posterior_samples(fit_weib_group60) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 60, method = "same_prior")
posts_group40 <- posterior_samples(fit_weib_group40) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 40, method = "same_prior")
posts_group20 <- posterior_samples(fit_weib_group20) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 20, method = "same_prior")

#combine posts
posts_all_group <- bind_rows(posts_group_today,
                       posts_group20,
                       posts_group40,
                       posts_group60,
                       posts_group80,
                       posts_group90) %>% as_tibble() %>% 
  mutate(b_ult_rest = b_ult_intercept + b_ult_group_restof_south_dakota,
         b_omega_rest = b_omega_intercept + b_omega_group_restof_south_dakota,
         b_theta_rest = b_theta_intercept + b_theta_group_restof_south_dakota)


posts_all_group_g <- posts_all_group %>% select(-contains(c("group", "lp", "prior"))) %>% 
  gather(key, value, c(-day, -iter,-method)) %>% 
  mutate(parameter = case_when(grepl("ult", key) ~ "max",
                               grepl("theta", key) ~ "slope",
                               grepl("omega", key) ~ "inflection"),
         prior_post = case_when(grepl("prior", key) ~ "prior", TRUE ~ "posterior"),
         model = "Model 2",
         day = case_when(prior_post == "prior" ~ 0, TRUE ~ day),
         group = case_when(grepl("rest", key) ~ "Rest of South Dakota", TRUE ~ "Minnehaha County")) %>% 
  filter(iter <= 1000) 



posts_everything <- bind_rows(posts_all_g, posts_all_group_g) %>% tibble()
saveRDS(posts_everything, file = here("outputs/posts_everything.rds"))


posts_all_group_g %>% filter(day == max(day)) %>% 
  distinct(prior_post)


#prior post comparison
posts_everything %>% filter(day == max(day)) %>% 
  mutate(group = case_when(grepl("rest", key) ~ "Minnehaha County",
                           model == "Model 2" ~ "Oustide of Minnehaha",
                                 TRUE ~ "South Dakota")) %>% 
  ggplot(aes(x = value, y = parameter, fill = prior_post)) + 
  geom_density_ridges() + 
  facet_grid(. ~ group, scales = "free") +
  scale_fill_grey() + 
  theme_bw()

