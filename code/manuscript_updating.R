library(tidyverse)
library(brms)
library(janitor)
library(here)
library(ggridges)

#past datasets
cum_hosp_state <- cum_hosp %>% ungroup() %>% group_by(date, date_original) %>% summarize(cum_hosp = sum(cum_hosp)) %>% 
  mutate(date_num = date_original)

# cum_hosp_state70 <- cum_hosp_state %>% filter(date_num <= 70)
cum_hosp_state120 <- cum_hosp_state %>% filter(date_num <= 120)
cum_hosp_state100 <- cum_hosp_state %>% filter(date_num <= 100)
cum_hosp_state80 <- cum_hosp_state %>% filter(date_num <= 80)
cum_hosp_state60 <- cum_hosp_state %>% filter(date_num <= 60)
cum_hosp_state40 <- cum_hosp_state %>% filter(date_num <= 40)
cum_hosp_state20 <- cum_hosp_state %>% filter(date_num <= 20)

#re-run model
fit_weib_state <- readRDS(file = here("outputs/fit_weib_state.rds"))
# fit_weib_state <- update(fit_weib_statelast, new_data = cum_hosp_state)
fit_weib_state120 <- update(fit_weib_state, newdata = cum_hosp_state120)
fit_weib_state100 <- update(fit_weib_state, newdata = cum_hosp_state100)
fit_weib_state80 <- update(fit_weib_state, newdata = cum_hosp_state80)
fit_weib_state60 <- update(fit_weib_state, newdata = cum_hosp_state60)
fit_weib_state40 <- update(fit_weib_state, newdata = cum_hosp_state40)
fit_weib_state20 <- update(fit_weib_state, newdata = cum_hosp_state20)

#extract posts
# posts_state_today <- posterior_samples(fit_weib_state) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = max(cum_hosp_state$date_num), method = "same_prior")
posts_state120 <- posterior_samples(fit_weib_state120) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 120, method = "same_prior")
posts_state100 <- posterior_samples(fit_weib_state100) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 100, method = "same_prior")
posts_state80 <- posterior_samples(fit_weib_state80) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 80, method = "same_prior")
posts_state60 <- posterior_samples(fit_weib_state60) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 60, method = "same_prior")
posts_state40 <- posterior_samples(fit_weib_state40) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 40, method = "same_prior")
posts_state20 <- posterior_samples(fit_weib_state20) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 20, method = "same_prior")


#combine posts
posts_all <- bind_rows(posts_state120,
                       posts_state20,
                       posts_state40,
                       posts_state60,
                       posts_state80,
                       posts_state100)


posts_all_g <- posts_all %>% select(-contains(c("r_group", "lp"))) %>% 
  gather(key, value, c(-day, -iter,-method)) %>% 
  mutate(parameter = case_when(grepl("ult", key) ~ "alpha",
                               grepl("theta", key) ~ "beta",
                               grepl("omega", key) ~ "gamma"),
         prior_post = case_when(grepl("prior", key) ~ "prior", TRUE ~ "posterior"),
         model = "Model 1",
         day = case_when(prior_post == "prior" ~ 0, TRUE ~ day),
         group = "South Dakota") %>% 
  filter(iter <= 1000) 






# Model 2 updating --------------------------------------------------------


#past datasets
cum_hosp <- read_csv("data/cum_hosp.csv", na = c("", ".")) %>% 
  mutate(date = mdy(date_hosp),
         date_numeric  = as.numeric(date),
         date_original = date_numeric - min(date_numeric),
         group_short = str_sub(group,1,1)) %>% filter(cum_hosp > 0) %>% 
  mutate(date_correct = case_when(grepl("innehah", group) ~ 12, TRUE ~ 0),
         date_num = date_original - date_correct)

# cum_hosp_state70 <- cum_hosp_state %>% filter(date_num <= 70)
cum_hosp_group120 <- cum_hosp %>% filter(date_num <= 120)
cum_hosp_group100 <- cum_hosp %>% filter(date_num <= 100)
cum_hosp_group80 <- cum_hosp %>% filter(date_num <= 80)
cum_hosp_group60 <- cum_hosp %>% filter(date_num <= 60)
cum_hosp_group40 <- cum_hosp %>% filter(date_num <= 40)
cum_hosp_group20 <- cum_hosp %>% filter(date_num <= 20)

#re-run model
fit_weib_group <- readRDS(file = here("outputs/fit_weib_group_f.rds"))
# fit_weib_group <- update(fit_weib_grouplast, new_data = cum_hosp_group)
fit_weib_group120 <- update(fit_weib_group, newdata = cum_hosp_group120)
fit_weib_group100 <- update(fit_weib_group, newdata = cum_hosp_group100)
fit_weib_group80 <- update(fit_weib_group, newdata = cum_hosp_group80, control = list(adapt_delta = 0.95))
fit_weib_group60 <- update(fit_weib_group, newdata = cum_hosp_group60)
fit_weib_group40 <- update(fit_weib_group, newdata = cum_hosp_group40)
fit_weib_group20 <- update(fit_weib_group, newdata = cum_hosp_group20)

#extract posts
# posts_group_today <- posterior_samples(fit_weib_group) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = max(cum_hosp$date_num), method = "same_prior")
posts_group120 <- posterior_samples(fit_weib_group120) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 120, method = "same_prior")
posts_group100 <- posterior_samples(fit_weib_group100) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 100, method = "same_prior")
posts_group80 <- posterior_samples(fit_weib_group80) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 80, method = "same_prior")
posts_group60 <- posterior_samples(fit_weib_group60) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 60, method = "same_prior")
posts_group40 <- posterior_samples(fit_weib_group40) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 40, method = "same_prior")
posts_group20 <- posterior_samples(fit_weib_group20) %>% clean_names() %>% mutate(iter = 1:nrow(.), day = 20, method = "same_prior")

#combine posts
posts_all_group <- bind_rows(posts_group120,
                       posts_group20,
                       posts_group40,
                       posts_group60,
                       posts_group80,
                       posts_group100) %>% as_tibble() %>% 
  mutate(b_ult_rest = b_ult_intercept + b_ult_group_restof_south_dakota,
         b_omega_rest = b_omega_intercept + b_omega_group_restof_south_dakota,
         b_theta_rest = b_theta_intercept + b_theta_group_restof_south_dakota)



posts_all_group_g <- posts_all_group %>% select(-contains(c("group", "lp"))) %>% 
  gather(key, value, c(-day, -iter,-method)) %>% 
  mutate(parameter = case_when(grepl("ult", key) ~ "alpha",
                               grepl("theta", key) ~ "beta",
                               grepl("omega", key) ~ "gamma"),
         prior_post = case_when(grepl("prior", key) ~ "prior", TRUE ~ "posterior"),
         model = "Model 2",
         day = case_when(prior_post == "prior" ~ 0, TRUE ~ day),
         group = case_when(grepl("rest", key) ~ "Rest of South Dakota", TRUE ~ "Minnehaha County")) %>% 
  filter(iter <= 1000) 



posts_everything <- bind_rows(posts_all_g, posts_all_group_g) %>% tibble()
saveRDS(posts_everything, file = here("outputs/posts_everything.rds"))





posts_state <- posts_all %>% mutate(model = "Model 1") %>% 
  expand_grid(date_num = seq(0, 175, by = 1)) %>% 
  mutate(y = exp(b_ult_intercept*(1 - exp(-(date_num/b_omega_intercept)^b_theta_intercept))),
         value = rpois(nrow(.),y)) %>% 
  mutate(group = "All of South Dakota") %>% 
  select(iter, group, value, model, date_num, day)


posts_group <- posts_all_group %>% mutate(model = "Model 2") %>% 
  expand_grid(date_num = seq(0, 175, by = 1)) %>% 
  mutate(yminn = exp(b_ult_intercept*(1 - exp(-(date_num/b_omega_intercept)^b_theta_intercept))),
         yrest = exp((b_ult_intercept + b_ult_group_restof_south_dakota)*(1 - exp(-(date_num/(b_omega_intercept + 
                                                                                                b_omega_group_restof_south_dakota))^(b_theta_intercept + b_theta_group_restof_south_dakota)))),
         yminn_pred = rpois(nrow(.), yminn),
         yrest_pred = rpois(nrow(.), yrest)) %>% 
  select(iter, yminn_pred, yrest_pred, date_num, model, day) %>% 
  gather(group, value, c(yminn_pred, yrest_pred))


posts_bind <- bind_rows(posts_state, posts_group) %>% 
  group_by(group, model, date_num, day) %>% 
  summarize(median = median(value),
            upper = quantile(value, probs = 0.975),
            lower = quantile(value, probs = 0.025)) %>% 
  ungroup() %>% 
  mutate(group = case_when(grepl("minn", group) ~ "Minnehaha County",
                           grepl("rest", group) ~ "Outside of Minnehaha",
                           TRUE ~ group),
         date = case_when(group == "Minnehaha County" ~ as_date(date_num + 18329 + 12),
                          TRUE ~ as_date(date_num + 18329)))



data_a <- cum_hosp %>% 
  select(date, date_num, group, cum_hosp, date_original) %>% 
  mutate(model = "Model 2",
         group = case_when(grepl("Rest", group) ~ "Outside of Minnehaha", 
                           TRUE ~ group))

data_b <- plot_data_state <- cum_hosp_state %>% 
  mutate( group = "All of South Dakota",
          model = "Model 1")

data_ab <- bind_rows(plot_data_group, plot_data_state) %>% 
  expand_grid(day = c(20, 40, 60, 80, 100, 120)) %>% 
  mutate(data = case_when(date_num <= day ~ "fit",
                           TRUE ~ "future"))



pred_overtime <- posts_bind %>% 
  ggplot(aes(x = date, y = median)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  facet_grid(day ~ group) +
  coord_cartesian(ylim = c(0, 2000)) +
  geom_point(data = data_ab, aes(y = cum_hosp, fill = data, size = data), shape = 21, color = "black") +
  scale_size_manual(values = c(1.5, 0.5)) +
  scale_fill_brewer(type = "qual", palette = 5) +
  theme_bw() +
  labs(y = "Cumulative Hospitalizations",
       x = "")

saveRDS(pred_overtime, file = here::here("plots/pred_overtime.rds"))
ggsave(pred_overtime, file = here::here("plots/pred_overtime.jpg"), dpi = 500, width = 8, height = 10)



