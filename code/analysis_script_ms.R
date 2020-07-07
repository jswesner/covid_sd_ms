## ----Load packages, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------
library(tidyverse)
library(scales)
library(ggridges)
library(here)
library(RcppRoll)
library(janitor)
library(lubridate)
library(viridis)
library(readr)
library(readxl)
library(ggrepel)
library(cowplot)
library(brms)
library(kableExtra)
library(ggpubr)

## ----Load hospitalization data, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----
cum_hosp <- read_csv("data/cum_hosp.csv", na = c("", ".")) %>% 
  mutate(date = mdy(date_hosp),
         date_numeric  = as.numeric(date),
         date_original = date_numeric - min(date_numeric),
         group_short = str_sub(group,1,1)) %>% filter(cum_hosp > 0) %>% 
  mutate(date_correct = case_when(grepl("innehah", group) ~ 12, TRUE ~ 0),
         date_num = date_original - date_correct)

cum_hosp_state <- cum_hosp %>% ungroup() %>% group_by(date, date_original) %>% summarize(cum_hosp = sum(cum_hosp)) %>% 
  mutate(date_num = date_original)

nyc_hosp <- read_csv(here::here("data/nyc_hosp.csv")) %>% clean_names() %>% 
  mutate(date = mdy(date_of_interest),
         date_num = as.numeric(date) - min(as.numeric(date)),
         cum_hosp = cumsum(hospitalizations),
         nyc_pop = 8300000,
         prop_hosp = cum_hosp/nyc_pop,
         model = "Model 1",
         group = "All of South Dakota",
         pop_correct = 884000/8300000,
         cum_hosp_corrected = as.integer(cum_hosp*pop_correct),
         model = "NYC")


## ----Plot NYC data, echo=FALSE, fig.cap="Left: Model of New York City's hospitalization curve. Data are divided by 10 to reflect the relative population sizes in South Dakota versus New York city. Three-hundred simulations of cumulative hospitalizations from the prior predictive distribution of each model. Priors for Model 1 were derived from the fit of NYC's hospitalization curve. Priors for Model 2 were similar to those of Model 1, but had a reduced prediction of cumulative hospitalizations to account for the smaller populations of each group (Minnehaha County vs Outside Minnehaha County) relative to the whole state population.", fig.height=4, fig.width=12, message=FALSE, warning=FALSE, paged.print=FALSE----
#data from https://www1.nyc.gov/site/doh/covid/covid-19-data.page

# gamma_parms <- function(mean, sd) {
#   out <- tibble(shape = mean^2/sd^2,
#                 rate = mean/sd^2,
#                 scale = 1/(mean/sd^2))
#   return(out)
#   return(plot)
# }
# 
# gamma_parms(mean = 8, sd = 1)
# gamma_parms(mean = 17, sd = 10)
# gamma_parms(mean = 1.2, sd = .5)
# # 
# tibble(ult = rgamma(300, 25, 3.3),
#                 omega = rgamma(300, 4, 0.2),
#                 theta = rgamma(300, 4, 2)
# nyc_model <- brm(bf(cum_hosp_corrected ~ ult*(1 - exp(-(date_num/omega)^theta)),
#                   ult ~ 1 , omega ~ 1 , theta ~ 1 ,
#                   nl = TRUE ),
#                data = nyc_hosp,
#                family = poisson(link = "log"),
#                prior = c(
#                  prior(gamma(1.2, 0.1), nlpar = "ult"), #asymptote - based on proportion total hosp in NY
#                  prior(gamma(0.25, 0.005), nlpar = "omega"), #time (days since start) of inflection
#                  prior(gamma(1.4, 0.3), nlpar = "theta")), # slope at inflection. Gamma to ensure it is positive (has to be since data are cumulative) assumes slope of exp(2) with sd of 0.5
#                iter = 1000, chains = 1)
# saveRDS(nyc_model, file = "outputs/nyc_model.rds")

nyc_model <- readRDS(file = "outputs/nyc_model.rds")
nyc_pred <- predict(nyc_model) %>% as_tibble() %>% clean_names() %>% mutate(date_num = nyc_hosp$date_num,
                                                                            model = "NYC")

nyc_plot <- nyc_pred %>% 
  ggplot(aes(x = date_num, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = q2_5, ymax = q97_5), alpha = 0.5) +
  geom_point(data = nyc_hosp, aes(y = cum_hosp_corrected), size = 0.5) +
  scale_y_log10() +
  coord_cartesian(ylim = c(1, 100000)) +
  theme_classic() +
  facet_grid(~model) +
  labs(subtitle = "NYC data (scaled to SD's population)",
       y = "NYC Cumulative Hospitalizations*0.1") +
  NULL

state_priors <- tibble(ult = rgamma(300, 64, 8),
                omega = rgamma(300, 2.9, .17),
                theta = rgamma(300, 5.8, 4.8),
                iter = 1:300) %>% 
  expand_grid(date_num = seq(1, 150, by = 1)) %>% 
  mutate(fit = exp(ult*(1 - exp(-(date_num/omega)^theta))),
         pred = rpois(nrow(.), fit),
         group = "All of South Dakota",
         model = "Model 1")


group_priors <- tibble(ult = rgamma(300, 64, 8),
                       omega = rgamma(300, 2.9, .17),
                       theta = rgamma(300, 5.8, 4.8),
                       ult_r = rnorm(300, 0, 1),
                       omega_r = rnorm(300, 0, 5),
                       theta_r = rnorm(300, 0, 0.5),
                       iter = 1:300) %>% 
  expand_grid(date_num = 1:150) %>% 
  mutate(y_minn = exp(ult*(1 - exp(-(date_num/omega)^theta))),
         y_rest = exp((ult + ult_r)*(1 - exp(-(date_num/(omega + omega_r))^(theta + theta_r)))),
         y_minn_pred = rpois(nrow(.), y_minn),
         y_rest_pred = rpois(nrow(.), y_rest)) %>% 
  gather(key, pred, c(y_minn_pred:y_rest_pred)) %>% 
  mutate(group = case_when(grepl("minn", key) ~ "Minnehaha County",
                           TRUE ~ "Outside of Minnehaha"),
         model = "Model 2")


plot_priors <- state_priors %>% select(pred, iter, group, date_num,model) %>% 
  bind_rows(group_priors %>% select(pred, group, iter, date_num, model))


pop_correct = 884000/8400000

prior_predictive <- plot_priors %>% 
  ggplot(aes(x = date_num, y = pred, color = group)) +
  geom_line(aes(group = interaction(group,iter)), alpha = 0.2) +
  # geom_point(data = nyc_hosp, aes(y = cum_hosp*pop_correct), color = "black", shape = 21) +
  scale_y_log10() +
  facet_wrap(~ model) +
  labs(y = expression(paste("SD Cumulative Hospitalizations")),
       x = "Days since first hospitalization",
       subtitle = "Prior predictive simulations") +
  theme_classic() + 
  coord_cartesian(ylim = c(1, 100000)) +
  scale_color_brewer(type = "qual", palette = 8) +
  # scale_color_manual(values = c("black", "red", "blue")) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.title = element_blank()) +
  NULL




prior_plots <- plot_grid(nyc_plot, prior_predictive, ncol = 2, rel_widths = c(0.33, 0.67), align = "h")
saveRDS(prior_plots, file = here("plots/prior_plots.rds"))
ggsave(prior_plots, file = here("plots/prior_plots.jpg", width = 8, height = 4))


## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE-----------------------------
nyc_fixef <- fixef(nyc_model)
set.seed(202)

nycpostprior <- tibble(param = c("alpha", "beta", "gamma", "alpha_rest", "beta_rest", "gamma_rest"),
       mean_nyc_posterior = c(nyc_fixef[1], nyc_fixef[2], nyc_fixef[3], NA, NA, NA),
       sd_nyc_posterior = c(nyc_fixef[4], nyc_fixef[5], nyc_fixef[6], NA, NA, NA),
       mean_m1_prior = c(mean(rgamma(1000, 64, 8)), mean(rgamma(1000, 2.9, 0.17)), mean(rgamma(1000, 5.8, 4.8)), NA, NA, NA),
       sd_m1_prior = c(sd(rgamma(1000, 64, 8)), sd(rgamma(1000, 2.9, 0.17)), sd(rgamma(1000, 5.8, 4.8)), NA, NA, NA),
       mean_m2_prior = c(mean(rgamma(1000, 49,7)), mean(rgamma(1000, 2.9, 0.17)), mean(rgamma(1000, 5.8, 4.8)),
                         0, 0, 0),
       sd_m2_prior = c(sd(rgamma(1000, 49,7)), sd(rgamma(1000, 2.9, 0.17)), sd(rgamma(1000, 5.8, 4.8)),
                       1, 5, 0.5))
nycpostprior_table <- nycpostprior %>% gather(key, value, -param) %>% 
  separate(key, c("measure", "model", "post_prior")) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(shape = mean^2/sd^2,
         rate = mean/sd^2) %>% 
  mutate_if(is.numeric,round, 2) %>% 
  arrange(desc(param)) %>% 
  drop_na() %>% 
  mutate(shape = na_if(shape, 0),
         rate = na_if(rate, 0))

saveRDS(nycpostprior_table, file = here("plots/nycpostprior_table.rds"))
# kable(nycpostprior_table, "latex", caption = "Posterior distributions from the New York City model and the prior distributions for the South Dakota models.", booktabs = T) %>%
# kable_styling(latex_options = c("striped", "hold_position"))


## ----Fit state model, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------
set.seed(202)

# fit_weib_state <- brm(bf(cum_hosp ~ ult*(1 - exp(-(date_num/omega)^theta)),
#    ult ~ 1 , omega ~ 1 , theta ~ 1 ,
#    nl = TRUE ),
# data = cum_hosp_state,
# family = poisson(link = "log"),
# prior = c(
#   prior(gamma(64, 8), nlpar = "ult"), #asymptote - based on proportion total hosp in NY
#   prior(gamma(2.9, 0.17), nlpar = "omega"), #time (days since start) of inflection
#   prior(gamma(5.8, 4.8), nlpar = "theta")), # slope at inflection. Gamma to ensure it is positive (has to be since data are cumulative) assumes slope of exp(2) with sd of 0.5
# iter = 2000, chains = 4, sample_prior = "yes")
# saveRDS(fit_weib_state, file = "outputs/fit_weib_state.rds")

fit_weib_state <- update(readRDS(here::here("outputs/fit_weib_state.rds")), newdata = cum_hosp_state)
saveRDS(fit_weib_state, file = here::here(paste0("outputs/fit_weib_state_", Sys.Date(), ".rds")))


## ----Fit group model, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------
# get_prior(bf(cum_hosp ~ (ult*date_num^omega)/(theta^omega + date_num^omega),
#                         ult ~ 1 + group, omega ~ 1 + group, theta ~ 1 + group,
#                         nl = TRUE ),
#                      data = cum_hosp,
#                      family = poisson(link = "log"))
# 

# fit_weib_group_f <-  brm(bf(cum_hosp ~ ult*(1 - exp(-(date_num/omega)^theta)),
#                         ult ~ 1 + group, omega ~ 1 + group, theta ~ 1 + group,
#                         nl = TRUE ),
#                      data = cum_hosp,
#                      family = poisson(link = "log"),
#                      prior = c(
#                        prior(gamma(49,7), nlpar = "ult", lb = 0), #asymptote - based on proportion total hosp in NY
#                        prior(gamma(2.9, 0.17), nlpar = "omega", lb = 0), #time (days since start) of inflection
#                        prior(gamma(5.8, 4.8), nlpar = "theta", lb = 0),
#                        prior(normal(0,1), coef = "groupRestofSouthDakota", nlpar = "theta"),
#                        prior(normal(0,5), coef = "groupRestofSouthDakota", nlpar = "omega"),
#                        prior(normal(0,0.5), coef = "groupRestofSouthDakota", nlpar = "ult")),
#                      iter = 2000, chains = 4, sample_prior = T)
# 
# saveRDS(fit_weib_group_f, file = here::here("outputs/fit_weib_group_f.rds"))

fit_weib_group_f <- update(readRDS(here::here("outputs/fit_weib_group_f.rds")), newdata = cum_hosp)

saveRDS(fit_weib_group_f, file = here::here("outputs/fit_weib_group_f.rds"))
#update models
# fit_weib_group <- update(readRDS(here::here("outputs/fit_weib_group.rds")), newdata = cum_hosp)


## ----extract posteriors, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE---------

#new dates to condition on
newstatefit <- tibble(date_num = seq(0, max(cum_hosp$date_num, by = 1)))
newstatepred <- tibble(date_num = seq(max(cum_hosp$date_num, 200, by = 1)))

#extract posteriors distributions
#model 1
fit_state <- fitted(fit_weib_state, newdata = newstatefit, probs = c(0.025, 0.25, 0.75, 0.975)) %>% as_tibble() %>% clean_names() %>% 
  mutate(date_num = newstatefit$date_num,
         group = "All of South Dakota",
         prior_post = "Posterior",
         method = "Fitted", 
         model = "Model 1")

pred_state <- predict(fit_weib_state, newdata = newstatepred, probs = c(0.025, 0.25, 0.75, 0.975)) %>% as_tibble() %>% clean_names() %>% 
  mutate(date_num = newstatepred$date_num,
         group = "All of South Dakota",
         prior_post = "Posterior",
         method = "Predicted", 
         model = "Model 1")


#model 2
newminnfit <- tibble(date_num = seq(0, max(cum_hosp$date_num, by = 1)),
                  group = "Minnehaha County")

fit_minn <- fitted(fit_weib_group_f, newdata = newminnfit, probs = c(0.025, 0.25, 0.75, 0.975)) %>% as_tibble() %>% clean_names() %>% 
  mutate(date_num = newminnfit$date_num,
         group = "Minnehaha County",
         prior_post = "Posterior",
         method = "Fitted", 
         model = "Model 2")


newminnpred <- tibble(date_num = seq(max(cum_hosp$date_num), 200, by = 1),
                                    group = "Minnehaha County")

pred_minn <- predict(fit_weib_group_f, newdata = newminnpred, probs = c(0.025, 0.25, 0.75, 0.975)) %>% as_tibble() %>% clean_names() %>% 
  mutate(date_num = newminnpred$date_num,
         group = "Minnehaha County",
         prior_post = "Posterior",
         method = "Predicted", 
         model = "Model 2")


newrestfit <- tibble(date_num = seq(0, max(cum_hosp$date_num, by = 1)),
                  group = "Rest of South Dakota")

fit_rest <- fitted(fit_weib_group_f, newdata = newrestfit, probs = c(0.025, 0.25, 0.75, 0.975)) %>% as_tibble() %>% clean_names() %>% 
  mutate(date_num = newrestfit$date_num,
         group = "Oustide of Minnehaha",
         prior_post = "Posterior",
         method = "Fitted", 
         model = "Model 2")

newrestpred <- tibble(date_num = seq(max(cum_hosp$date_num), 200, by = 1),
                                    group = "Rest of South Dakota")

pred_rest <- predict(fit_weib_group_f, newdata = newrestpred, probs = c(0.025, 0.25, 0.75, 0.975)) %>% as_tibble() %>% clean_names() %>% 
  mutate(date_num = newrestpred$date_num,
         group = "Oustide of Minnehaha",
         prior_post = "Posterior",
         method = "Predicted", 
         model = "Model 2")

posts <- bind_rows(fit_state, pred_state, fit_minn, pred_minn, fit_rest, pred_rest) %>% 
  mutate(date_num = case_when(grepl("County", group) ~ date_num + 12, TRUE ~ date_num)) %>% 
  mutate(date = as_date(18329 + date_num))


## ----extract priors, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE-------------
#new dates to condition on

#extract prior distributions
#model 1
state_priors <- prior_samples(fit_weib_state) %>% as_tibble() %>% 
  expand_grid(date_num = seq(0, 200, by = 1)) %>% 
  mutate(y_fit_state = exp(b_ult*(1 - exp(-(date_num/b_omega)^b_theta))),
         y_pred_state = rpois(nrow(.), y_fit_state)) %>% 
  gather(key, value, c(y_fit_state:y_pred_state)) %>% 
group_by(date_num, key) %>% 
  summarize(estimate = median(value),
            est_error = sd(value),
            q2_5 = quantile(value, probs = 0.025),
            q25 = quantile(value, probs = 0.25),
            q75 = quantile(value, probs = 0.75),
            q97_5 = quantile(value, probs = 0.975)) %>% 
  mutate(group = "All of South Dakota",
         prior_post = "Prior",
         method = case_when(grepl("fit", key) ~ "Fitted", TRUE ~ "Predicted"),
         model = "Model 1",
         date = as_date(date_num + 18329))

#model 2
group_priors <- prior_samples(fit_weib_group_f) %>% as_tibble() %>% clean_names() %>% 
  expand_grid(date_num = seq(0, 200, by = 1)) %>% 
  mutate(y_fit_minn = exp(b_ult_intercept*(1 - exp(-(date_num/b_omega_intercept)^b_theta_intercept))),
         ult_gr = b_ult_intercept + b_ult_group_restof_south_dakota,
         omega_gr = b_omega_intercept + b_omega_group_restof_south_dakota,
         theta_gr = b_theta_intercept + b_theta_group_restof_south_dakota,
         y_fit_rest = exp(ult_gr*(1 - exp(-(date_num/omega_gr)^theta_gr))),
         y_pred_minn = rpois(nrow(.), y_fit_minn),
         y_pred_rest = rpois(nrow(.), y_fit_rest)) %>% 
  select(-ult_gr, -omega_gr, -theta_gr) %>% 
  gather(key, value, c(y_fit_minn:y_pred_rest)) %>% 
group_by(date_num, key) %>% 
  summarize(estimate = median(value),
            est_error = sd(value),
            q2_5 = quantile(value, probs = 0.025),
            q25 = quantile(value, probs = 0.25),
            q75 = quantile(value, probs = 0.75),
            q97_5 = quantile(value, probs = 0.975)) %>% 
  mutate(group = case_when(grepl("rest", key) ~ "Oustide of Minnehaha", TRUE ~ "Minnehaha County"),
         prior_post = "Prior",
         method = case_when(grepl("fit", key) ~ "Fitted", TRUE ~ "Predicted"),
         model = "Model 2",
         date = as_date(date_num + 18329))


priors <- bind_rows(group_priors, state_priors) %>% filter(method == "Predicted")


## ----Plot posteriors and priors, echo=FALSE, fig.cap="Posterior distributions of cumulative hospitalizations in South Dakota. Lines indicate medians and shading indicates the 50 and 90% intervals. Predictions beyond the data represent samples from the posterior predictive distribution. Predictions within the data represent samples from the posterior fitted distribution.", fig.height=6, fig.width=6, message=FALSE, warning=FALSE, paged.print=FALSE----

#combine post and prior preds
post_priors <- bind_rows(posts, priors)


#format raw data to plot
plot_data_group <- cum_hosp %>% 
  select(date, date_num, group, cum_hosp, date_original) %>% 
  mutate(prior_post = case_when(date_num >=0 ~ "Posterior", TRUE ~ "Prior"),
         prior_post = fct_relevel(prior_post, "Prior"),
         model = "Model 2") 

plot_data_state <- cum_hosp_state %>% 
  mutate(prior_post = case_when(date_num >=0 ~ "Posterior", TRUE ~ "Prior"),
         prior_post = fct_relevel(prior_post, "Prior"),
         group = "All of South Dakota",
         model = "Model 1")

plot_data_all <- bind_rows(plot_data_group, plot_data_state)

#modify post predictions so that they are fitted within the data and predictions beyond the data
post_all_plot <- post_priors  %>% 
  filter(prior_post != "Prior") %>% 
  ggplot(aes(x = date, y = estimate)) +
  geom_line(aes(color = group)) +
  geom_ribbon(alpha = 0.2, aes(ymin = q2_5, ymax = q97_5, fill = group)) +
  geom_ribbon(alpha = 0.2, aes(ymin = q25, ymax = q75, fill = group)) + 
  facet_grid(model ~ ., scales = "free") +
  geom_point(data = plot_data_all, aes(x = date, y = cum_hosp, shape = group), size = 0.8) +
  coord_cartesian(ylim = c(1, 800),
                  xlim = c(min(cum_hosp$date), as_date("2020-08-01"))) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_bw() +
  # scale_y_log10() +
  labs(y = "Cumulative Hospitalizations",
       x = "") +
  guides(shape = F) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank()) +
  NULL

saveRDS(post_all_plot, file = here::here("plots/post_all_plot.rds"))
ggsave(post_all_plot, file = here::here("plots/post_all_plot.jpg"), dpi = 500, width = 10, height = 10)

## ----tables, echo=FALSE----------------------------------------------------------------------
poststate <- posterior_samples(fit_weib_state) %>% tibble() %>% clean_names()
postgroup <- posterior_samples(fit_weib_group_f) %>% tibble() %>% clean_names()


state_table <- poststate %>% tibble() %>% mutate(iter = 1:nrow(.)) %>% 
  mutate(sd_max = exp(b_ult_intercept),
         sd_slope = exp(b_theta_intercept),
         sd_inf = b_omega_intercept) %>%
  select(iter, sd_max, sd_inf, sd_slope) %>% 
  gather(key, value, -iter) %>% 
  group_by(key) %>% 
  summarize(median = round(median(value),1),
            mean = round(mean(value),1),
            sd = round(sd(value),1),
            low90 = round(quantile(value, probs = 0.05),1),
            upper90 = round(quantile(value, probs = 0.95),1)) %>% 
  ungroup() %>% 
  filter(grepl("sd_", key) & !grepl("r_group", key)) %>% 
  mutate(Model = "Model 1",
         Group = case_when(grepl("minn", key) ~ "Minnehaha", TRUE ~ "South Dakota"),
         Parameter = str_sub(key, 4)) %>% 
  select(Model, Group, Parameter, everything(),-key)



group_table <- postgroup %>% tibble() %>% mutate(iter = 1:nrow(.)) %>% 
  mutate(minn_max = exp(b_ult_intercept),
         minn_inf = b_omega_intercept,
         minn_slope = exp(b_theta_intercept),
         rest_max = exp(b_ult_intercept + b_ult_group_restof_south_dakota),
         rest_slope = exp(b_theta_intercept + b_theta_group_restof_south_dakota),
         rest_inf = b_omega_intercept + b_omega_group_restof_south_dakota) %>%
  select(minn_max, minn_inf, minn_slope, rest_max, rest_inf, rest_slope, iter) %>% 
  gather(key, value, -iter) %>% 
  group_by(key) %>% 
  summarize(median = round(median(value),1),
            mean = round(mean(value),1),
            sd = round(sd(value),1),
            low90 = round(quantile(value, probs = 0.05),1),
            upper90 = round(quantile(value, probs = 0.95),1)) %>% 
  ungroup() %>% 
  filter(grepl("minn_", key) | grepl("rest_", key) & !grepl("r_group", key)) %>% 
  mutate(Model = "Model 2",
         Group = case_when(grepl("minn", key) ~ "Minnehaha", TRUE ~ "Outside of Minnehaha"),
         Parameter = str_sub(key, 6)) %>% 
  select(Model, Group, Parameter, everything(),-key)


table_all <- bind_rows(group_table, state_table) %>% mutate(Model = fct_relevel(Model, "Model 2"))
saveRDS(table_all, file = "plots/table_all.rds")

# kable(table_all, "latex", caption = "Summary statistics of the asymptote (max), inflection point in days since the first case (inf), and slope at inflection (slope). Values for max and slope are exponentiated to place them on the scale of the response variable (cumulative hospitalizations. Summaries are derived from the posterior distributions of each parameter in the corresponding model.", booktabs = T) %>%
# kable_styling(latex_options = c("striped", "hold_position"))



## ----echo=FALSE, fig.cap="Change in parameter values over time. Violins represent posterior distributions of parameter values (untransformed) over time as models are fit using data at day 0, 20, 40, 60, 80, 100, and the most recent date.", fig.height=5, fig.width=8, message=FALSE, warning=FALSE, paged.print=FALSE----

posts_methods <- readRDS(file = here("outputs/posts_everything.rds")) %>% as_tibble() %>% 
  mutate(value_trans = case_when(parameter == "slope" ~ value, TRUE ~ exp(value)),
         group = case_when(grepl("rest", key) ~ "Outside of Minnehaha",
                           model == "Model 1" ~ "South Dakota",
                                 TRUE ~ "Minnehaha County"))

param_time_plot <- posts_methods %>% 
  mutate(day = as.factor(day)) %>% 
  ggplot(aes(x = day, y = value)) +
  # geom_density(trim = T, aes(alpha = day)) +
  geom_violin(position = position_dodge(width = 0.1), alpha = 0.5, fill = "black") +
  # geom_boxplot(position = position_dodge(width = 5), alpha = 0.5, outlier.shape = NA) +
  facet_grid(parameter ~ group, scales = "free") +
  scale_fill_brewer(type = "qual") +
  # labs(y = "Parameter value") +
  # scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
  theme_bw() +
  labs(y = "Parameter value",
       x = "Days after first hospitalization") +
  guides(alpha = F) +
  theme(legend.title = element_blank()) +
  NULL

saveRDS(param_time_plot, file = here("plots/param_time_plot.rds"))
ggsave(param_time_plot, file = here("plots/param_time_plot.jpg"), dpi = 500, width = 8, height = 10)

## ----echo=FALSE, fig.cap="Posterior predictive distributions of active hospitalizations in South Dakota. Lines indicate medians and shading indicates the 50 and 95% prediction intervals", fig.height=10, fig.width=10, message=FALSE, warning=FALSE, paged.print=FALSE----
#active hospitalizations

#model_1
#predictions up to 200 days after the first hospitalization
newdata = tibble(date_num = 0:200)

sd_covid_data <- read.csv(here::here("data/data_kelo.csv")) %>%
  clean_names() %>%
  mutate(date_num = as.numeric(mdy(date)) - 18329,
         date = mdy(date),
         source = "SD_DOH via Keloland news",
         incidence = positive_cases - lag(positive_cases),
         cum_hosp = parse_number(as.character(hospitalized)))

#make predictions of daily hospital needs
predict_dailyhosp <- fitted(fit_weib_state, newdata = newdata, summary = F)  %>%
  as_tibble() %>%
  mutate(iter = 1:nrow(.)) %>%
  filter(iter <= 500) %>% 
  gather(key, value, -iter) %>%
  mutate(date_num = 18328 + parse_number(key),
         date = as_date(date_num)) %>%
  arrange(iter, date_num) %>%
  group_by(iter) %>%
  mutate(inc = as.numeric(value - lag(value, 1))) %>% 
  mutate(daily_total_5 = inc + lag(inc, 1) + lag(inc, 2) + lag(inc, 3) + lag(inc, 4),
         daily_total_10 = inc + lag(inc, 1) + lag(inc, 2) + lag(inc, 3) + lag(inc, 4) + lag(inc, 5) +
            + lag(inc, 6) + lag(inc, 7) + lag(inc, 8) + lag(inc, 9),
         daily_total_12 = inc + lag(inc, 1) + lag(inc, 2) + lag(inc, 3) + lag(inc, 4) + lag(inc, 5) +
            + lag(inc, 6) + lag(inc, 7) + lag(inc, 8) + lag(inc, 9) + lag(inc, 10) + lag(inc, 11),
         daily_total_15 = inc + lag(inc, 1) + lag(inc, 2) + lag(inc, 3) + lag(inc, 4) + lag(inc, 5) +
            + lag(inc, 6) + lag(inc, 7) + lag(inc, 8) + lag(inc, 9) + lag(inc, 10) + lag(inc, 11) + lag(inc, 12) + lag(inc, 13) + lag(inc, 14)) %>% 
  gather(hosp_guess, daily_total, c("daily_total_5":"daily_total_15")) %>% 
  ungroup() %>% 
  mutate(daily_total = rpois(nrow(.), daily_total)) %>%
  replace_na(list(daily_total = 0)) %>%
  group_by(date, hosp_guess) %>% 
  summarize(median = median(daily_total),
            mean = mean(daily_total),
            sd = sd(daily_total),
            high90 = quantile(daily_total, probs = 0.975),
            high50 = quantile(daily_total, probs = 0.75),
            low90 = quantile(daily_total, probs = 0.025),
            low50 = quantile(daily_total, probs = 0.25)) %>% 
  ungroup() %>% 
  mutate(hosp_guess = case_when(hosp_guess == "daily_total_5" ~ "5 days in hospital",
                                hosp_guess == "daily_total_10" ~ "10 days in hospital",
                                hosp_guess == "daily_total_12" ~ "12 days in hospital",
                                TRUE ~ "15 days in hospital"),
         hosp_guess = fct_relevel(hosp_guess, "5 days in hospital"),
         model = "Model 1", 
         group = "South Dakota")


active_plot <- predict_dailyhosp %>% 
ggplot() +
  geom_line(aes(x = date, y = median), color = "dodgerblue") +
  geom_ribbon(aes(x = date, ymin = low90, ymax = high90), fill = 'dodgerblue', alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = low50, ymax = high50), fill = 'dodgerblue', alpha = 0.2) +
  geom_point(data = sd_covid_data, aes(x = date, y = hospitalized_currently),
             shape = 21, fill = "yellow") +
  # scale_x_date(date_breaks = "months" , date_labels = "%b") +
  theme_classic() +
  facet_grid(.~hosp_guess) +
  # geom_hline(yintercept = all_beds, color = "dodgerblue") +
  # annotate("text", x = as.Date("2020-04-10"), y = 2960, label = "All Hospital Beds\nin SD") +
  labs(y = "Active hospitalizations",
       title = "Predicting active hospitalizations from Weibull model",
       subtitle = "Shading = 50% and 90% prediction intervals") +
  # coord_cartesian(ylim = c(0,100)) +
  NULL

# active_plot - model 2
postgroup_preds <- posterior_samples(fit_weib_group_f) %>% as_tibble() %>% clean_names() %>% 
  mutate(iter = 1:nrow(.),
         ult_rest = b_ult_intercept + b_ult_group_restof_south_dakota,
         omega_rest = b_omega_intercept + b_omega_group_restof_south_dakota,
         theta_rest = b_theta_intercept + b_theta_group_restof_south_dakota) %>% 
  expand_grid(date_num = as.numeric(0:250)) %>% 
  mutate(fit_minn = exp(b_ult_intercept*(1 - exp(-(date_num/b_omega_intercept)^b_theta_intercept))),
         fit_rest = exp(ult_rest*(1 - exp(-(date_num/omega_rest)^theta_rest))),
         fit_state = fit_minn + fit_rest) %>% 
  gather(key, value, c(fit_minn:fit_state)) %>% 
  select(key, iter, value, date_num) %>% 
  mutate(date_num = case_when(grepl("inn", key) ~ date_num + 12, TRUE ~ date_num))


predict_dailyhosp_m2 <- postgroup_preds %>% 
  filter(iter <= 500,
         key != "fit_state") %>% 
  mutate(date = as_date(date_num + 18329)) %>%
  group_by(iter, key) %>% 
  mutate(inc = value - lag(value, 1)) %>% 
  mutate(daily_total_5 = inc + lag(inc, 1) + lag(inc, 2) + lag(inc, 3) + lag(inc, 4),
         daily_total_10 = inc + lag(inc, 1) + lag(inc, 2) + lag(inc, 3) + lag(inc, 4) + lag(inc, 5) +
           + lag(inc, 6) + lag(inc, 7) + lag(inc, 8) + lag(inc, 9),
         daily_total_12 = inc + lag(inc, 1) + lag(inc, 2) + lag(inc, 3) + lag(inc, 4) + lag(inc, 5) +
           + lag(inc, 6) + lag(inc, 7) + lag(inc, 8) + lag(inc, 9) + lag(inc, 10) + lag(inc, 11),
         daily_total_15 = inc + lag(inc, 1) + lag(inc, 2) + lag(inc, 3) + lag(inc, 4) + lag(inc, 5) +
           + lag(inc, 6) + lag(inc, 7) + lag(inc, 8) + lag(inc, 9) + lag(inc, 10) + lag(inc, 11) + lag(inc, 12) + lag(inc, 13) + lag(inc, 14)) %>% 
  gather(hosp_guess, daily_total, c("daily_total_5":"daily_total_15")) %>%
  ungroup() %>% 
  mutate(daily_total = rpois(nrow(.), daily_total))

predhospm2_total <- predict_dailyhosp_m2 %>% ungroup() %>% group_by(date, iter, hosp_guess) %>% 
  summarize(daily_total = sum(daily_total, na.rm = T)) %>% 
  mutate(key = "South Dakota",
         model = "Model 2")

d <- bind_rows(predict_dailyhosp_m2, predhospm2_total)

d2 <- d %>%   
  replace_na(list(daily_total = 0)) %>% 
group_by(date, hosp_guess, key) %>% 
  summarize(median = median(daily_total, na.rm = T),
            mean = mean(daily_total, na.rm = T),
            sd = sd(daily_total, na.rm = T),
            high90 = quantile(daily_total, probs = 0.975, na.rm = T),
            high50 = quantile(daily_total, probs = 0.75, na.rm = T),
            low90 = quantile(daily_total, probs = 0.025, na.rm = T),
            low50 = quantile(daily_total, probs = 0.25, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(hosp_guess = case_when(hosp_guess == "daily_total_5" ~ "5 days in hospital",
                                hosp_guess == "daily_total_10" ~ "10 days in hospital",
                                hosp_guess == "daily_total_12" ~ "12 days in hospital",
                                TRUE ~ "15 days in hospital"),
         hosp_guess = fct_relevel(hosp_guess, "5 days in hospital")) %>% 
  mutate(group = case_when(grepl("minn", key) ~ "Minnehaha County",
                           grepl("rest", key) ~ "Outside of Minnehaha",
                           TRUE ~ "South Dakota"),
         model = "Model 2")


daily_hosp_all <- bind_rows(d2, predict_dailyhosp) %>% 
  mutate(group = fct_relevel(group, "South Dakota", "Minnehaha County"))



raw_active_data <- read_csv(file = "data/data_kelo.csv") %>% clean_names() %>% as_tibble() %>% 
  mutate(date = mdy(date),
         group = "South Dakota",
         Model_1 = hospitalized_currently,
         Model_2 = hospitalized_currently) %>% 
  select(-hospitalized_currently) %>% 
  gather(model, hospitalized_currently, c("Model_1", "Model_2")) %>% 
  mutate(group = fct_relevel(group, "South Dakota", "Minnehaha County"),
         model = case_when(model == "Model_1" ~ "Model 1", TRUE ~ "Model 2"))

#brewer.pal(n = 3, "Accent")

active_daily_group <- daily_hosp_all %>% 
  ggplot(aes(x = date, y= median, fill = group)) + 
  geom_line() +
  geom_ribbon(aes(fill = group, ymin = low90, ymax = high90), alpha = 0.2) +
  geom_ribbon(aes(fill = group, ymin = low50, ymax = high50), alpha = 0.2) +
  facet_grid(hosp_guess ~ model) +
  theme_classic() +
  # scale_color_manual(values = c("grey10", "#7FC97F", "#BEAED4")) +
  scale_fill_manual(values = c("grey10", "#7FC97F", "#BEAED4")) +
  geom_point(data = raw_active_data, aes(x = date, y = hospitalized_currently)) +
  # geom_hline(yintercept = all_beds, color = "dodgerblue") +
  # annotate("text", x = as.Date("2020-04-10"), y = 2960, label = "All Hospital Beds\nin SD") +
  labs(y = "Active hospitalizations",
       x = "") +
  # coord_cartesian(ylim = c(0,500)) +
  theme(legend.title = element_blank()) +
  NULL

saveRDS(active_daily_group, file = here("plots/active_daily_group.rds"))
ggsave(active_daily_group, file = here("plots/active_daily_group.jpg"), dpi = 600, width = 8, height = 16)


## ----echo=FALSE, fig.cap="Comparison of prior and posterior distributions for each parameter in models 1 and 2.", fig.height=4, fig.width=12, message=FALSE, warning=FALSE, paged.print=FALSE----
poststate <- posterior_samples(fit_weib_state) %>% as_tibble() %>% clean_names()
postgroup <- posterior_samples(fit_weib_group_f) %>% as_tibble() %>% clean_names()

poststate_g <- poststate %>% mutate(model = "Model 1", 
                     iter = 1:nrow(.)) %>% 
  select(-lp) %>% 
  gather(key, value, c(-model,-iter)) %>% 
  mutate(prior_post = case_when(grepl("prior", key) ~ "Prior", TRUE ~ "Posterior"),
         key = str_remove(key, "prior_")) %>% 
  separate(key, c("b","param","c"))



postgroup_g <- postgroup %>% mutate(model = "Model 2", 
                     iter = 1:nrow(.)) %>% 
  select(-lp) %>% 
  gather(key, value, c(-model,-iter)) %>% 
  mutate(prior_post = case_when(grepl("prior", key) ~ "Prior", TRUE ~ "Posterior"),
         key = str_remove(key, "prior_")) %>% 
  separate(key, c("b","param","c"), remove = F) %>% 
  mutate(param = case_when(grepl("ult_group", key) ~ "Beta1",
                           grepl("omega_group", key) ~ "Beta2",
                           grepl("theta_group", key) ~ "Beta3",
                           TRUE ~ param))


allpost <- bind_rows(poststate_g, postgroup_g) %>% 
  mutate(prior_post = fct_relevel(prior_post, "Prior"),
         param = case_when(param == "ult" ~ "max",
                           param == "theta" ~ "slope",
                           param == "omega" ~ "inflection",
                           TRUE ~ param),
         param = fct_relevel(param, "max", "slope", "inflection"))

prior_post_all <- allpost %>% 
  ggplot(aes(x = value, y = ..scaled.., fill = prior_post)) +
  geom_density() +
  facet_grid(model ~ param, scales = "free") +
  scale_fill_brewer() +
  theme_bw() + 
  theme(legend.title = element_blank()) + 
  labs(x = "Parameter Values",
       y = "Density (scaled)") +
  NULL

saveRDS(prior_post_all, file = here("plots/prior_post_all.rds"))
ggsave(prior_post_all, file = here("plots/prior_post_all.jpg"), dpi = 600, width = 8, height = 8)

## ----echo=FALSE, fig.cap="Posterior distributions and trace plots for each parameter from Model 1.", fig.height=5, fig.width=8, message=FALSE, warning=FALSE, paged.print=FALSE----

trace1 <- plot(fit_weib_state, N = 10)
saveRDS(trace1, file = here("plots/trace1.rds"))

## ----echo=FALSE, fig.cap="Posterior distributions and trace plots for each parameter from Model 2.", fig.height=10, fig.width=8, message=FALSE, warning=FALSE, paged.print=FALSE----
trace2 <- plot(fit_weib_group_f, N = 10)
saveRDS(trace2, file = here("plots/trace2.rds"))

## ----Model 1, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------------
mod1.stan <- stancode(fit_weib_state)
saveRDS(mod1.stan, file = here("code/mod1.stan"))

## ----Model 2, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE--------------------
mod2.stan <- stancode(fit_weib_group_f)
saveRDS(mod2.stan, file = here("code/mod2.stan"))
