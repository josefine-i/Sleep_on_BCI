#### Effect of weather on waking period sleep ####

library(rstan)
library(brms)
library(cmdstanr)
library(dagitty)
library(ggplot2)
library(mixR)
library(ggpubr)


options(mc.cores = parallel::detectCores()) 

#### DAG of Weather influences ####

# plotting a DAG to evaluate the calusal interfence of weather 
sleep_weather_dag <- dagitty('dag {
                             rain -> sleep
                             temp -> sleep
                             humid -> sleep
                             temp -> humid
                             rain -> temp
                             rain -> humid }')
plot(sleep_weather_dag)

# checking which variables need to be included to avoid confounding
adjustmentSets(sleep_weather_dag, exposure = "temp", outcome = "sleep" )

# opening the needed file
sleep_per_nona = read.csv("/Users/capuchin_monkey/Documents/Sleep_on_BCI/DATA/All_species_sleep_with_weather.csv")
sleep_per_nona$sunset = as.POSIXct( as.character(sleep_per_nona$sunset), tz = 'America/Panama')
sleep_per_nona$sunrise = as.POSIXct( as.character(sleep_per_nona$sunrise), tz = 'America/Panama')
sleep_per_nona$night_or_day = as.Date( as.character(sleep_per_nona$night_or_day))

nrow(sleep_per_nona[which(is.na(sleep_per_nona$prev_active_per_sleep)),])
sleep_per_nona = sleep_per_nona[which(!is.na(sleep_per_nona$prev_active_per_sleep)),]


#### Correlation between TST and temperature ####
## we need to work with prev_active_per_sleep here as this is the timeframe in which we added the temperature to the dataframe 
hist(sleep_per_nona$prev_active_per_sleep, breaks = 100)
median(sleep_per_nona$prev_active_per_sleep)
mean(sleep_per_nona$prev_active_per_sleep)

## minimum temp correlation model
Active_sleep_min_temp_model <- brm(bf(prev_active_per_sleep ~ min_temp_active_per * species + rain_sleep_per + 
                                        (1 + min_temp_active_per | species / ID), decomp = "QR"), 
                          data = sleep_per_nona,
                          save_pars = save_pars(all = TRUE),
                          iter = 2000,
                          init = 0,
                          prior = c(
                            prior(gamma(.1, .1), class = shape ),
                            prior(student_t(3, 100, 2.5), class = Intercept), #
                            prior(student_t(3, 0, 2.5), class = sd ),
                            prior(student_t(3, 0, 2.5), class = b)
                          ),
                          family = Gamma(link = "log"), 
                          backend = "cmdstanr",
                          threads = threading(2),
                          control = list(max_treedepth = 10, adapt_delta = .999))


# check the model prediction 
summary(Active_sleep_min_temp_model)
pp_check(Active_sleep_min_temp_model)

posterior_interval(Active_sleep_min_temp_model)

# adding criterion for R2 or comparison
Active_sleep_min_temp_model = add_criterion(Active_sleep_min_temp_model, c("loo", "loo_R2"),reloo = TRUE, 
                                   backend = "cmdstanr", 
                                   control = list(max_treedepth = 10, adapt_delta = .999)) # adjust this to model 

# save the model
save(Active_sleep_min_temp_model, file = "Active_sleep_min_temp_model")
saveRDS(Active_sleep_min_temp_model, file = "Active_sleep_min_temp_model_RDS")



#### Model per species ####

##### Capuchins #####
Cap_sleep = sleep_per_nona[which(sleep_per_nona$species == 'Cebus capucinus'),]
hist(Cap_sleep$prev_active_per_sleep, breaks = 100)
Cap_sleep$prev_active_per_sleep = Cap_sleep$prev_active_per_sleep + 0.00001
#exponential?

Cap_nap_temp_model <- brm(bf(prev_active_per_sleep ~ max_temp_active_per + rain_active_per + ( max_temp_active_per | ID), decomp = "QR"), 
                          data = Cap_sleep,
                          save_pars = save_pars(all = TRUE),
                          iter = 4000,
                          init = 0,
                          prior = c(
                            prior(student_t(3, 2, 3.5), class = Intercept), #
                            prior(student_t(3, 0, 3.5), class = sd ),
                            prior(normal(0, .1), class = b)#
                          ),
                          family = exponential(link="log"), 
                          backend = "cmdstanr",
                          threads = threading(6),
                          control = list(max_treedepth = 15, adapt_delta = .99999))

prior_summary(Cap_nap_temp_model)
# check the model prediction 
print(summary(Cap_nap_temp_model),digits=3)
pp_check(Cap_nap_temp_model)

# adding criterion for R2 or comparison
Cap_nap_temp_model = add_criterion(Cap_nap_temp_model, c("loo", "loo_R2"),reloo = TRUE, 
                                      backend = "cmdstanr", 
                                      control = list(max_treedepth = 15, adapt_delta = .99999)) # adjust this to model 

# check R2
print(loo_R2(Cap_nap_temp_model),digits=3)
loo(Cap_nap_temp_model)

# save the model
save(Cap_nap_temp_model, file = "Cap_nap_temp_model")
saveRDS(Cap_nap_temp_model, file = "Cap_nap_temp_model_RDS")

# see effect
h1 = hypothesis(Cap_nap_temp_model,c("b_max_temp_active_per>0","b_max_temp_active_per<0", "b_rain_active_per>0","b_rain_active_per<0"),class="")
print(h1,digits=4)



#### #Spider monkeys #####
Spider_sleep = sleep_per_nona[which(sleep_per_nona$species == "Ateles geoffroyi"),]
hist(Spider_sleep$prev_active_per_sleep, breaks = 100)
#skewed normal?

# maximum temp 
Spider_nap_temp_model <- brm(bf(prev_active_per_sleep ~ max_temp_active_per + rain_active_per + (max_temp_active_per | ID), decomp = "QR"), 
                           data = Spider_sleep,
                           save_pars = save_pars(all = TRUE),
                           init = 0,
                           iter = 4000,
                           prior = c(
                             prior(student_t(3, 84, 5), class = Intercept), #
                             prior(student_t(3, 0, 5), class = sd ),
                             # prior(normal(0, 1), class = alpha), #
                             prior(normal(0, 1), class = b)#
                             #used to be 0,.5
                           ), 
                           family = skew_normal, 
                           backend = "cmdstanr",
                           threads = threading(6),
                           control = list(max_treedepth = 12, adapt_delta = .99999999999))

prior_summary(Spider_nap_temp_model)
# check the model prediction 
print(summary(Spider_nap_temp_model),digits=3)
pp_check(Spider_nap_temp_model)

# adding criterion for R2 or comparison
Spider_nap_temp_model = add_criterion(Spider_nap_temp_model, c("loo", "loo_R2"),reloo = TRUE, 
                                    backend = "cmdstanr", 
                                    control = list(max_treedepth = 12, adapt_delta = .99999999999)) # adjust this to model 

# check R2
print(loo_R2(Spider_nap_temp_model),digits=3)
loo(Spider_nap_temp_model)

# save the model
save(Spider_nap_temp_model, file = "Spider_nap_temp_model")
saveRDS(Spider_nap_temp_model, file = "Spider_nap_temp_model_RDS")

# see effect
h2 = hypothesis(Spider_nap_temp_model,c("b_max_temp_active_per>0","b_max_temp_active_per<0", "b_rain_active_per>0","b_rain_active_per<0"),class="")
print(h2,digits=4)

plot(conditional_effects(Spider_nap_temp_model, spaghetti = F),points = TRUE) 

##### Coatis #####
Coati_sleep = sleep_per_nona[which(sleep_per_nona$species == "Nasua narica"),]
hist(Coati_sleep$prev_active_per_sleep, breaks = 100)
#exponential?
Coati_sleep$prev_active_per_sleep = Coati_sleep$prev_active_per_sleep + 0.00001

Coati_nap_temp_model <- brm(bf(prev_active_per_sleep ~ max_temp_active_per + rain_active_per + (max_temp_active_per | ID), decomp = "QR"), 
                            data = Coati_sleep,
                            save_pars = save_pars(all = TRUE),
                            init = 0,
                            iter = 2000,
                            prior = c(
                              prior(student_t(3, 3, 2.5), class = Intercept), #
                              prior(student_t(3, 0, 2.5), class = sd ),
                              prior(normal(0, .1), class = b)
                            ),
                            family =  exponential(link="log"), 
                            backend = "cmdstanr",
                            threads = threading(4),
                            control = list(max_treedepth = 14, adapt_delta = .999))

prior_summary(Coati_nap_temp_model)
# check the model prediction 
print(summary(Coati_nap_temp_model),digits=3)
pp_check(Coati_nap_temp_model)

# adding criterion for R2 or comparison
Coati_nap_temp_model = add_criterion(Coati_nap_temp_model, c("loo", "loo_R2"),reloo = TRUE, 
                                     backend = "cmdstanr", 
                                     control = list(max_treedepth = 14, adapt_delta = .999)) # adjust this to model 

# check R2
print(loo_R2(Coati_nap_temp_model),digits=4)
loo(Coati_nap_temp_model)

# save the model
save(Coati_nap_temp_model, file = "Coati_nap_temp_model")
saveRDS(Coati_nap_temp_model, file = "Coati_nap_temp_model_RDS")

# see effect
h3 = hypothesis(Coati_nap_temp_model,c("b_max_temp_active_per>0","b_max_temp_active_per<0", "b_rain_active_per>0","b_rain_active_per<0"),class="")
print(h3,digits=4)

plot(conditional_effects(Coati_nap_temp_model, spaghetti = F),points = TRUE) 


##### Kinkajous #####
Kinkajou_sleep = sleep_per_nona[which(sleep_per_nona$species == "Potos flavus"),]
hist(Kinkajou_sleep$prev_active_per_sleep, breaks = 100)
Kinkajou_sleep$prev_active_per_sleep = Kinkajou_sleep$prev_active_per_sleep + 0.00001

Kinkajou_nap_temp_model <- brm(bf(prev_active_per_sleep ~ max_temp_active_per + rain_active_per + (max_temp_active_per | ID), decomp = "QR"), 
                               data = Kinkajou_sleep,
                               save_pars = save_pars(all = TRUE),
                               init = 0,
                               iter = 2000,
                               prior = c(
                                 prior(student_t(3, 1, 2.5), class = Intercept), 
                                 prior(student_t(3, 0, 2.5), class = sd ),
                                 prior(student_t(3, 0, 2.5), class = b)
                               ),
                               family =  exponential(link="log"), 
                               backend = "cmdstanr",
                               threads = threading(4),
                               control = list(max_treedepth = 15, adapt_delta = .9999))


prior_summary(Kinkajou_nap_temp_model)
# check the model prediction 
print(summary(Kinkajou_nap_temp_model),digits=3)
pp_check(Kinkajou_nap_temp_model)

# adding criterion for R2 or comparison
Kinkajou_nap_temp_model = add_criterion(Kinkajou_nap_temp_model, c("loo", "loo_R2"),reloo = TRUE, 
                                        backend = "cmdstanr", 
                                        control = list(max_treedepth = 15, adapt_delta = .9999)) # adjust this to model 

# check R2
print(loo_R2(Kinkajou_nap_temp_model),digits=3)
loo(Kinkajou_nap_temp_model)

# save the model
save(Kinkajou_nap_temp_model, file = "Kinkajou_nap_temp_model")
saveRDS(Kinkajou_nap_temp_model, file = "Kinkajou_nap_temp_model_RDS")

# see effect
h4 = hypothesis(Kinkajou_nap_temp_model,c("b_max_temp_active_per>0","b_max_temp_active_per<0", "b_rain_active_per>0","b_rain_active_per<0"),class="")
print(h4,digits=4)

plot(conditional_effects(Kinkajou_nap_temp_model, spaghetti = F),points = TRUE) 

#### Plotting ####

## dataframes of models 
Cap_nap <- conditional_effects(Cap_nap_temp_model)
effects_caps <- as.data.frame(Cap_nap[[1]])
Spider_nap <- conditional_effects(Spider_nap_temp_model)
effects_spider <- as.data.frame(Spider_nap[[1]])
Coa_nap <- conditional_effects(Coati_nap_temp_model)
effects_coa <- as.data.frame(Coa_nap[[1]])
Kin_nap <- conditional_effects(Kinkajou_nap_temp_model)
effects_kin <- as.data.frame(Kin_nap[[1]])

Cap_plot = ggplot(effects_caps, aes(x = max_temp_active_per, y = estimate__)) +
  geom_point(Cap_sleep, mapping = aes(x= max_temp_active_per, y = prev_active_per_sleep), color = "#444444", size = 1.0)+
  geom_ribbon(effects_caps, mapping = aes(ymin = lower__, ymax = upper__), fill = "#0D0887FF", alpha = 0.3) +
  geom_line(effects_caps, mapping= aes(x = max_temp_active_per, y = estimate__), linewidth = 1.5, color = "#0D0887FF") +
  theme_classic(base_size = 15) + 
  labs(x="", y = "") +
  scale_x_continuous(limits = c(min(Cap_sleep$max_temp_active_per), max(Cap_sleep$max_temp_active_per)), expand = c(0.02,0.03))
Cap_plot

Spider_plot = ggplot(effects_spider, aes(x = max_temp_active_per, y = estimate__)) +
  geom_point(Spider_sleep, mapping = aes(x= max_temp_active_per, y = prev_active_per_sleep), color = "#444444", size = 1.0)+
  geom_ribbon(effects_spider, mapping = aes(ymin = lower__, ymax = upper__), fill = "#7E03A8FF", alpha = 0.3) +
  geom_line(effects_spider, mapping= aes(x = max_temp_active_per, y = estimate__), linewidth = 1.5, color = "#7E03A8FF") +
  theme_classic(base_size = 15) + 
  labs(x="", y = "") +
  scale_x_continuous(limits = c(min(Spider_sleep$max_temp_active_per), max(Spider_sleep$max_temp_active_per)), expand = c(0.02,0.03))

Coa_plot = ggplot(effects_coa, aes(x = max_temp_active_per, y = estimate__)) +
  geom_point(Coati_sleep, mapping = aes(x= max_temp_active_per, y = prev_active_per_sleep), color = "#444444", size = 1.0)+
  geom_ribbon(effects_coa, mapping = aes(ymin = lower__, ymax = upper__), fill = "#CC4678FF", alpha = 0.3) +
  geom_line(effects_coa, mapping= aes(x = max_temp_active_per, y = estimate__), linewidth = 1.5, color = "#CC4678FF") +
  theme_classic(base_size = 15) + 
  labs(x="", y = "") +
  scale_x_continuous(limits = c(min(Coati_sleep$max_temp_active_per), max(Coati_sleep$max_temp_active_per)), expand = c(0.02,0.03))

Kin_plot = ggplot(effects_kin, aes(x = max_temp_active_per, y = estimate__)) +
  geom_point(Kinkajou_sleep, mapping = aes(x= max_temp_active_per, y = prev_active_per_sleep), color = "#444444", size = 1.0)+
  geom_ribbon(effects_kin, mapping = aes(ymin = lower__, ymax = upper__), fill = "#F89441FF", alpha = 0.3) +
  geom_line(effects_kin, mapping= aes(x = max_temp_active_per, y = estimate__), linewidth = 1.5, color = "#F89441FF") +
  theme_classic(base_size = 15) + 
  labs(x="", y = "") +
  scale_x_continuous(limits = c(min(Kinkajou_sleep$max_temp_active_per), max(Kinkajou_sleep$max_temp_active_per)), expand = c(0.02,0.03))

## plot all together 
tiff("nap_temp_plot.png", width = 10.5, height = 9, units = "in", res = 500)
figure <- ggarrange(Cap_plot, Spider_plot, Coa_plot, Kin_plot, 
                    ncol = 2, nrow = 2, labels = c(5, 6, 7, 8), label.x = 0.025)

annotate_figure(figure,
                bottom = text_grob("Maximum temperature [ºC]",face = "plain", size = 16, vjust = -0.5, hjust = 0.45),
                left = text_grob("TST of active period [min]", size = 16, rot = 90, vjust = 1.3, hjust = 0.32))

dev.off()







