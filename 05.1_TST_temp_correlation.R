#### Temperature effect on TST sleep period ####


library(rstan)
library(brms)
library(cmdstanr)
library(dagitty)
library(ggplot2)
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

# save the DAG
save(sleep_weather_dag, file = "sleep_weather_dag")

# opening the needed file
sleep_per_nona = read.csv("/Users/capuchin_monkey/Documents/Sleep_on_BCI/DATA/All_species_sleep_with_weather.csv")
sleep_per_nona$sunset = as.POSIXct( as.character(sleep_per_nona$sunset), tz = 'America/Panama')
sleep_per_nona$sunrise = as.POSIXct( as.character(sleep_per_nona$sunrise), tz = 'America/Panama')
sleep_per_nona$night_or_day = as.Date( as.character(sleep_per_nona$night_or_day))


#### Correlation between TST and temperature ####
hist(sleep_per_nona$TST_sleep_per, breaks = 100)
mean(sleep_per_nona$TST_sleep_per)


#### Model per species ####

##### Capuchins #####
Cap_sleep = sleep_per_nona[which(sleep_per_nona$species == 'Cebus capucinus'),]
hist(Cap_sleep$TST_sleep_per, breaks = 100)
median(Cap_sleep$TST_sleep_per)
#skew normal?

Cap_temp_TST_model <- brm(bf(TST_sleep_per ~ mean_temp_sleep_per + rain_sleep_per + ( mean_temp_sleep_per + rain_sleep_per |ID),decomp = "QR"), 
                          data = Cap_sleep,
                          save_pars = save_pars(all = TRUE),
                          iter = 4000,
                          init = 0,
                          prior = c(
                            prior(student_t(3, 480, 10), class = Intercept), #
                            #prior(normal(0, 4), class = alpha), #did not use to be there
                            prior(student_t(3, 0, 10), class = sd ),#
                            prior(normal(0, 100), class = b ) #used to be 0,100
                          ),
                          family = skew_normal, 
                          backend = "cmdstanr",
                          threads = threading(6),
                          control = list(max_treedepth = 12, adapt_delta = .9999999999))


prior_summary(Cap_temp_TST_model)
# check the model prediction 
print(summary(Cap_temp_TST_model),digits=3)
pp_check(Cap_temp_TST_model)

# adding criterion for R2 or comparison
Cap_temp_TST_model = add_criterion(Cap_temp_TST_model, c("loo", "loo_R2"),reloo = TRUE, 
                                   backend = "cmdstanr", 
                                   control = list(max_treedepth = 12, adapt_delta = .9999999999)) # adjust this to model 

# check R2
print(loo_R2(Cap_temp_TST_model),digits=4)
loo(Cap_temp_TST_model)

# save the model
save(Cap_temp_TST_model, file = "Cap_temp_TST_model")
saveRDS(Cap_temp_TST_model, file = "Cap_temp_TST_model_RDS")

# see effect
h1 = hypothesis(Cap_temp_TST_model,c("b_mean_temp_sleep_per>0","b_mean_temp_sleep_per<0", "b_rain_sleep_per>0","b_rain_sleep_per<0"),class="")
print(h1,digits=4)

plot(conditional_effects(Cap_temp_TST_model, spaghetti = F),points = TRUE) 



#### #Spider monkeys #####
Spider_sleep = sleep_per_nona[which(sleep_per_nona$species == "Ateles geoffroyi"),]
hist(Spider_sleep$TST_sleep_per, breaks = 100)
#skewed normal?

Spider_temp_TST_model <- brm(bf(TST_sleep_per ~ mean_temp_sleep_per + rain_sleep_per + ( mean_temp_sleep_per + rain_sleep_per |ID),decomp = "QR"), 
                             data = Spider_sleep,
                             save_pars = save_pars(all = TRUE),
                             iter = 2000,
                             init = 0,
                             prior = c(
                               prior(student_t(3, 470, 10), class = Intercept), 
                               prior(student_t(3, 0, 10), class = sd ),#
                               prior(normal(0, 100), class = b ) #
                             ),
                             family = skew_normal, 
                             backend = "cmdstanr",
                             threads = threading(6),
                             control = list(max_treedepth = 12, adapt_delta = .999))

prior_summary(Spider_temp_TST_model)
# check the model prediction 
print(summary(Spider_temp_TST_model),digits=3)
pp_check(Spider_temp_TST_model)

# adding criterion for R2 or comparison
Spider_temp_TST_model = add_criterion(Spider_temp_TST_model, c("loo", "loo_R2"),reloo = TRUE, 
                                   backend = "cmdstanr", 
                                   control = list(max_treedepth = 12, adapt_delta = .999)) # adjust this to model 
# check R2
print(loo_R2(Spider_temp_TST_model),digits=4)
loo(Spider_temp_TST_model)

# save the model
save(Spider_temp_TST_model, file = "Spider_temp_TST_model")
saveRDS(Spider_temp_TST_model, file = "Spider_temp_TST_model_RDS")

# see effect
h2 = hypothesis(Spider_temp_TST_model,c("b_mean_temp_sleep_per>0","b_mean_temp_sleep_per<0", "b_rain_sleep_per>0","b_rain_sleep_per<0"),class="")
print(h2,digits=4)

plot(conditional_effects(Spider_temp_TST_model, spaghetti = F),points = TRUE) 



##### Coatis #####
Coati_sleep = sleep_per_nona[which(sleep_per_nona$species == "Nasua narica"),]
hist(Coati_sleep$TST_sleep_per, breaks = 100)
#mixture?
mix <- mixture(gaussian, skew_normal)

coati_priors <- c(
  prior(normal(300, 10), class = Intercept, dpar = mu1),#used to be 0, 300
  prior(normal(600, 10), class = Intercept, dpar = mu2), #used to be 500, 600
  prior(student_t(3, 0, 5), class = b, dpar = mu1),
  prior(student_t(3, 0, 5), class = b, dpar = mu2),
  #prior(gamma(2, .5), class = sigma1),
  #prior(gamma(2, .1), class = sigma2),
  prior(student_t(3, 0, 5), class = sd, dpar = mu1),
  prior(student_t(3, 0, 5), class = sd, dpar = mu2)
)#next: try adding sigma priors

Coati_temp_TST_model <- brm(bf(TST_sleep_per ~ mean_temp_sleep_per + rain_sleep_per + ( mean_temp_sleep_per + rain_sleep_per |ID),decomp = "QR"), 
                            data = Coati_sleep,
                            save_pars = save_pars(all = TRUE),
                            iter = 10000,
                            init = 0,
                            prior = coati_priors,
                            family = mix, 
                            backend = "cmdstanr",
                            threads = threading(8),
                            control = list(max_treedepth = 14, adapt_delta = .9999999999999))

prior_summary(Coati_temp_TST_model)
# check the model prediction 
print(summary(Coati_temp_TST_model),digits=3)
pp_check(Coati_temp_TST_model)

# adding criterion for R2 or comparison
Coati_temp_TST_model = add_criterion(Coati_temp_TST_model, c("loo", "loo_R2"),reloo = TRUE, 
                                        backend = "cmdstanr", 
                                        control = list(max_treedepth = 14, adapt_delta = .9999999999999)) # adjust this to model 
# check R2
print(loo_R2(Coati_temp_TST_model),digits=4)
loo(Coati_temp_TST_model)

# save the model
save(Coati_temp_TST_model, file = "Coati_temp_TST_model")
saveRDS(Coati_temp_TST_model, file = "Coati_temp_TST_model_RDS")

# see effect
h3 = hypothesis(Coati_temp_TST_model,c("b_mu1_mean_temp_sleep_per>0","b_mu1_mean_temp_sleep_per<0","b_mu2_mean_temp_sleep_per>0","b_mu2_mean_temp_sleep_per<0", 
                                       "b_mu1_rain_sleep_per>0","b_mu1_rain_sleep_per<0", "b_mu2_rain_sleep_per>0","b_mu2_rain_sleep_per<0"),class="")
print(h3,digits=4)

plot(conditional_effects(Coati_temp_TST_model, spaghetti = F),points = TRUE) 



##### Kinkajous #####
Kinkajou_sleep = sleep_per_nona[which(sleep_per_nona$species == "Potos flavus"),]
hist(Kinkajou_sleep$TST_sleep_per, breaks = 100)

Kinkajou_temp_TST_model <- brm(bf(TST_sleep_per ~ mean_temp_sleep_per + rain_sleep_per + ( mean_temp_sleep_per + rain_sleep_per |ID),decomp = "QR"), 
                               data = Kinkajou_sleep,
                               save_pars = save_pars(all = TRUE),
                               iter = 4000,
                               init = 0,
                               prior = c(
                                 prior(student_t(3, 610, 5), class = Intercept), 
                                 prior(student_t(3, 0, 5), class = sd ),#
                                 prior(normal(0, 100), class = b ) #used to be 0,100
                               ),
                               family = skew_normal, 
                               backend = "cmdstanr",
                               threads = threading(6),
                               control = list(max_treedepth = 12, adapt_delta = .999999999999))

prior_summary(Kinkajou_temp_TST_model)
# check the model prediction 
print(summary(Kinkajou_temp_TST_model),digits=3)
pp_check(Kinkajou_temp_TST_model)

# adding criterion for R2 or comparison
Kinkajou_temp_TST_model = add_criterion(Kinkajou_temp_TST_model, c("loo", "loo_R2"),reloo = TRUE, 
                                      backend = "cmdstanr", 
                                      control = list(max_treedepth = 12, adapt_delta = .999999999999)) # adjust this to model 
# check R2
print(loo_R2(Kinkajou_temp_TST_model),digits=4)
loo(Kinkajou_temp_TST_model)

# save the model
save(Kinkajou_temp_TST_model, file = "Kinkajou_temp_TST_model")
saveRDS(Kinkajou_temp_TST_model, file = "Kinkajou_temp_TST_model_RDS")

# see effect
h4 = hypothesis(Kinkajou_temp_TST_model,c("b_mean_temp_sleep_per>0","b_mean_temp_sleep_per<0", "b_rain_sleep_per>0","b_rain_sleep_per<0"),class="")
print(h4,digits=4)

plot(conditional_effects(Kinkajou_temp_TST_model, spaghetti = F),points = TRUE) 


#### Plotting ####

## dataframes of models 
Cap_temp <- conditional_effects(Cap_temp_TST_model)
effects_caps <- as.data.frame(Cap_temp[[1]])
Spider_temp <- conditional_effects(Spider_temp_TST_model)
effects_spider <- as.data.frame(Spider_temp[[1]])
Coa_temp <- conditional_effects(Coati_temp_TST_model)
effects_coa <- as.data.frame(Coa_temp[[1]])
Kin_temp <- conditional_effects(Kinkajou_temp_TST_model)
effects_kin <- as.data.frame(Kin_temp[[1]])

Cap_plot = ggplot(effects_caps, aes(x = mean_temp_sleep_per, y = estimate__)) +
  geom_point(Cap_sleep, mapping = aes(x= mean_temp_sleep_per, y = TST_sleep_per), color = "#444444", size = 1.0)+
  geom_ribbon(effects_caps, mapping = aes(ymin = lower__, ymax = upper__), fill = "#0D0887FF", alpha = 0.3) +
  geom_line(effects_caps, mapping= aes(x = mean_temp_sleep_per, y = estimate__), linewidth = 1.5, color = "#0D0887FF") +
  theme_classic(base_size = 15) + 
  labs(x="", y = "") +
  scale_x_continuous(limits = c(min(Cap_sleep$mean_temp_sleep_per), max(Cap_sleep$mean_temp_sleep_per)), expand = c(0.02,0.03))
Cap_plot

Spider_plot = ggplot(effects_spider, aes(x = mean_temp_sleep_per, y = estimate__)) +
  geom_point(Spider_sleep, mapping = aes(x= mean_temp_sleep_per, y = TST_sleep_per), color = "#444444", size = 1.0)+
  geom_ribbon(effects_spider, mapping = aes(ymin = lower__, ymax = upper__), fill = "#7E03A8FF", alpha = 0.3) +
  geom_line(effects_spider, mapping= aes(x = mean_temp_sleep_per, y = estimate__), linewidth = 1.5, color = "#7E03A8FF") +
  theme_classic(base_size = 15) + 
  labs(x="", y = "") +
  scale_x_continuous(limits = c(min(Spider_sleep$mean_temp_sleep_per), max(Spider_sleep$mean_temp_sleep_per)), expand = c(0.02,0.03))

Coa_plot = ggplot(effects_coa, aes(x = mean_temp_sleep_per, y = estimate__)) +
  geom_point(Coati_sleep, mapping = aes(x= mean_temp_sleep_per, y = TST_sleep_per), color = "#444444", size = 1.0)+
  geom_ribbon(effects_coa, mapping = aes(ymin = lower__, ymax = upper__), fill = "#CC4678FF", alpha = 0.3) +
  geom_line(effects_coa, mapping= aes(x = mean_temp_sleep_per, y = estimate__), linewidth = 1.5, color = "#CC4678FF") +
  theme_classic(base_size = 15) + 
  labs(x="", y = "") +
  scale_x_continuous(limits = c(min(Coati_sleep$mean_temp_sleep_per), max(Coati_sleep$mean_temp_sleep_per)), expand = c(0.02,0.03))

Kin_plot = ggplot(effects_kin, aes(x = mean_temp_sleep_per, y = estimate__)) +
  geom_point(Kinkajou_sleep, mapping = aes(x= mean_temp_sleep_per, y = TST_sleep_per), color = "#444444", size = 1.0)+
  geom_ribbon(effects_kin, mapping = aes(ymin = lower__, ymax = upper__), fill = "#F89441FF", alpha = 0.3) +
  geom_line(effects_kin, mapping= aes(x = mean_temp_sleep_per, y = estimate__), linewidth = 1.5, color = "#F89441FF") +
  theme_classic(base_size = 15) + 
  labs(x="", y = "") +
  scale_x_continuous(limits = c(min(Kinkajou_sleep$mean_temp_sleep_per), max(Kinkajou_sleep$mean_temp_sleep_per)), expand = c(0.02,0.03))

## plot all together 
tiff("TST_temp_plot_new.png", width = 10.5, height = 9, units = "in", res = 500)
figure <- ggarrange(Cap_plot, Spider_plot, Coa_plot, Kin_plot, 
                    ncol = 2, nrow = 2, labels = c(1, 2, 3, 4), label.x = 0.025)

annotate_figure(figure,
                bottom = text_grob("Mean temperature [ºC]",face = "plain", size = 16, vjust = -0.5, hjust = 0.45),
                left = text_grob("TST of sleep period [min]", size = 16, rot = 90, vjust = 1.3, hjust = 0.32))

dev.off()



