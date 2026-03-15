#### DESCRIPTION ####

# This script analyses the data looking at the effect of temperature on winter 
# moth pre-oviposition time

#### SET UP ####

# packages
library(here)
library(tidyverse)

# data
adult_moths <- read.csv(here("data_files", "adult_moths.csv"))
pupal_temperature_treatments <- read.csv(here("data_files", "pupa_temperature_treatments_daily.csv"))

# reorder temperature treatments for plots
adult_moths$pupa_temperature_treatment <- factor(adult_moths$pupa_temperature_treatment,
                                                 levels = c("cold", "cool", "mean", "warm", "hot", "ambient"))
# set up a common colour palette for plots
temperature_treatment_colours <- c("cold" = "#2166AC",
                                   "cool" = "#63BFB4",
                                   "mean" = "#FDDC7A",
                                   "warm" = "#EF8A62", 
                                   "hot" = "#B2182B",
                                   "ambient" = "#777777") 


#### PLOT ####

# filter the adult moth data for females mated within treatment and remove NAs
# i.e., one female that didn't survive long enough to be mated and any females 
# that laid no eggs 
female_moths_clean <- adult_moths %>%
  filter(sex == "female") %>%
  filter(!adult_id %in% c("f1.cool", "f1.cold")) %>%
  drop_na(pre_oviposition_time)

# plot the mean pupal development time for each temperature treatment with the 
# raw data points
pre_oviposition_time_plot <- ggplot(female_moths_clean, 
                                    aes(x = pupa_temperature_treatment, 
                                        y = pre_oviposition_time, 
                                        fill = pupa_temperature_treatment,
                                        colour = pupa_temperature_treatment)) +
  geom_jitter(width = 0.2, 
              alpha = 0.7, 
              size = 3, 
              stroke = 0) +
  stat_summary(fun = mean, 
               geom = "crossbar", 
               width = 0.5,
               linewidth = .8, 
               alpha = 1, 
               lineend = "round") +
  geom_vline(xintercept = 5.5, 
             linetype = "dashed", 
             color = "black", 
             linewidth = 1) +
  labs(x = "Pupa Temperature Treatment",
       y = "Pre-oviposition time (days)") +
  scale_fill_manual(values = temperature_treatment_colours) +
  scale_colour_manual(values = temperature_treatment_colours) +
  theme_bw() +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none")

# look at the output 
pre_oviposition_time_plot

# save as a png in the figures folder 
ggsave(here("figures", "figureS4.png"), 
       plot = pre_oviposition_time_plot, width = 6, height = 4, dpi = 300)

#### MODEL ####

# filter the data to remove the control treatment
female_moths_minus_control <- female_moths_clean %>%
  filter(pupa_temperature_treatment != "ambient")

# reorder the temperature treatments so that the mean is the reference fro modelling
female_moths_minus_control$pupa_temperature_treatment <- factor(female_moths_minus_control$pupa_temperature_treatment, 
                                                               levels = c("mean", "cold", "cool", "warm", "hot"))

# subset the numerical pupal temperature data and rename the columns
pupal_temperature_treatments <- pupal_temperature_treatments %>%
  subset(select = c(ambient_rounded, 
                    cold_rounded, 
                    cool_rounded, 
                    mean_rounded, 
                    warm_rounded, 
                    hot_rounded)) %>%
  rename(ambient = ambient_rounded, 
         cold = cold_rounded, 
         cool = cool_rounded, 
         mean = mean_rounded, 
         warm = warm_rounded, 
         hot = hot_rounded) %>%
  drop_na()

# calculate the mean development time for each treatment
mean_pre_oviposition_times <- adult_moths %>%
  group_by(pupa_temperature_treatment) %>%
  summarise(mean_development_time = mean(pupa_development_time))

# look at the output
print(mean_pre_oviposition_times)

# create two objects, one with the temperature treatments and the other with the 
# mean pupal development times 
treatment_names <- c("ambient", "cold", "cool", "mean", "warm", "hot")
mean_development_time <- c(147, 168, 144, 145, 161, 169)

# calculate the mean temperature in the 7 days before the mean development time
# for each temperature treatment
seven_day_mean_temperatures <- mapply(function(treatment, days) {
  start_day <- days - 6
  mean(pupal_temperature_treatments[[treatment]][start_day:days], na.rm = TRUE)}, 
  treatment_names, mean_development_time)

# look at the output
seven_day_mean_temperatures

# assign the categorical temperature treatments a numerical values using mean 
# temperature in the 7 days before the mean development time for each treatment
female_moths_minus_control <- female_moths_minus_control %>%
  mutate(pupa_temperature_treatment_numerical = case_when(
    pupa_temperature_treatment == "mean" ~ 6.24,
    pupa_temperature_treatment == "cold" ~ 0.29,
    pupa_temperature_treatment == "cool" ~ 4.37,
    pupa_temperature_treatment == "warm" ~ 8.10,
    pupa_temperature_treatment == "hot" ~ 9.23))

# model the effect of temperature on pre-oviposition time, include a linear and 
# quadratic component and clutch ID
pre_oviposition_time_model <- lm(pre_oviposition_time ~ pupa_temperature_treatment_numerical + 
                                   I(pupa_temperature_treatment_numerical^2), 
                                 data = female_moths_minus_control)

# look at the output 
summary(pre_oviposition_time_model)

#### CHECK FIT ####

# compare the full model to a reduced model without the squared term 
full_model <- lm(pre_oviposition_time ~ pupa_temperature_treatment_numerical + 
                   I(pupa_temperature_treatment_numerical^2), data = female_moths_minus_control)
reduced_model <- lm(pre_oviposition_time ~ pupa_temperature_treatment_numerical, 
                    data = female_moths_minus_control)

anova(full_model, reduced_model, test = "Chisq")

# compare the full model to a null model
full_model <- lm(pre_oviposition_time ~ pupa_temperature_treatment_numerical + 
                   I(pupa_temperature_treatment_numerical^2), 
                 data = female_moths_minus_control)
null_model <- lm(pre_oviposition_time ~ 1, 
                 data = female_moths_minus_control)

anova(full_model, null_model, test = "Chisq")

#### CHECK ASSUMPTIONS ####

# check the assumption of equal variance by plotting the residuals vs fitted values 
plot(fitted(pre_oviposition_time_model), resid(pre_oviposition_time_model)) 
abline(h = 0, col = "red")


# check the normality of the residuals 
qqnorm(resid(pre_oviposition_time_model)) 
qqline(resid(pre_oviposition_time_model), col = "red")
