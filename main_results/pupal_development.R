#### DESCRIPTION ####

# This script analyses the data looking at the effect of temperature on winter 
# moth pupal development time

#### SET UP ####

# packages
library(here)
library(tidyverse)
library(lmerTest)

# data
adult_moths <- read.csv(here("data_files", "adult_moths.csv"))
pupal_temperature_treatments <- read.csv(here("data_files", "pupa_temperature_treatments_daily.csv"))

# reorder temperature treatments for plots
adult_moths$pupa_temperature_treatment <- factor(adult_moths$pupa_temperature_treatment,
                                                 levels = c("cold", "cool", "mean", "warm", "hot", "ambient"))


# set up a common colour palette for plots
temperature_treatment_colours <- c("cold" = "#1965AE",
                                   "cool" = "#3FAB5C",
                                   "mean" = "#FFDB58",
                                   "warm" = "#E66815", 
                                   "hot" = "#DE1117", 
                                   "ambient" = "#777777") 


#### PLOT ####

# plot the mean pupal development time for each temperature treatment with the 
# raw data points 
pupal_development_time_plot <- ggplot(adult_moths, 
                                      aes(x = pupa_temperature_treatment,
                                          y = pupa_development_time,
                                          fill = pupa_temperature_treatment,
                                          colour = pupa_temperature_treatment)) +
  geom_jitter(width = 0.2, 
              alpha = 0.7, 
              size = 3, 
              stroke = 0) +
  stat_summary(fun = mean, 
               geom = "crossbar", 
               width = 0.5,
               linewidth = 0.8, 
               alpha = 0.8, 
               lineend = "round") +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black", linewidth = 1) +
  labs(x = "Pupa Temperature Treatment",
       y = "Pupal Development Time (days)") +
  scale_fill_manual(values = temperature_treatment_colours) +
  scale_colour_manual(values = temperature_treatment_colours) +
  scale_x_discrete(labels = c("Cold", "Cool", "Mean", "Warm", "Hot", "Ambient")) +
  theme_bw() +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none")

# look at the output
pupal_development_time_plot

# save as a png file in figures folder
ggsave(here("figures", "pupal_development_time_plot.png"), 
            plot = pupal_development_time_plot, width = 6, height = 4, dpi = 300)

#### MODEL ####

# filter the data to remove the control treatment 
adult_moths_minus_control <- adult_moths %>%
  filter(pupa_temperature_treatment != "ambient")

# reorder the temperature treatment so that the mean is the reference for modelling
adult_moths_minus_control$pupa_temperature_treatment <- factor(adult_moths_minus_control$pupa_temperature_treatment, 
                                                               levels = c("mean", "cold", "cool", "warm", "hot"))

# convert pupa development time to a numeric variable
adult_moths_minus_control <- adult_moths_minus_control %>%
  mutate(pupa_development_time = as.numeric(as.character(pupa_development_time)))

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
mean_pupal_development_times <- adult_moths %>%
  group_by(pupa_temperature_treatment) %>%
  summarise(mean_development_time = mean(pupa_development_time))

# look at the output
print(mean_pupal_development_times)

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
adult_moths_minus_control <- adult_moths_minus_control %>%
  mutate(pupa_temperature_treatment_numerical = case_when(
    pupa_temperature_treatment == "mean" ~ 6.24,
    pupa_temperature_treatment == "cold" ~ 0.29,
    pupa_temperature_treatment == "cool" ~ 4.37,
    pupa_temperature_treatment == "warm" ~ 8.10,
    pupa_temperature_treatment == "hot" ~ 9.23))


# model the effect of temperature on pupal development time, include a linear 
# and quadratic component and clutch ID as a random effect
pupal_development_time_model <- lmer(pupa_development_time ~ pupa_temperature_treatment_numerical + 
                                       I(pupa_temperature_treatment_numerical^2) + 
                                       (1 | clutch_id), 
                                     data = adult_moths_minus_control)

# look at the output 
summary(pupal_development_time_model)

#### CHECK FIT ####

# compare the full model to a reduced model without the squared term
full_model <- lm(pupa_development_time ~ pupa_temperature_treatment_numerical + 
                   I(pupa_temperature_treatment_numerical^2), data = adult_moths_minus_control)
reduced_model <- lm(pupa_development_time ~ pupa_temperature_treatment_numerical, 
                    data = adult_moths_minus_control)

anova(full_model, reduced_model, test = "Chisq")

# compare the full model to a null model 
full_model <- lm(pupa_development_time ~ pupa_temperature_treatment_numerical + 
                   I(pupa_temperature_treatment_numerical^2), data = adult_moths_minus_control)
null_model <- lm(pupa_development_time ~ 1, 
                 data = adult_moths_minus_control)

anova(full_model, null_model, test = "Chisq")

#### CHECK ASSUMPTIONS ####

# check the assumption of equal variance by plotting the residuals vs fitted values 
plot(fitted(pupal_development_time_model), resid(pupal_development_time_model)) 
abline(h = 0, col = "red")


# check the normality of the residuals 
qqnorm(resid(pupal_development_time_model)) 
qqline(resid(pupal_development_time_model), col = "red")

# check the normality of the random effects
pupal_development_time_re <- ranef(pupal_development_time_model)$clutch_id[[1]]
qqnorm(pupal_development_time_re)
qqline(pupal_development_time_re, col = "red")

