#### DESCRIPTION ####

# This script analyses the data on carryover effects between winter moth 
# pupation and larval hatching

#### SET UP ####

# packages
library(here)
library(tidyverse)
library(lmerTest)
library(ggeffects)

# data
egg_subcltuches <- read.csv(here("data_files", "egg_subcltuches_with_egg_dev_time.csv"))

# convert emergence date to day (since year began)
egg_subcltuches$emergence_day <- yday(egg_subcltuches$emergence_date)

# remove the control 
egg_subcltuches_minus_control <- egg_subcltuches %>%
  filter(egg_temperature_treatment != "ambient")


# reorder the temperature treatments to make the mean the reference for modelling 
egg_subcltuches_minus_control$maternal_temperature_treatment <- factor(egg_subcltuches_minus_control$maternal_temperature_treatment, 
                                                                levels = c("mean", "cold", "cool", "warm", "hot", "ambient"))
egg_subcltuches_minus_control$egg_temperature_treatment <- factor(egg_subcltuches_minus_control$egg_temperature_treatment, 
                                                           levels = c("mean", "cold", "cool", "warm", "hot"))

# set up a common colour palette for plots
temperature_treatment_colours <- c("cold" = "#1965AE",
                                   "cool" = "#3FAB5C",
                                   "mean" = "#FFDB58",
                                   "warm" = "#E66815", 
                                   "hot" = "#DE1117", 
                                   "ambient" = "#777777") 

#### (1) Effect of maternal emergence date on larval half-hatch date ####
#### across egg temperature treatments ####

#### MODEL ####

# model the effect of emergence date on half-hatch date controlling for egg
# temperature treatment
emergence_vs_hatching_model <- lmer(half_hatch_day ~ emergence_day*egg_temperature_treatment +
                                      (1 | female_id), data = egg_subcltuches_minus_control)

#look at the output
summary(emergence_vs_hatching_model)

#### CHECK FIT ####

# compare the full model to a reduced model without interaction 

full_model <- lm(half_hatch_day ~ emergence_day*egg_temperature_treatment, data = egg_subcltuches_minus_control)
reduced_model <- lm(half_hatch_day ~ emergence_day + egg_temperature_treatment, data = egg_subcltuches_minus_control)

anova(full_model, reduced_model, test = "Chisq")

# compare the full model to a null model 

full_model <- lm(half_hatch_day ~ emergence_day*egg_temperature_treatment, data = egg_subcltuches_minus_control)
null_model <- lm(half_hatch_day ~ 1, data = egg_subcltuches_minus_control)

anova(full_model, null_model, test = "Chisq")

#### CHECK ASSUMPTIONS ####

# check the assumption of equal variance by plotting the residuals vs fitted values 

plot(fitted(emergence_vs_hatching_model), resid(emergence_vs_hatching_model)) 
abline(h = 0, col = "red")

# Check the normality of the residuals 

qqnorm(resid(emergence_vs_hatching_model)) 
qqline(resid(emergence_vs_hatching_model), col = "red")

# check the normality of the random effects
emergence_vs_hatching_re <- ranef(emergence_vs_hatching_model)$female_id[[1]]
qqnorm(emergence_vs_hatching_re)
qqline(emergence_vs_hatching_re, col = "red")

#### PLOT ####

# calculate model prediction and save to data frame 
emergence_vs_hatching_prediction <- as.data.frame(predict_response(emergence_vs_hatching_model, 
                                                                   terms = c("emergence_day", "egg_temperature_treatment")))
# change the order of the temperature treatments
emergence_vs_hatching_prediction$group <- factor(emergence_vs_hatching_prediction$group, 
                                                 levels = c("cold", "cool", "mean", "warm", "hot"))

# plot the model prediction with confidence intervals and data points 
emergence_vs_hatching <- ggplot() +
  geom_line(data = emergence_vs_hatching_prediction, 
            aes(x = x, 
                y = predicted, 
                colour = group), 
            linewidth = 1.2) +
  geom_ribbon(data = emergence_vs_hatching_prediction, 
              aes(x = x, 
                  ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), 
              alpha = 0.2) +
  geom_jitter(data = egg_subcltuches_minus_control,
              aes(x = emergence_day, 
                  y = half_hatch_day, 
                  colour = egg_temperature_treatment), 
              width = 0.2, 
              alpha = 0.7, 
              size = 2.5, 
              stroke = 0) +
  labs(x = "Maternal Emergence Day (2024)",
       y = "Larval Half-hatch Day (2025)", 
       color = "Egg Temperature Treatment",
       fill = "Egg Temperature Treatment") +
  scale_colour_manual(values = temperature_treatment_colours, 
                      labels = c("Cold", "Cool", "Mean", "Warm", "Hot")) + 
  scale_fill_manual(values = temperature_treatment_colours, 
                    labels = c("Cold", "Cool", "Mean", "Warm", "Hot")) +
  theme_bw() +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.position = "top",
        legend.justification = "center",
        plot.margin = margin(t = 10, r = 40, b = 10, l = 10))

emergence_vs_hatching

ggsave(here("figures", "emergence_vs_hatching.png"), 
            plot = emergence_vs_hatching, width = 6, height = 4, dpi = 300)

#### (2) Effect of maternal emergence date on egg development time ####
#### across egg temperature treatments ####

#### MODEL ####

# model the effect of emergence date on egg development controlling for egg
# temperature treatment 
emergence_vs_egg_dev_model <- lmer(egg_development_time ~ emergence_day*egg_temperature_treatment + 
                                     (1 | female_id), data = exp2_dev_minus_control)

# look at the output
summary(emergence_vs_egg_dev_model)

#### CHECK FIT ####

# compare full model to a reduced model without interaction 
full_model <- lm(egg_development_time ~ emergence_day*egg_temperature_treatment, data = exp2_dev_minus_control)
reduced_model <- lm(egg_development_time ~ emergence_day + egg_temperature_treatment, data = exp2_dev_minus_control)

anova(full_model, reduced_model, test = "Chisq")

# compare full model to a null model 
full_model <- lm(egg_development_time ~ emergence_day*egg_temperature_treatment, data = exp2_dev_minus_control)
null_model <- lm(egg_development_time ~ 1, data = exp2_dev_minus_control)

anova(full_model, null_model, test = "Chisq")

#### CHECK ASSUMPTIONS ####

# check the assumption of equal variance by plotting the residuals vs fitted values 
plot(fitted(emergence_vs_egg_dev_model), resid(emergence_vs_egg_dev_model)) 
abline(h = 0, col = "red")

#check the normality of the residuals 
qqnorm(resid(emergence_vs_egg_dev_model)) 
qqline(resid(emergence_vs_egg_dev_model), col = "red")

# check the normality of the random effects
emergence_vs_egg_dev_re <- ranef(emergence_vs_egg_dev_model)$female_id[[1]]
qqnorm(emergence_vs_egg_dev_re)
qqline(emergence_vs_egg_dev_re, col = "red")

#### PLOT ####

# calculate model prediction and save to data frame
emergence_vs_egg_dev_prediction <- as.data.frame(predict_response(emergence_vs_egg_dev_model, 
                                                                  terms = c("emergence_day", "egg_temperature_treatment")))

# change the order of the temperature treatments
emergence_vs_egg_dev_prediction$group <- factor(emergence_vs_egg_dev_prediction$group, 
                                                levels = c("cold", "cool", "mean", "warm", "hot"))

# plot the model prediction with confidence intervals and data points
emergence_vs_egg_dev <- ggplot() +
  geom_line(data = emergence_vs_egg_dev_prediction, 
            aes(x = x, 
                y = predicted, 
                colour = group), 
            linewidth = 1.2) +
  geom_ribbon(data = emergence_vs_egg_dev_prediction, 
              aes(x = x, 
                  ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), 
              alpha = 0.2) +
  geom_jitter(data = exp2_dev_minus_control,
              aes(x = emergence_day, 
                  y = egg_development_time, 
                  colour = egg_temperature_treatment),
              width = 0.2, 
              alpha = 0.7, 
              size = 2.5, 
              stroke = 0) +
  labs(x = "Maternal Emergence Day (2024)",
       y = "Egg Development Time (days)", 
       color = "Egg Temperature Treatment",
       fill = "Egg Temperature Treatment") +
  scale_colour_manual(values = temperature_treatment_colours, 
                      labels = c("Cold", "Cool", "Mean", "Warm", "Hot")) + 
  scale_fill_manual(values = temperature_treatment_colours, 
                    labels = c("Cold", "Cool", "Mean", "Warm", "Hot")) +
  theme_bw() +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.position = "top",
        legend.justification = "center",
        plot.margin = margin(t = 10, r = 40, b = 10, l = 10))

# look at the output
emergence_vs_egg_dev

# save as a png in the figures folder
ggsave(here("figures", "emergence_vs_egg_dev.png"), 
       plot = emergence_vs_egg_dev, width = 6, height = 4, dpi = 300)
