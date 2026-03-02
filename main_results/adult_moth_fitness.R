#### DESCRIPTION ####

# This script analyses the data looking at the effect of temperature on winter 
# moth adult fitness. It first run a survival analysis to look at the effect of 
# temperature on adult method emergence success and then plots and models the 
# effect of temperature on female clutch size

#### SET UP ####

# packages
library(here)
library(tidyverse)
library(survival)
library(ggsurvfit)
library(glmmTMB)

# data
adult_moths <- read.csv(here("data_files", "wm_exp1.csv"))
pupal_temperature_treatments <- read.csv(here("data_files", "pupa_temperature_treatments_daily.csv"))
pupal_survival <- read.csv(here("data_files", "pupa_survival.csv"))

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


####  SURVIVAL ANALYSIS ####

# Use the survfit() function to create a survival curve using the Kaplan-Meier 
# method to compute survival probability for each temperature group
s1 <- survfit(Surv(time, status) ~ temperature, data = pupal_survival) 
summary(s1)

# make a dataframe to label the plot with the treatments at the final survival 
# probability on day 189 (after the end of the experiment) 
label_df <- data.frame(
  x = 189,
  y = c(0.78, 0.74, 0.67, 0.535, 0.51, 0.45),
  group = c("Cold", "Warm", "Hot", "Cool", "Ambient", "Mean"))

# plot the Kaplan-Meier survival analysis where S(t) = probability of not 
# emerging (survival) at time t and it is equal to S(t previous) * (ni - di)/ni 
# where: S(t previous) = survival probability at previous time step, ni = number
# of individuals at risk (not emerged) just before time t, and di = number of 
# events (number emergenced) at time t
pupal_survival_analysis <- survfit2(Surv(time, status) ~ temperature, data = pupal_survival) %>% 
  ggsurvfit(linewidth = 0.9) +
  labs(x = "Pupal Development Time (days)",
       y = "Proportion of Pupae Not Emerged",
       color = "Pupa Temperature Treatment") +
  geom_text(data = label_df,
            aes(x = x, 
                y = y, 
                label = group, 
                color = group),
            fontface = "italic",
            hjust = 0,
            size = 3.5,
            colour = "black") +
  scale_colour_manual(values = temperature_treatment_colours) +
  coord_cartesian(xlim = c(100, 200)) + 
  theme_bw() +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none")


# look at the output 
pupal_survival_analysis

# save as png file in figures folder 
ggsave(here("figures", "pupal_survival_analysis.png"), 
            plot = pupal_survival_analysis, width = 6, height = 4, dpi = 300)

# carry out a Chi Squared analysis that assumes all temperature groups have the 
# same survival distribution
survdiff(Surv(time, status) ~ temperature, data = pupal_survival)

#### PLOT ####

# filter the adult moth data for females mated within treatment and remove NAs 
# i.e., one female that didn't survive long enough to be mated and any females 
# that laid no eggs 
female_moths_clean <- adult_moths %>%
  filter(sex == "female") %>%
  filter(!adult_id %in% c("f1.cool", "f1.cold")) %>%
  drop_na(clutch_size)

# plot the mean clutch size for each treatment with the raw data 
clutch_size_plot <- ggplot(female_moths_clean, 
                           aes(x = pupa_temperature_treatment,
                               y = clutch_size, 
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
               alpha = 0.8, 
               lineend = "round") +
  geom_vline(xintercept = 5.5, 
             linetype = "dashed", 
             color = "black", 
             linewidth = 1) +
  labs(x = "Pupa Temperature Treatment",
       y = "Clutch Size (eggs)") +
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
clutch_size_plot

# save as a png file in figures folder 
ggsave(here("figures", "clutch_size_plot.png"), 
       plot = clutch_size_plot, width = 6, height = 4, dpi = 300)

#### MODEL ####

# filter to remove the control 
female_moths_clean_minus_control <- female_moths_clean %>%
  filter(pupa_temperature_treatment != "ambient")

# reorder the temperature treatments to make the mean the reference
female_moths_clean_minus_control$pupa_temperature_treatment <- factor(female_moths_clean_minus_control$pupa_temperature_treatment, 
                                                                      levels = c("mean", "cold", "cool", "warm", "hot"))


# convert clutch size from a character to an integer
female_moths_clean$clutch_size <- as.integer(female_moths_clean$clutch_size)

# assign the categorical temperature treatments a numerical values using mean 
# temperature in the 7 days before the mean development time for each treatments.
# these values are calculated in the pupal_development.R script pre modelling 
female_moths_clean_minus_control <- female_moths_clean_minus_control %>%
  mutate(pupa_temperature_treatment_numerical = case_when(
    pupa_temperature_treatment == "mean" ~ 6.24,
    pupa_temperature_treatment == "cold" ~ 0.29,
    pupa_temperature_treatment == "cool" ~ 4.37,
    pupa_temperature_treatment == "warm" ~ 8.10,
    pupa_temperature_treatment == "hot" ~ 9.23))


# check for over dispersion in clutch size
mean_clutch_size <- mean(female_moths_clean_minus_control$clutch_size)
variance_clutch_size <- var(female_moths_clean_minus_control$clutch_size)
dispersion_statistic <- variance_clutch_size / mean_clutch_size

#look at the output
dispersion_statistic 

# model the effect of temperature on clutch size, include a linear and quadratic 
# component. use glmmTMB (negative binomial) instead of glmer because of large 
# over dispersion in the count data
clutch_size_model <- glmmTMB(clutch_size ~ pupa_temperature_treatment_numerical + 
                               I(pupa_temperature_treatment_numerical^2), 
                             family = nbinom2(link = "log"),
                             data = female_moths_clean_minus_control)

# look at the output 
summary(clutch_size_model)

#### CHECK FIT ####

# compare the full model to a reduced model without the squared term 

full_model <- glmmTMB(clutch_size ~ pupa_temperature_treatment_numerical +
                        I(pupa_temperature_treatment_numerical^2),
                      family = nbinom2(link = "log"),
                      data = female_moths_clean_minus_control)
reduced_model <- glmmTMB(clutch_size ~ pupa_temperature_treatment_numerical, 
                         family = nbinom2(link = "log"), 
                         data = female_moths_clean_minus_control)

anova(full_model, reduced_model, test = "Chisq")

# compare the full model to a null model
full_model <- glmmTMB(clutch_size ~ pupa_temperature_treatment_numerical +
                        I(pupa_temperature_treatment_numerical^2),
                      family = nbinom2(link = "log"),
                      data = female_moths_clean_minus_control)
null_model <- glmmTMB(clutch_size ~ 1, 
                      family = nbinom2(link = "log"), 
                      data = female_moths_clean_minus_control)

anova(full_model, null_model, test = "Chisq")

#### CHECK ASSUMPTIONS ####

# check the assumption of equal variance by plotting the residuals vs fitted values 
plot(fitted(clutch_size_model), resid(clutch_size_model)) 
abline(h = 0, col = "red")

# check the normality of the residuals 
qqnorm(resid(clutch_size_model)) 
qqline(resid(clutch_size_model), col = "red")

