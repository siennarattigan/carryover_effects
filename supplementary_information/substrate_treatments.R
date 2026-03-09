#### DESCRIPTION ####

# This script analyses the data on the effect of six substrate treatments on 
# winter moth pupal emergence success

#### SET UP ####

# packages
library(here)
library(tidyverse)

# data
pupae <- read.csv(here("data_files", "pupa_emergence.csv"))

#### PLOT #### 

# filter the data for emergence and non-emergence 
pupal_emergence_success <- pupae %>%
  mutate(pupa_outcome_grouped = case_when(
    pupa_outcome %in% c('unemerged viable', 'unemerged not viable', 'unemerged fully developed moth') ~ 'unemerged',
    TRUE ~ pupa_outcome)) %>%
  filter(pupa_outcome_grouped %in% c('emerged', 'unemerged'))

# calculate the percentage emerged or unemerged in each substrate treatment 
emergence_success_percentages <- pupal_emergence_success %>%
  group_by(substrate_treatment, pupa_outcome_grouped) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(substrate_treatment) %>%
  mutate(percentage = count / sum(count) * 100)

# change the order of the of outcomes for plotting 
emergence_success_percentages$pupa_outcome_grouped <- factor(emergence_success_percentages$pupa_outcome_grouped, 
                                                             levels = c( "unemerged", "emerged"))


# plot pupal emergence success across substrate treatments 
emergence_success_substrate <- ggplot(emergence_success_percentages,
                                      aes(x = substrate_treatment, 
                                          y = percentage, 
                                          fill = pupa_outcome_grouped)) +
  geom_bar(stat = "identity",
           alpha = 0.9) +
  labs(x = "Pupa Substrate Treatment",
       y = "Percentage of Pupae (%)",
       fill = "Pupae Outcome") +
  scale_fill_manual(values = c("emerged" = "#75a3ce",
                               "unemerged" = "#0f3d68")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold", 
                                  size = 11),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10), 
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)))

# look at the output
emergence_success_substrate

# save as a png in the figures folder 
ggsave(here("figures", "emergence_success_substrate.png"), 
       plot = emergence_success_substrate, width = 6, height = 4, dpi = 300)

#### MODEL ####

# assign a binary outcome 
pupal_emergence_success <- pupal_emergence_success %>%
  mutate(survival_binary = case_when(
    pupa_outcome_grouped == "emerged" ~ 1,
    pupa_outcome_grouped == "unemerged" ~ 0))

# filter the data to remove the control
emergence_success_minus_control <- pupal_emergence_success %>%
  filter(pupa_temperature_treatment != "ambient") 

# model the effect of substrate treatment on pupal emergence 
emergence_success_substrate_model <- glm(survival_binary ~ substrate_treatment, 
                                         family = binomial(link = "logit"), 
                                         data = emergence_success_minus_control)

# look at the output 
summary(emergence_success_substrate_model)

#### CHECK FIT #### 

# compare the full model to a null model
full_model <- glm(survival_binary ~ substrate_treatment, family = binomial(link = "logit"), data = emergence_success_minus_control)
null_model <- glm(survival_binary ~ 1, family = binomial(link = "logit"), data = emergence_success_minus_control)

anova(full_model, null_model, test = "Chisq")

#### CHECK ASSUMPTIONS ####

# check the assumption of equal variance by plotting the residuals vs fitted values 
plot(fitted(emergence_success_substrate_model), resid(emergence_success_substrate_model)) 
abline(h = 0, col = "red")

# check the normality of the residuals 
qqnorm(resid(emergence_success_substrate_model)) 
qqline(resid(emergence_success_substrate_model), col = "red")
