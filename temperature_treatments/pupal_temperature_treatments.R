#### DESCRIPTION ####

# This script cleans the pupal temperature treatment data and then plots (i) the
# experimental treatments and (ii) the mean and ambient treatments 

#### SET UP ####

# packages
library(here)
library(tidyverse)

# data
pupa_temperature_treatments <- read.csv(here("data_files", "pupa_temperature_treatments_daily.csv"))

# set up a common colour palette for plots
temperature_treatment_colours <- c("cold" = "#1965AE",
                                   "cool" = "#3FAB5C",
                                   "mean" = "#FFDB58",
                                   "warm" = "#E66815", 
                                   "hot" = "#DE1117", 
                                   "ambient" = "#777777") 

#### CLEAN #### 

# subset the temperature data to select the date, rounded columns and rename them 
temperatures <- pupa_temperature_treatments %>%
  subset(select = c(date,
                    ambient_rounded, 
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
  drop_na

# change to long format 
temperatures_long <- temperatures %>%
  pivot_longer(cols = -date,
               names_to = "treatment",
               values_to = "temperature")

# reorder the temperature treatments for plotting 
temperatures_long$treatment <- factor(temperatures_long$treatment, 
                                      levels = c("hot", "warm", "mean", "cool", "cold", "ambient"))

# convert data from a character to a date 
temperatures_long$date <- as.Date(temperatures_long$date, format = "%d/%m/%Y")
 
#### PLOT ####

# filter for the experimental treatments 
experimental_temperatures <- temperatures_long %>%
  filter(treatment %in% c("cold", "cool", "mean", "warm", "hot"))

# make a data frame to label the treatments 
label_df <- data.frame(
  x = as.Date("2025-01-10"),
  y = c(8.5, 6.75, 4, 1.5, -0.25),
  group = c("Hot", "Warm", "Mean", "Cool", "Cold"))

# plot the experimental treatments as individual lines
pupa_temperature_treatments_experimental <- ggplot(experimental_temperatures, 
                                                   aes(x = date, 
                                                       y = temperature, 
                                                       colour = treatment)) + 
  geom_line(linewidth = 1) + 
  labs(x = "Date",
       y = "Temperature (°C)",
       color = "Treatment") +
  geom_text(data = label_df,
            aes(x = x, 
                y = y, 
                label = group, 
                color = group),
            fontface = "italic",
            hjust = 0,
            size = 3.5,
            colour = "black") +
  expand_limits(x = as.Date("2025-01-17")) +
  scale_colour_manual(values = temperature_treatment_colours) +
  scale_x_date(breaks = seq(as.Date("2024-07-04"),
                            as.Date("2025-01-08"), 
                            by = "1 month"),
               date_labels = "%b %y",
               limits = c(as.Date("2024-07-04"),
                          as.Date("2025-01-17"))) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold",
                                  size = 11),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none")  

pupa_temperature_treatments_experimental

ggsave(here("figures", "pupa_temperature_treatments_experimental.png"), 
            plot = pupa_temperature_treatments_experimental, width = 6, height = 4, dpi = 300)

#### MEAN vs AMBIENT ####

# filter for the mean and ambient treatments 
mean_and_ambient_temperatures <- temperatures_long %>%
  filter(treatment %in% c("mean", "ambient"))

# change the order of the treatments for plotting 
mean_and_ambient_temperatures$treatment <- factor(mean_and_ambient_temperatures$treatment, levels = c("ambient", "mean"))

# plot the mean and ambient treatments as individual lines 
pupa_temperature_treatments_mean_ambient <- ggplot(mean_and_ambient_temperatures, 
                                                   aes(x = date, 
                                                       y = temperature, 
                                                       colour = treatment)) + 
  geom_line(linewidth = 0.7) + 
  labs(title = "(a) Pupal development",
       x = "Date",
       y = "Temperature (°C)",
       color = "Treatment") +
  scale_colour_manual(values = c("ambient" = "#777777",
                                 "mean" = "#FFDB58")) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %y") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold", 
                                  size = 11),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 11.5),
        legend.title = element_text(size = 11.5),
        legend.position = "top")  

# check it looks as expected
pupa_temperature_treatments_mean_ambient

# save as a csv file in the figures folder
ggsave(here("figures", "pupa_temperature_treatments_mean_ambient.png"), 
            plot = pupa_temperature_treatments_mean_ambient, width = 6, height = 4, dpi = 300)
