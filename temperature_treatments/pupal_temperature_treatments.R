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
temperature_treatment_colours <- c("cold" = "#2166AC",
                                   "cool" = "#63BFB4",
                                   "mean" = "#FDDC7A",
                                   "warm" = "#EF8A62", 
                                   "hot" = "#B2182B",
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
label_df_pupae <- data.frame(
  x = as.Date("2025-01-10"),
  y = c(8.5, 6.75, 4, 1.5, -0.25),
  group = c("Hot", "Warm", "Mean", "Cool", "Cold"))

# plot the experimental treatments as individual lines
pupa_temperature_treatments_experimental_plot <- ggplot(experimental_temperatures, 
                                                   aes(x = date, 
                                                       y = temperature, 
                                                       colour = treatment)) + 
  geom_line(linewidth = 0.7) + 
  labs(title = "(a) Pupae and emerged adults",
       x = "Date",
       y = "Temperature (°C)",
       color = "Treatment") +
  geom_text(data = label_df_pupae,
            aes(x = x, 
                y = y, 
                label = group, 
                color = group),
            fontface = "italic",
            hjust = 0,
            size = 3,
            colour = "black") +
  expand_limits(x = as.Date("2025-01-17")) +
  scale_colour_manual(values = temperature_treatment_colours) +
  scale_x_date(breaks = seq(as.Date("2024-07-04"),
                            as.Date("2025-01-08"), 
                            by = "1 month"),
               date_labels = "%b %y",
               limits = c(as.Date("2024-07-04"),
                          as.Date("2025-02-07"))) +
  scale_y_continuous(limits = c(-5,25)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold",
                                  size = 11),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")  

# look at the output
pupa_temperature_treatments_experimental_plot

#### MEAN vs AMBIENT ####

# filter for the mean and ambient treatments 
mean_and_ambient_temperatures_pupae <- temperatures_long %>%
  filter(treatment %in% c("mean", "ambient"))

# change the order of the treatments for plotting 
mean_and_ambient_temperatures_pupae$treatment <- factor(mean_and_ambient_temperatures_pupae$treatment, levels = c("ambient", "mean"))

# plot the mean and ambient treatments as individual lines 
pupa_temperature_treatments_mean_ambient_plot <- ggplot(mean_and_ambient_temperatures_pupae, 
                                                   aes(x = date, 
                                                       y = temperature, 
                                                       colour = treatment)) + 
  geom_line(linewidth = 0.6) + 
  labs(title = "(a) Pupal development",
       x = "Date",
       y = "Temperature (°C)",
       color = "Treatment") +
  scale_colour_manual(values = c("ambient" = "#777777",
                                 "mean" = "#FDDC7A")) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b %y") +
  scale_y_continuous(limits = c(-5, 25)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold", 
                                  size = 11),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "top")  

# check it looks as expected
pupa_temperature_treatments_mean_ambient_plot

# save as a csv file in the figures folder
ggsave(here("figures", "pupa_temperature_treatments_mean_ambient_plot.png"), 
            plot = pupa_temperature_treatments_mean_ambient_plot, width = 6, height = 4, dpi = 300)
