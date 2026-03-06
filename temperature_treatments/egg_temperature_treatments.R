#### DESCRIPTION ####

# this script creates the egg temperature treatmentd from historical temperature
# data and then plots (i) the
# experimental treatments and (ii) the mean and ambient treatments 

#### SET UP ####

# packages
library(here)
library(tidyverse)
library(lubridate)
library(chillR)

# data
histtemp <- read.csv(here("data_files", "daily_temp_minmax_1960_to_2016.csv"))

# set up a common colour palette for plots
temperature_treatment_colours <- c("cold" = "#1965AE",
                                   "cool" = "#3FAB5C",
                                   "mean" = "#FFDB58",
                                   "warm" = "#E66815", 
                                   "hot" = "#DE1117", 
                                   "ambient" = "#777777")

#### CLEAN ####

# rename the columns in the historical temperature data 
histtemp$date <- dmy(histtemp$date)
histtemp$Year <- year(histtemp$date)
histtemp$JDay <- yday(histtemp$date)
histtemp$Day <- day(histtemp$date)
histtemp$Month <- month(histtemp$date)

# filter for years 1966-2016
histtemp <- histtemp %>%
  filter(Year>1965)

# calculate daily max and min
historicmean_minmax <- histtemp %>%
  group_by(Day) %>%
  summarise(tmin_mean = mean(Tmin), tmax_mean = mean(Tmax))

# get day length for all calendar days for Wytham latitude
Days <- daylength(latitude = 51.7734, JDay = 1:365)

Days_df <-
  data.frame(
    JDay = 1:365,
    Sunrise = Days$Sunrise,
    Sunset = Days$Sunset,
    Daylength = Days$Daylength
  )

# get monthly average sunrise/sunset/daylength

monthly_df <- Days_df %>% 
  mutate(date = as_date(paste("2025", JDay), format="%Y %j")) %>%
  mutate(month = format(date,"%m")) %>% 
  group_by(month) %>%
  summarise(m.sunrise = mean(Sunrise),m.sunset=mean(Sunset),
            m.daylength = mean(Daylength))

# put date in sensible format
days_df <- Days_df %>% 
  mutate(date = as_date(paste("2025", JDay), format="%Y %j")) %>%
  mutate(month=format(date,"%m"))

# change to long format
daylength_df <- pivot_longer(days_df,cols = c(Sunrise:Daylength))

# plot daylength, sunrise and sunset 
ggplot(daylength_df, aes(JDay, value)) +
  geom_line(lwd = 1.5) +
  facet_grid(cols = vars(name)) +
  ylab("time of Day / Daylength (Hours)") +
  theme_bw(base_size = 20)

# put data in format package wants 
histtemp_formatted <- histtemp %>% 
  select(Year, Month, Day, Tmin, Tmax)

# remove leap day (February 29)
histtemp_formatted <- histtemp_formatted[!(histtemp_formatted$Month == 2 & histtemp_formatted$Day == 60), ]

#### 2-HOURLY ####

# get hourly temperature data from curve/photoperiod calculations
historicalhourlytemps <- data.frame(stack_hourly_temps(histtemp_formatted,latitude=51.7734)) #weird prefix
historicalhourlytemps <- historicalhourlytemps %>%
  rename_with(~str_remove(., 'hourtemps.')) #remove weird prefix

# calculate the hourly average of each calendar day from historical data
avghourlytemps <- historicalhourlytemps %>% 
  filter(!is.na(Temp)) %>%
  group_by(Month,Day,JDay, Hour) %>%
  summarise(AvgTemp = mean(Temp))

# pair hours in twos so 1/2, 3/4, 5/6, etc.
avg2hourlytemps <- avghourlytemps %>%
  mutate(TwoHourGroup = (Hour %/% 2)) %>%  # Group hours into 0-1, 2-3, ..., 22-23
  group_by(Month, Day, TwoHourGroup) %>%
  summarise(AvgTemp = mean(AvgTemp, na.rm = TRUE), .groups = 'drop') %>%
  mutate(HourRange = paste0((TwoHourGroup * 2), "_", (TwoHourGroup * 2 + 1))) %>%
  select(Month, Day, HourRange, AvgTemp, TwoHourGroup)%>%
  mutate(
    date = make_date(year = 2025, month = Month, day = Day),
    jday = yday(date))

# calculate monthly standard deviation from the mean
monthlysd <- histtemp %>% group_by(Year, Month) %>% 
  summarise(mean_meantemp=mean(Tmean)) %>%
  ungroup()%>% group_by(Month) %>%
  summarise(monthlystdev=sd(mean_meantemp)) %>%
  mutate(plus2.5stddev = 2.5*monthlystdev,plus1.5stddev = 1.5*monthlystdev,
         minus1.5stddev = -1.5*monthlystdev,minus2.5stddev = -2.5*monthlystdev)
#will be adding these as constants to the calculations

# merge to hourly data frame
climchamber_df <- avg2hourlytemps %>%
  left_join(monthlysd, by = "Month") %>%
  mutate(
    plus1.5 = AvgTemp + plus1.5stddev,
    plus2.5 = AvgTemp + plus2.5stddev,
    minus1.5 = AvgTemp + minus1.5stddev,
    minus2.5 = AvgTemp + minus2.5stddev) %>%
  rename(actual.mean = AvgTemp) %>%
  mutate(Date = as.Date(paste(2025, Month, Day, sep = "-"), format = "%Y-%m-%d")) %>%
  mutate(JulianDay = yday(Date)) %>%
  select(Month,Day,Date,JulianDay,HourRange,plus2.5,plus1.5,actual.mean,minus1.5,minus2.5)

# make a julian day column and convert to long
climchamber_long <- climchamber_df %>%
  pivot_longer(cols = plus2.5:minus2.5,names_to = "treatment",
               values_to = "temperature")

# make two functions to group the temperature data into two-day intervals and 
# then calculate the temperature within those groups
group_avg_temp <- function(data, days_group) {
  # add group information
  data <- data %>%
    filter(!(Month == 2 & Day == 29)) %>%
    mutate(
      group_num = ceiling(jday / days_group),
      day_range = paste0(
        ((group_num - 1) * days_group + 1), 
        "_", 
        (group_num * days_group)))
  # group by and calculate mean
  result <- data %>%
    group_by(group_num, day_range, TwoHourGroup) %>%
    summarize(AvgTemp = mean(AvgTemp, na.rm = TRUE), .groups = 'drop') %>%
    rowwise() %>%
    # expand day ranges into individual jdays
    mutate(jdays = list(seq(
      as.numeric(strsplit(day_range, "_")[[1]][1]),
      as.numeric(strsplit(day_range, "_")[[1]][2])))) %>%
    unnest_longer(jdays) %>%
    ungroup() %>%
    rename(jday = jdays) %>%
    arrange(jday,TwoHourGroup) %>%
    mutate(timestep = row_number()) %>%
    filter(jday <= 365)
  return(result)
}

# apply the function 
twoday <- group_avg_temp(avg2hourlytemps, days_group = 2)

# calculate the temperature treatments for the climate chambers 
two_day_temperature_treatments <- twoday %>%
  mutate(Date = make_date(2025, 1, 1) + days(jday - 1),
         Month = month(Date)) %>%
  left_join(monthlysd, by = "Month") %>%
  mutate(
    plus1.5 = AvgTemp + plus1.5stddev,
    plus2.5 = AvgTemp + plus2.5stddev,
    minus1.5 = AvgTemp + minus1.5stddev,
    minus2.5 = AvgTemp + minus2.5stddev) %>%
  rename(actual.mean = AvgTemp) %>%
  select(group_num,Date,jday,TwoHourGroup,plus2.5,plus1.5,actual.mean,minus1.5,minus2.5)

# convert data to long format 
two_day_temperature_treatments_long <- two_day_temperature_treatments %>%
  pivot_longer(cols = plus2.5:minus2.5,names_to="treatment",
               values_to = "temperature")

# change the column names to the temperature treatments 
two_day_temperature_treatments_long <- two_day_temperature_treatments_long %>%
  mutate(treatment = case_when(
    treatment == "actual.mean" ~ "mean",
    treatment == "minus2.5" ~ "cold",
    treatment == "minus1.5" ~ "cool", 
    treatment == "plus1.5" ~ "warm", 
    treatment == "plus2.5" ~ "hot"))

# reorder temperature treatments 
two_day_temperature_treatments_long$treatment <- factor(two_day_temperature_treatments_long$treatment, levels = c ("hot", "warm", "mean", "cool", "cold"))

# convert date from a character to a date object 
two_day_temperature_treatments_long$Date <- as.Date(two_day_temperature_treatments_long$Date)
# filter the data for dates where eggs are in climate chambers
two_day_temperature_treatments_filtered <- two_day_temperature_treatments_long %>%
  filter(Date >= as.Date("2025-01-08") & Date <= as.Date("2025-05-10"))

# save to a csv file in the data files folder
write.csv(two_day_temperature_treatments_filtered, here("data_files", "egg_temperature_treatments_hourly.csv"), row.names = FALSE)

#### DAILY #### 

# condense two hourly temperatures into daily means 
daily_mean_temperature_treatments <- two_day_temperature_treatments_filtered %>%
  group_by(group_num, Date, jday, treatment) %>%
  summarise(temperature = mean(temperature)) %>%
  ungroup()

#### PLOT #### 

# plot the two-hour interval temperature treatments
egg_temperature_treatments_hourly <- ggplot(two_day_temperature_treatments_filtered, 
                                            aes(x = Date, y = temperature, colour = treatment)) +
  geom_point(size = 0.25) +
  labs(x = "Date",
       y = "temperature (°C)") + 
  scale_color_manual(values = temperature_treatment_colours) +
  scale_x_date(breaks = seq(as.Date("2025-01-08"),
                            as.Date("2025-05-10"), 
                            by = "1 month"),
               date_labels = "%b %y",
               limits = c(as.Date("2025-01-08"),
                          as.Date("2025-05-10"))) +
  theme_bw() +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none") 

# look at the output 
egg_temperature_treatments_hourly

# save as a csv file in the figures folder
ggsave(here("figures", "egg_temperature_treatments_hourly.png"), 
       plot = egg_temperature_treatments_hourly, width = 4, height = 3, dpi = 300)

# make a data frame to label the treatments
label_df <- data.frame(
  x = as.Date("2025-05-11"),
  y = c(14, 12.75, 11.25, 9.75, 8.5),
  group = c("Hot", "Warm", "Mean", "Cool", "Cold"))

# plot the daily mean temperature treatments 
egg_temperature_treatments_daily_experimental <- ggplot(daily_mean_temperature_treatments, 
                                                        aes(x = Date, y = temperature, colour = treatment)) + 
  geom_line(linewidth = 1) + 
  labs(x = "Date",
       y = "Temperature (°C)") +
  geom_text(data = label_df,
            aes(x = x,
                y = y, 
                label = group, 
                colour = group),
            fontface = "italic",
            hjust = 0,
            size = 3.5,
            colour = "black") +
  scale_colour_manual(values = temperature_treatment_colours) +
  scale_x_date(breaks = seq(as.Date("2025-01-08"),
                            as.Date("2025-05-10"), 
                            by = "1 month"),
               date_labels = "%b %y",
               limits = c(as.Date("2025-01-08"),
                          as.Date("2025-05-15"))) + 
  scale_y_continuous(limits = c(-5, 15)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none")

# look at the output 
egg_temperature_treatments_daily_experimental

# save as a png file in the figures folder
ggsave(here("figures", "egg_temperature_treatments_daily_experimental.png"), 
       plot = egg_temperature_treatments_daily_experimental, width = 6, height = 4, dpi = 300)

