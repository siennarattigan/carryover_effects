#### DESCRIPTION ####

# This script analyses the data look at the effect of temperature on winter moth
# egg development 

#### SET UP ####

# packages
library(here)
library(tidyverse)
library(lmerTest)

# data
egg_subcltuches <- read.csv(here("data_files", "egg_subclutches.csv"))
larval_hatching <- read.csv(here("data_files", "egg_hatching_raw_NAs.csv"))
egg_temperature_treatments <- read.csv(here("data_files", "egg_temperature_treatments_daily.csv"))

# reorder temperature treatments for plots
egg_subcltuches$egg_temperature_treatment <- factor(egg_subcltuches$egg_temperature_treatment,
                                                 levels = c("cold", "cool", "mean", "warm", "hot", "ambient"))


# set up a common colour palette for plots
temperature_treatment_colours <- c("cold" = "#1965AE",
                                   "cool" = "#3FAB5C",
                                   "mean" = "#FFDB58",
                                   "warm" = "#E66815", 
                                   "hot" = "#DE1117", 
                                   "ambient" = "#777777") 

#### CALCULATE HALF-HATCH DATE ####

# transform date and larvae number to numerical format, remove the duplicate 
# subclutch and select only the rows needed
larval_hatching_transformed <- larval_hatching %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  mutate(day = dense_rank(date)) %>%
  filter(!is.na(number_larvae)) %>%
  mutate(number_larvae = as.integer(number_larvae)) %>%
  filter(subclutch_id != "f3.control.hot.2") %>%
  select(day, subclutch_id, number_larvae)

# find the total positive increase in larvae hatching 
larval_hatching_processed <- larval_hatching_transformed %>%
  arrange(subclutch_id, day) %>%
  group_by(subclutch_id) %>%
  mutate(change_larvae = number_larvae - lag(number_larvae)) %>%
  mutate(change_larvae = ifelse(change_larvae < 0 | is.na(change_larvae), 0, change_larvae)) %>%
  mutate(cum_larvae = cumsum(change_larvae)) %>%
  mutate(max_cum_larvae = max(cum_larvae)) %>%
  ungroup()

# calculate the proportion of larvae hatched each day in each subclutch
larval_hatching_proportions <- larval_hatching_processed %>%
  mutate(proportion_hatched = cum_larvae / max_cum_larvae) 


## subclutches with >2 larvae hatched = large

# filter the processed data for (large) subclutches with great than two larvae hatched (enough for a logistic curve)
larval_hatching_proportions_large <- larval_hatching_proportions %>%
  filter(max_cum_larvae > 2) %>%
  select(subclutch_id, day, proportion_hatched)

# calculate the median hatching day to find a good initial guess for the inflection point
median(larval_hatching_proportions_large$day)

# make a function to fit logistic curve per subclutch and extract parameters
# extract date of half-hatch and curve metrics for each tree
process_curve_dataset <- function(data) {
  #define the get_curve function which will be applied to each subclutch individually 
  get_curve <- function(rn) {
    #print the current subclutch being processed 
    print(as.character(rn$subclutch_id[1]))
    #initial guesses for the logistic model 
    a <- 0.1 #slope
    b <- 35  #inflection point when 50% hatched
    d <- 0   #lower asymptote 
    
    params <- list(a = a, b = b, d = d)
    while(TRUE) {
      params <- list(a = a, b = b, d = d)
      fit <- NULL
      try(fit <- nls(proportion_hatched ~ I(d + (1 - d) * 1/(1 + exp(-a * (day - b)))), #fit logistic growth curve using nonlinear least squares 
                     data = rn, start = params, trace = F, 
                     control = list(maxiter = 500), 
                     lower = list(a = 0, b = 0, d = 0), 
                     upper = list(a = 3, b = 160, d = 1), algorithm = "port"))
      
      if(!is.null(fit)) break;
      
      #resample params for new fit attempts
      a <- sample(seq(0.1, 2, 0.05), 1)
      b <- sample(seq(1, 50, 1), 1)
      d <- sample(seq(0, 0.6, 0.05), 1)
    }
    
    #get the half-hatch date (the x0 point where the proportion hatched is 0.5)
    #create smooth day sequence to evaluate the model over 
    s <- seq(min(rn$day), max(rn$day), length = 1000) 
    #define half-hatch date
    half <- s[which.min(abs(predict(fit, list(day = s)) - 0.5))] 
    
    #extract the coefficients from the fit
    coefs <- coef(fit)
    a <- coefs[1]
    b <- coefs[2]
    d <- coefs[3]
    
    #calculate slope at the half-hatch date using the logistic derivative formula
    half_slope <- (a * (1 - d)) / 4
    
    #return the values for the subclutch being evaluated
    return(list(fit, half, half_slope, as.character(rn$subclutch_id[1]), coefs[1], coefs[2], coefs[3]))
  }
  
  #apply the get_curve function to each subclutch ID
  rn_out <- lapply(by(data, data$subclutch_id, get_curve), identity)
  
  #combine the results into a matrix
  rn_out_matrix <- do.call(rbind, rn_out)
  
  #convert the matrix to a dataframe
  rn_hatching <- as.data.frame(rn_out_matrix)
  
  #separate the list columns into individual columns
  rn_hatching$fit <- lapply(rn_hatching$V1, as.vector)
  rn_hatching$halfdate <- unlist(rn_hatching$V2)
  rn_hatching$halfslope <- unlist(rn_hatching$V3)
  rn_hatching$subclutch_id <- unlist(rn_hatching$V4)
  rn_hatching$intercept <- unlist(rn_hatching$V5)
  rn_hatching$maxdate <- unlist(rn_hatching$V6)
  rn_hatching$maxslope <- unlist(rn_hatching$V7)
  
  #select and reorder columns needed
  rn_hatching <- rn_hatching %>%
    dplyr::select(subclutch_id, halfdate_large = halfdate, fit)
  
  return(rn_hatching)
}

# use the function on the larval hatching data
logistic_results <- process_curve_dataset(larval_hatching_proportions_large) 


## subclutches with <2 larvae hatched = small 

# filter the processed data for (smaller) subclutches with 1 or 2 larvae hatched 
larval_hatching_proportions_small<- larval_hatching_proportions %>%
  filter(max_cum_larvae < 3 & max_cum_larvae > 0) %>%
  select(subclutch_id, day, proportion_hatched)

# calculate halfdate as the day that proportion hatched equals or exceeds 50%
estimate_results <- larval_hatching_proportions_small %>%
  group_by(subclutch_id) %>%
  filter(proportion_hatched >= 0.5) %>%
  arrange(day) %>%
  slice(1) %>%
  ungroup() %>%
  select(subclutch_id, halfdate_small = day)

# match the half-hatch date estimates (for large and small) subcltuches to 
# larval hatching data from experiment two 
egg_subcltuches <- egg_subcltuches %>%
  #for subclutches with >2 larvae
  left_join(logistic_results, by = "subclutch_id") %>%
  #for subclutches with <=2 larvae
  left_join(estimate_results, by = "subclutch_id") %>% 
  mutate(halfdate = coalesce(halfdate_large, halfdate_small)) %>%
  select(-halfdate_large, -halfdate_small)


# filter the data to remove subclutches from females that were mated twice or 
# mated with a male in a different treatment
egg_subcltuches_clean <- egg_subcltuches %>%
  filter(!(female_id %in% c("f3.mean", "f7.control", "f1.cold", "f1.cool")))

# transform half-hatch date into a real date 
start_date <- as.Date("2025-03-01")
egg_subcltuches_clean$half_hatch_date <- start_date + (egg_subcltuches_clean$halfdate - 1)

# convert half-hatch date to a day (since year began)
egg_subcltuches_clean$half_hatch_day <- yday(egg_subcltuches_clean$half_hatch_date)

# save half-hatch dates to new object 
egg_subcltuches_clean_hatch <- egg_subcltuches_clean %>%
  select(-fit)

#### PLOT ####

# calculate egg development time as the number of days between first egg and half hatch
egg_subcltuches_clean_hatch$first_egg_date <- ymd(egg_subcltuches_clean_hatch$first_egg_date)
egg_subcltuches_clean_hatch$egg_development_time <- as.numeric(egg_subcltuches_clean_hatch$half_hatch_date - egg_subcltuches_clean_hatch$first_egg_date)

# save to a csv file in the data_files folder
write.csv(egg_subcltuches_clean_hatch, 
          here("data_files", "egg_subcltuches_with_egg_dev_time.csv"), row.names = FALSE)

# plot the mean egg development time for each treatment with the raw data 
egg_development_time_plot <- ggplot(egg_subcltuches_clean_hatch, 
                                    aes(x = egg_temperature_treatment, 
                                        y = egg_development_time, 
                                        fill = egg_temperature_treatment,
                                        colour = egg_temperature_treatment)) +
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
  labs(x = "Egg Temperature Treatment",
       y = "Egg Development Time (days)") +
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
egg_development_time_plot

# save as a png in the figures folder
ggsave(here("figures", "egg_development_time_plot.png"),
       plot = egg_development_time_plot, width = 6, height = 4, dpi = 300)

#### MODEL ####

# filter to remove the control 
egg_subcltuches_clean_hatch_minus_control <- egg_subcltuches_clean_hatch %>%
  filter(egg_temperature_treatment != "ambient")

# reorder the temperature treatment so that the mean is the reference
egg_subcltuches_clean_hatch_minus_control$egg_temperature_treatment <- factor(egg_subcltuches_clean_hatch_minus_control$egg_temperature_treatment, levels = c("mean", "cold", "cool", "warm", "hot"))

# convert egg development time to a numeric variable
egg_subcltuches_clean_hatch <- egg_subcltuches_clean_hatch %>%
  mutate(egg_development_time = as.numeric(as.character(egg_development_time)))

# subset the temperature data
egg_temperature_treatments <- egg_temperature_treatments %>%
  subset(select = c(cold, cool, mean, warm, hot, ambient)) %>%
  drop_na()

# calculate the mean development time for each treatment
mean_egg_development_times <-  egg_subcltuches_clean_hatch %>%
  group_by(egg_temperature_treatment) %>%
  summarise(mean_development_time = mean(egg_development_time))

# look at the output 
print(mean_egg_development_times)

# create two objects, one with the temperature treatments and the other with the 
# mean egg development times
treatment_names <- c("cold", "cool", "mean", "warm", "hot", "ambient")
mean_egg_development_time <- c(161, 153, 141, 125, 114, 122)

# calculate the mean temperature in the 7 days before the mean development time
# for each temperature treatment
seven_day_mean_temperatures <- mapply(function(treatment, days) {
  start_day <- days - 6
  mean(egg_temperature_treatments[[treatment]][start_day:days], na.rm = TRUE)
}, treatment_names, mean_egg_development_time)

# look at the output 
print(seven_day_mean_temperatures)

# assign the categorical temperature treatments a numerical values using mean 
# temperature in the 7 days before the mean development time for each treatment
egg_subcltuches_clean_hatch_minus_control <- egg_subcltuches_clean_hatch_minus_control %>%
  mutate(egg_temperature_treatment_numerical = case_when(
    egg_temperature_treatment == "mean" ~ 11.62,
    egg_temperature_treatment == "cold" ~ 10.88,
    egg_temperature_treatment == "cool" ~ 11.07,
    egg_temperature_treatment == "warm" ~ 11.49,
    egg_temperature_treatment == "hot" ~ 11.77))

# model the effect of temperature treatment on egg development time, include female ID as a random effect 
egg_development_time_model <- lmer(egg_development_time ~ egg_temperature_treatment + 
                                     (1 | female_id), data = egg_subcltuches_clean_hatch_minus_control)

# look at the output
summary(egg_development_time_model)


## // check whether egg temperature  treatment should be continuous or categorical 


#### CHECK FIT ####

# compare the full model to a null model 

full_model <- lm(egg_development_time ~ egg_temperature_treatment, data = egg_subcltuches_clean_hatch_minus_control)
null_model <- lm(egg_development_time ~ 1, data = egg_subcltuches_clean_hatch_minus_control)

anova(full_model, null_model, test = "Chisq")

#### CHECK ASSUMPTIONS ####

# check the assumption of equal variance by plotting the residuals vs fitted values 
plot(fitted(egg_development_time_model), resid(egg_development_time_model)) 
abline(h = 0, col = "red")

# check the normality of the residuals 
qqnorm(resid(egg_development_time_model)) 
qqline(resid(egg_development_time_model), col = "red")

# check the normality of the random effects
egg_development_time_re <- ranef(egg_development_time_model)$female_id[[1]]
qqnorm(egg_development_time_re)
qqline(egg_development_time_re, col = "red")
