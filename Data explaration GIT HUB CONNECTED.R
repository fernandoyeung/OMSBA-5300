library(tidyverse)
library(dplyr)
library(rio)
library(lubridate)

# Aggregating Google Tends data

trends_files <- list.files( path = "C:/Users/ferna/OneDrive/Documents/OMSBA 5300/Data explaration/OMSBA-5300/Lab3_Rawdata",
                            pattern = "^trends_up_to.*\\.csv$",
                            full.names = TRUE)

#Bind the files into a single data set
google_trends_data <- import_list(trends_files, rbind = TRUE)

# Extract first ten characters from monthorweek

google_trends_data$monthorweek <- as.Date(ymd(str_sub(google_trends_data$monthorweek, 1, 10)))

# Aggregate to months using floor_date
google_trends_data$month <- floor_date(google_trends_data$monthorweek, unit = "month")

# Standardize the index variable within each school and keyword
google_trends_data <- google_trends_data %>%
  group_by(schname, keynum) %>%
  mutate(std_index = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)) %>%
  ungroup()

# aggregate to the school-month level
school_data <- google_trends_data %>%
  group_by(schname, month) %>%
  summarize(mean_std_index = mean(std_index, na.rm = TRUE))

# Print the first few rows of the aggregated data
head(school_data)

# adding Score card data
scorecard_data <- read_csv("C:/Users/ferna/OneDrive/Documents/OMSBA 5300/Data explaration/OMSBA-5300/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv")

# reading in id_name_link data
name_data <- import("C:/Users/ferna/OneDrive/Documents/OMSBA 5300/Data explaration/OMSBA-5300/Lab3_Rawdata/id_name_link.csv")

schools <- name_data %>%
  group_by(schname) %>%
  mutate(n = n()) %>%
  ungroup()

# Filter out schools that show up more than once
filtered_name_data <- schools %>%
  filter(n == 1) %>%
  select(schname, unitid, opeid)

final_data <- school_data %>%
  inner_join(filtered_name_data, by = "schname") %>%
  inner_join(scorecard_data, by = c("unitid" = "UNITID", "opeid" = "OPEID")) %>% 
  filter(PREDDEG == 3) %>% 
  rename("average_earnings" = "md_earn_wne_p10-REPORTED-EARNINGS")

#create a threshold to find the median earnings
threshold <- median(final_data$average_earnings)
  #median earnings is 42300

#create a binary variable for high/low earning
final_data$HighEarning <- ifelse(final_data$average_earnings >= threshold, 1, 0)

#create date value for date
release_date <- as.Date("2015-09-01")

#binary value for date
final_data$PostScorecard <- ifelse(final_data$month >= release_date, 1, 0)

#SAT median for college
sat_average <- median(final_data$SAT_AVG_ALL)

final_data$SATMScore <- ifelse(final_data$SAT_AVG_ALL >= sat_average, 1,  0)

# Print the first few rows of the final data
head(final_data)

final_data_release <- school_data %>%
  inner_join(filtered_name_data, by = "schname") %>%
  inner_join(scorecard_data, by = c("unitid" = "UNITID", "opeid" = "OPEID")) %>% 
  filter(PREDDEG == 3) %>% 
  filter(month >= as.Date("2015-09-01")) %>% 
  rename("average_earnings" = "md_earn_wne_p10-REPORTED-EARNINGS")

threshold_release <- median(final_data_release$average_earnings)

final_data_release$HighEarningRelease <- ifelse(final_data_release$average_earnings >= threshold, 1, 0)

###########################REGRESSION###########################################

#create a regression to answer the research question
library(fixest)
google_search <- feols(mean_std_index ~ HighEarning, data = final_data)
etable(google_search)
summary(google_search)

#after the release
google_search_after <- feols(mean_std_index ~ HighEarning + PostScorecard, data = final_data)
etable(google_search)
summary(google_search_after)

#regression taking SAT average scores
google_SAT <- feols(mean_std_index ~ HighEarning + SATMScore, data = final_data)
etable(google_SAT)

#after release 
google_SAT_after <- feols(mean_std_index ~ HighEarning + PostScorecard  + SATMScore, data = final_data)
etable(google_SAT_after)

###################################Graph########################################
library(ggplot2)

ggplot(final_data, aes(x = average_earnings , y = mean_std_index, color = factor(HighEarning))) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Regression of mean_std_index on HighEarning",
       x = "HighEarning",
       y = "mean_std_index")+
  scale_color_manual(values = c("red", "blue"),labels = c("Low Earnings", "High Earnings")) +
  theme_minimal()

#graph after the release of the article
ggplot(final_data_release, aes(x = average_earnings , y = mean_std_index, color = factor(HighEarningRelease))) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Regression of mean_std_index on HighEarning",
       x = "HighEarning",
       y = "mean_std_index")+
  scale_color_manual(values = c("red", "blue"),labels = c("Low Earnings", "High Earnings")) +
  theme_minimal()

#graph for SAT
ggplot(final_data, aes(x = average_earnings , y = mean_std_index, color = factor(SATMScore))) +
  geom_point() +  # scatter plot of observed data
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # fitted regression line
  labs(title = "Regression of mean_std_index on HighEarning",
       x = "HighEarning",
       y = "mean_std_index")+
  scale_color_manual(values = c("red", "blue"),labels = c("Low Score", "High Score")) +
  theme_minimal()



