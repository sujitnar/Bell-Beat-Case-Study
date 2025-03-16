#Let's begin by importing all the data as a dataframe.
#We have included a daily version of the files for sleep, steps, activity, intensities
#calories and the same for minutes save activity. 

daily_sleep <- read_csv("sleepDay_merged.csv")
minute_steps <- read_csv("minuteStepsWide_merged.csv")

minute_intensities <- read_csv("minuteIntensitiesWide_merged.csv")
minutes_sleep <-read_csv("minuteSleep_merged.csv")

calories_minute <- read_csv("minuteCaloriesWide_merged.csv")
daily_steps <- read_csv("dailySteps_merged.csv")
daily_calories <- read_csv("dailyCalories_merged.csv")
daily_Intensities <- read_csv("dailyIntensities_merged.csv")
daily_activity <- read_csv("dailyActivity_merged.csv")


#Let's load our libraries in the script console so that we have it saved even if we exit.
install.packages("corrplot")
install.packages("ggpubr", repos = "https://cloud.r-project.org/", dependencies = TRUE)
install.packages("classInt", repos = "https://cloud.r-project.org/", dependencies = TRUE)
library(corrplot)
library(readr)
library(skimr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(here)
library(ggplot2)
library(ggpubr)
library(classInt)
install.packages("viridis")
library(viridis)
install.packages("wesanderson")
library(wesanderson)

#Now let's start organizing and cleaning the data

#Let's break down our analysis into two parts. For the first part we analyze teh data collected on
# a daily basis for sleep, calories etc. 

## Daily Activity Tracker

#Lets start by calculating the number of distinct Ids in our datasets. 

n_unique(daily_activity$Id)
#35 unique users in daily activity dataset
n_unique(daily_calories$Id)
#33 unique users in daily calories dataset
n_unique(daily_Intensities$Id)
# 33 unique Ids in daily Intensities
n_unique(daily_sleep$Id)
# 24 unique users in daily sleep
n_unique(daily_steps$Id)
# 33 unique users. 

#Check for duplicates
sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_Intensities))
sum(duplicated(daily_sleep))
sum(duplicated(daily_steps))
# 3 duplicates in daily_sleep df

#Two methods of checking for null values. 
# colSums(is.na(data_frame)) > 0 : The is.na(data_frame) returns a value True = 1 if it finds a 
#null value and False = 0 if it finds a null value the sum of columns will be greater than zero. It
#checks this for each and every column. The "which" method identifies the location of the null values. 
#The two methods combined can tell us where and how many null values there are in the data frame. 
# 
#colSums(is.na(daily_steps)) >0 
#which(is.na(daily_steps))

#There are no null values in the daily datasets. 

#Since we have 3 duplicated values in daily sleep, let's  the duplicates. 

daily_sleep <- daily_sleep %>% 
  distinct()
#Let's check to see if duplicates have been removed. 

sum(duplicated(daily_sleep))


#Let's now look at the datasets one by one to check for formats and naming consistency
head(daily_activity)
str(daily_activity)
colnames(daily_activity)
#Let's rename the columns names so that they are all in lower case. And change the Activity date 
#column to a date format mm-dd-yyyy
clean_names(daily_activity)
daily_activity <- rename_with(daily_activity, tolower)
#to check they are all lower case and only have a _ as a special character
colnames(daily_activity)
daily_activity <- daily_activity %>% 
  rename(activity_day = activitydate)
#Change the format of the activity_date column
daily_activity <- daily_activity %>% 
  mutate(activity_day = as.POSIXct(activity_day, format="%m/%d/%Y", tz=Sys.timezone()))
str(daily_activity)
#Before we do anything else let's check if the data makes sense. One possible sanity check is
# to recognize that the total_distance scales linearly with the total_steps. 
#ggplot(daily_activity, aes(total_steps, total_distance , colour = total_steps)) + geom_point()
#str(daily_activity)

#Let's rename column names and change date or date-time formats for all the data sets
clean_names(daily_calories)
daily_calories <- rename_with(daily_calories, tolower)
head(daily_calories)
str(daily_calories)
#Let's change date to date format
daily_calories <- daily_calories %>% 
  rename(activity_day = activityday)
daily_calories <- daily_calories %>% 
  mutate(activity_day = as.POSIXct(activity_day, format="%m/%d/%Y", tz=Sys.timezone()))
str(daily_calories)

clean_names(daily_Intensities)
daily_Intensities <- rename_with(daily_Intensities, tolower)
daily_Intensities <- daily_Intensities %>% 
  rename(activity_day = activityday)
daily_Intensities <- daily_Intensities %>% 
  mutate(activity_day = as.POSIXct(activity_day, format="%m/%d/%Y", tz=Sys.timezone()))
str(daily_Intensities)
clean_names(daily_sleep)
str(daily_sleep)
daily_sleep <- rename_with(daily_sleep, tolower)
daily_sleep <- daily_sleep %>% 
  rename(activity_day = sleepday)

# we need to change the sleep_day sleep_day to a date-time format. Also 
#since we will eventually want to merge the datasets we will rename the 
#sleep day column as activity_day
daily_sleep <- daily_sleep %>% 
  mutate(activity_day = as.POSIXct(activity_day, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone()))
str(daily_sleep)

clean_names(daily_steps)
str(daily_steps)
daily_steps <- rename_with(daily_steps, tolower)
daily_steps <- daily_steps %>% 
  rename(activity_day = activityday)



daily_steps <- daily_steps %>% 
  mutate(activity_day = as.POSIXct(activity_day, format="%m/%d/%Y", tz=Sys.timezone()))
str(daily_steps)

#Let's try out the different merge functions. Using both tidyverse and just the plain merge function. First
# let's merge just using the merge function


merge_4 <- merge(daily_activity, daily_sleep, by = c("id", "activity_day"))
merge_5 <- merge(merge_4, daily_calories, by = c("id", "activity_day"))
merge_6 <- merge(merge_5, daily_Intensities,by = c("id", "activity_day") )
daily_routine <- merge(merge_6, daily_steps,by = c("id", "activity_day") )

#Now we have the final merged data set of all the different daily activities. With this we can now start
#analyzing the data and figuring out what we can infer from the data. 

#Let's first check the column names of the final dataframe
colnames(daily_routine)
#Now we have the merged dataset. The dataset contains information about users identified by their user id. 
#We can now either choose to focus on individual id's and look at data corresponding to those id's or we c
# can look at aggregated data for different users. We will follow the latter method for data analysis and
# look at at averages of sleep times, active times etc for each id. 

#For this purose let's define a new data frame that averages the values of different columns, grouped by id.
daily_users_aggregate <- daily_routine %>% 
  group_by(id) %>% 
  summarise(mean_steps = mean(totalsteps), mean_distance = mean(totaldistance), mean_sleep = mean(totalminutesasleep),
            mean_time_in_bed = mean(totaltimeinbed), mean_sedentary = mean((sedentaryminutes.x + sedentaryminutes.y )/2), 
            mean_tracker_distance = mean(trackerdistance), 
            mean_calories = mean((calories.x + calories.y)/2), mean_Light_active_minutes = mean((lightlyactiveminutes.x+lightlyactiveminutes.y)/2),
            mean_sleep_to_bed = mean(totalminutesasleep/totaltimeinbed), n = n(), mean_moderate_active_minutes = mean((fairlyactiveminutes.x+fairlyactiveminutes.y)/2),
            mean_very_active_minutes = mean((veryactiveminutes.x + veryactiveminutes.y)/2))
            
#Now that we have the data let's create some visualizations to looks at the relations between the different users

#Let's start with a measure of the average time spent in bed vs average time asleep. This gives
# us a clue to the quality of sleep for the person. I am going to try to fit this to a linear model 
# assuming that the "quality" of sleep is reflected in how much time a person spends in bed versus
# the amount of time they actually sleep. This is a qualitative assumption since there might be other
# reasons for a person to spend time in bed and not sleep like scrolling through social media, watching
# a tv series, reading a book etc. 

#We start by fitting a linear model to the data. 
lm_fit <- lm(mean_time_in_bed ~ mean_sleep, data = daily_users_aggregate)
summary(lm_fit)

# The summary of the linear fit is given below : 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -44.025 -29.930 -17.329  -0.222 212.690 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -31.5273    36.9626  -0.853    0.403    
# mean_sleep    1.1961     0.0923  12.959 8.96e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 60.52 on 22 degrees of freedom
# Multiple R-squared:  0.8842,	Adjusted R-squared:  0.8789 
# F-statistic: 167.9 on 1 and 22 DF,  p-value: 8.957e-12

# The mismatch of the residuals (the difference between observed and predicted (linear) values)
# is mainly due to the outliers in the dataset. The high F-statistic value and and low p-value 
# of the model fit implies the linear relationship is pretty strong. To test this lets look 
# at the dataset without the outlier. Since the dataset is small we can identfy the outlier
# just by looking at the ratio of sleep to time in bed. But we will work with the assumpiton
# that we don't know this and find out the outliers. 

predicted <- data.frame(sleep_pred = predict(lm_fit, daily_users_aggregate), sleep=daily_users_aggregate$mean_sleep)

ggplot(data = daily_users_aggregate, aes(x = mean_time_in_bed , y = mean_sleep)) + 
  geom_point(color='blue') +
  geom_smooth(colour = 'red', method = "lm", se = FALSE)

daily_users_aggregate_outlier <- daily_users_aggregate %>% 
  filter(mean_sleep_to_bed > 0.7)
#With the filtering let's try plotting the data again
lin_mod_fit <- lm(mean_time_in_bed ~ mean_sleep, data = daily_users_aggregate_outlier)
summary(lin_mod_fit)
# The summary of the results are given below:
#   Residuals:
#   Min       1Q   Median       3Q      Max 
# -16.2347  -6.2825  -0.8939   4.8026  31.2566 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   5.2622     7.4378   0.707    0.487    
# mean_sleep    1.0527     0.0191  55.127   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 11.23 on 20 degrees of freedom
# Multiple R-squared:  0.9935,	Adjusted R-squared:  0.9931 
# F-statistic:  3039 on 1 and 20 DF,  p-value: < 2.2e-16

#We see that the linear fit is now even better. Of course we have to be careful in assuming
# a linear relationship between time spent in bed vs time spent sleep because there may
# be a vareity of reasons because of which users might be spending time in bed not necessarily
# sleeping.
#We can also verify this visually by plotting mean time in bed vs mean time spent sleeping

ggplot(data = daily_users_aggregate_outlier, aes(x = mean_time_in_bed , y = mean_sleep)) + 
  geom_point(color='blue') +
  geom_smooth(colour = 'red', method = "lm", se = FALSE)

num_intervals <- 3
kmeans_intervals <- classIntervals(daily_users_aggregate$n, num_intervals, style = "kmeans")

# Assign each mpg value to its corresponding interval
daily_users_aggregate$User_category <- cut(daily_users_aggregate$n,
                                          breaks = kmeans_intervals$brks, include.lowest = TRUE, labels = c("Low use", "Medium Use", "High Use"))

# Create the dotplot using ggplot2
# ggplot(daily_users_aggregate, aes(x = factor(User_category), y = n, colour = User_category)) +
#   geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
#   labs(title = "Users category",
#        x = "Category of users",
#        y = "Mean_Sleep") +
#   coord_flip()
#Catergorical Plot of User type
ggplot(daily_users_aggregate, aes(x = factor(User_category), y = n)) +
  geom_point() +
  labs(title = "User category",
       x = "Category of users",
       y = "Number of days") +
  coord_flip()

#Bar plot for user type
ggplot(data.frame(daily_users_aggregate$User_category), aes(x=daily_users_aggregate$User_category)) +
  geom_bar(colour = 'black', fill = 'blue')+
  labs(title = "Number of User Type",
       x = "Type of User",
       y = "Count of User Type")
#Pie chart for user type
#install.packages("lessR")
#library(lessR)

# Categorical data
# cat <- daily_users_aggregate$User_category
# User_cat <- factor(cat)
# # Store the variable as data frame
# Uscat <- data.frame(User_cat)

# Pie

# mytable <- table(daily_users_aggregate$User_category)
# lbls <- paste(names(mytable))
# pie(mytable, labels = lbls,
#     main="Pie Chart of User Type" )


daily_users_aggregate %>% 
  ggplot(aes(x = "", fill = User_category)) + 
  geom_bar(position = "fill", width = 1) + 
  coord_polar(theta = "y") + 
  labs( 
    title = "User Category", 
    x = "", 
    y = ""
  ) + 
  theme(
    panel.grid = element_blank(), 
    axis.ticks = element_blank(), 
    axis.text.x=element_blank(), 
    panel.border = element_blank())

#We see that almost half of the users in our acquired data use the activity tracker regularly. 
#We could also quantify this and ask what percentage of users are 
# active  users (high use) vs inactive users (low use).


percent_data <- daily_users_aggregate %>% 
  group_by(User_category) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
   group_by(User_category) %>%
   summarise(total_percent = total / totals) %>%
   mutate(User_Percentage = scales::percent(total_percent))

daily_users_aggregate <- daily_users_aggregate %>%
  left_join(percent_data, by = "User_category")

#Let's check the calories burned by users.

# Let's run another clustering to see if we can identify users based 
# on calories burned
num_intervals <- 3
kmeans_intervals_calories <- classIntervals(daily_users_aggregate$mean_calories, num_intervals,
                                   style = "kmeans")
daily_users_aggregate$Calories_category <- cut(daily_users_aggregate$mean_calories,
                                           breaks = kmeans_intervals_calories$brks, include.lowest = TRUE, 
                                           labels = c("Low burn", "Medium burn", "High burn"))
#Categorical Plot
ggplot(daily_users_aggregate, aes(x = factor(Calories_category), y = mean_calories)) +
  geom_point() +
  labs(title = "Calories Burned",
       x = "Category of Calories Burned",
       y = "Mean_Calories") +
  coord_flip()

#Bar plot for Calories burned

ggplot(data.frame(daily_users_aggregate$Calories_category), aes(x=daily_users_aggregate$Calories_category)) +
  geom_bar(colour = 'black', fill = 'blue')+
  labs(title = "Number of Users and their Category of Burned Calories",
       x = "Calories Burned",
       y = "Count of User")
#We see an overwhelming number of users displayed low calories burned. 


#An obvious check we can also do is to check the relationship between mean_steps and
# mean_distance or mean_calories. Let's create a scatter plot for the two. 

ggplot(daily_users_aggregate, aes(x = mean_distance, y = mean_steps))+
  geom_point()
#We see that it sort of has a linear relationship. Let's try to fit a linear model
# and see how well the model fits the data. 
lm_distance_steps_fit <- lm(mean_steps ~ mean_distance, data = daily_users_aggregate)
summary(lm_distance_steps_fit)

#Here is the summary of our fit. 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1028.8  -452.5  -141.2   534.3  1532.7 
# 
# Coefficients:
#   Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)     823.92     283.72   2.904              0.00823 ** 
#   mean_distance  1260.68      45.24  27.866 < 0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 627.1 on 22 degrees of freedom
# Multiple R-squared:  0.9724,	Adjusted R-squared:  0.9712 
# F-statistic: 776.5 on 1 and 22 DF,  p-value: < 0.00000000000000022
#Now let's try to fit the linear model line to the data
ggplot(data = daily_users_aggregate, aes(x = mean_steps , y = mean_distance)) + 
  geom_point(color='blue') +
  geom_smooth(colour = 'red', method = "lm", se = FALSE)+ 
  labs( 
    title = "Mean distance vs Mean Steps", 
    x = "Mean Steps", 
    y = "Mean Distance"
  )
#As expected the linear model fits the data extremely well. Now let's check mean calories
#vs mean steps
ggplot(data = daily_users_aggregate, aes(x = mean_steps , y = mean_calories)) + 
  geom_point(color='blue') +
  geom_smooth(colour = 'red', method = 'lm', se= FALSE)
#We see mean calories has no obvious linear relation to mean steps. Technically it 
#shouldn't have a linear relationship to mean_distance either but let's verify

ggplot(data = daily_users_aggregate, aes(x = mean_distance , y = mean_calories)) + 
  geom_point(color='blue') +
  geom_smooth(colour = 'red')
#As expected
# let's create a new column based on active minutes. Let's do this in two steps. One wehre we keep
#sedentary minutes. And one where we don't. 
daily_users_aggregate$total_active_minutes_sed <- daily_users_aggregate$mean_Light_active_minutes + daily_users_aggregate$mean_moderate_active_minutes+
  daily_users_aggregate$mean_very_active_minutes + daily_users_aggregate$mean_sedentary
daily_users_aggregate$total_active_minutes_no_sed <- daily_users_aggregate$mean_Light_active_minutes + daily_users_aggregate$mean_moderate_active_minutes+
  daily_users_aggregate$mean_very_active_minutes 

df <- daily_users_aggregate[, !(names(daily_users_aggregate) %in% c("proportion_active_minutes"))] 
daily_users_aggregate <- df

#What proportion of their total minutes tracked is sedentary minutes?
daily_users_aggregate$proportion_sedentary_minutes <- (daily_users_aggregate$mean_sedentary)/(daily_users_aggregate$mean_Light_active_minutes + daily_users_aggregate$mean_moderate_active_minutes+
                                                      daily_users_aggregate$mean_very_active_minutes +
                                                        daily_users_aggregate$mean_sedentary)
#Let's catergorize the users based on their number of steps. 
num_intervals <- 4
kmeans_intervals_steps <- classIntervals(daily_users_aggregate$mean_steps, num_intervals,
                                            style = "kmeans")
daily_users_aggregate$active_category_steps <- cut(daily_users_aggregate$mean_steps,
                                               breaks = kmeans_intervals_steps$brks, include.lowest = TRUE, 
                                               labels = c("Sedentary", "Semi-Active", "Active", "Very Active"))
#Categorical Plot
ggplot(daily_users_aggregate, aes(x = factor(active_category_steps), y = mean_steps, colour = mean_calories )) +
  geom_point() +
  labs(title = "Total Calories burned vs Total Steps (User activity)",
       x = "Mean_Calories",
       y = "Mean_Steps") +
  coord_flip()

id_active <- subset(daily_users_aggregate,
                    Calories_category == "High burn" & User_category == "High Use",
                    select = id)
#Let's Plot the bar plot to get a better idea of the categorization.

ggplot(data.frame(daily_users_aggregate$active_category_steps), aes(x=daily_users_aggregate$active_category_steps)) +
  geom_bar(colour = 'black', fill = 'blue')+
  labs(title = "Category of active nature of users based on Steps",
       x = "Category of Activeness",
       y = "Count of User")
#We see a large portion of users lie in the active part. 
#Yet another category we can check is that based on their active minutes and we can compare if active minutes
#and number of steps tally up. We again categorize the users by their total active minutes. 
#Again we we will apply similar categories. 
num_intervals_minutes <- 3
kmeans_intervals_active_minutes <- classIntervals(daily_users_aggregate$total_active_minutes_no_sed, num_intervals_minutes,
                                         style = "kmeans")
daily_users_aggregate$category_active_minutes <- cut(daily_users_aggregate$total_active_minutes_no_sed,
                                                   breaks = kmeans_intervals_active_minutes$brks, include.lowest = TRUE, 
                                                   labels = c("Light_Active", "Moderate_Active", "Very_Active"))
#Let's do the usual visualisation of categorical plots and bar plots

#Categorical Plot
ggplot(daily_users_aggregate, aes(x = factor(category_active_minutes), y = total_active_minutes_no_sed)) +
  geom_point() +
  labs(title = "Category of User Based on Active Minutes",
       x = "Category of Users",
       y = "Total Active Minutes") +
  coord_flip() +
  scale_fill_viridis() + theme_bw()
#Bar plot for the same measure

ggplot(data.frame(daily_users_aggregate$category_active_minutes), aes(x=daily_users_aggregate$category_active_minutes)) +
  geom_bar(colour = 'black', fill = 'blue')+
  labs(title = "Category of Users Based on Active Minutes",
       x = "Category of Users",
       y = "Count of Category")
daily_users_aggregate$active_minutes <- ((daily_users_aggregate$mean_moderate_active_minutes+daily_users_aggregate$mean_very_active_minutes)*daily_users_aggregate$n)
#Let's look at the lightly active users and check their data
users_light_active <- subset(daily_users_aggregate, category_active_minutes == "Light_Active")
users_light_active
c_a_m <- daily_users_aggregate
#Let's see if there's a correlation between the sedentary minutes vs sleep
ggplot(data = daily_users_aggregate, aes(x = total_active_minutes_sed, y = mean_sleep))+
  geom_point()  +
  geom_smooth(method = 'lm', se = FALSE)+
  scale_fill_viridis() + theme_bw() +
  labs( 
    title = "Mean_Sleep vs Mean Sedentary Minutes", 
    x = "Mean Minutes Active", 
    y = "Mean Sleep"
  )
num_intervals_sleep <- 3
kmeans_intervals_sleep <- classIntervals(daily_users_aggregate$mean_sleep, num_intervals_sleep,
                                         style = "kmeans")
daily_users_aggregate$Sleep_Category <- cut(daily_users_aggregate$mean_sleep,
                                            breaks = kmeans_intervals_sleep$brks, include.lowest = TRUE, 
                                            labels = c("Bad Sleep", "Decent Sleep", "Good Sleep"))
  
#At first glance it seems to be that there is a negative correlation but it's also a
#a bit over the place. Let's categorize this again by adding a colour based on 
#cateogry of active minutes grouped by users and see if it gives clearner data
ggplot(daily_users_aggregate, aes(x = mean_sedentary, y = mean_sleep, color = Sleep_Category, shape = Sleep_Category)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(color = Sleep_Category)) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 18)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  labs(
    x = "Mean Sedentary Minutes",
    y = "Mean Sleep Minutes",
    color = "Activity Level",
    shape = "Activity Level",
    title = "Relationship Between Sedentary Time and Sleep",
    subtitle = "Colored by Activity Level"
  )
#We see that there is a negative correlation between mean time spent being sedentary and mean sleeping time
#Does this mean that people who are more active tend to sleep better? Let's check this
ggplot(daily_users_aggregate, aes(x = total_active_minutes_no_sed, y = mean_sleep, color = Sleep_Category, shape = Sleep_Category)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(color = Sleep_Category)) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 18)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  labs(
    x = "Mean Total Active Minutes",
    y = "Mean Sleep Minutes",
    color = "Activity Level",
    shape = "Activity Level",
    title = "Relationship Between Active Time and Sleep",
    subtitle = "Colored by Activity Level")
caterogies <- c("id", "User_category", "Calories_category", "active_category_steps", "category_active_minutes", "Sleep_Category" )
data <- daily_users_aggregate[caterogies]   

data_long <- pivot_longer(data, cols = -id, names_to = "category", values_to = "value")

# Create stacked bar plot
ggplot(data_long, aes(x = factor(id), y = 1, fill = value)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~category, nrow = 1) +
  labs(title = "Categorical Data by ID",
       x = "ID",
       y = "Count",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), # Hide y-axis labels
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # Hide y-axis ticks

ggplot(data_long, aes(x = factor(id), y = category, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = 'inferno') + # Use a discrete color scale
  labs(title = "Heatmap of Categorical Data by ID",
       x = "ID",
       y = "Category",
       fill = "Value") +
  theme_minimal() +  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # Rotate x-axis labels
  )


highlight_ids <- data_long %>%
  group_by(id) %>%
  filter(value == "High Use") %>%
  pull(id) %>%
  unique()

highlight_ids
data_long <- data_long %>%
  mutate(highlight = ifelse(id %in% highlight_ids, "Yes", "No"))
ggplot(data_long, aes(x = factor(id), y = category, fill = value)) +
  geom_tile(aes(color = highlight), linewidth = 1.5) + # Add border for highlighted IDs
  scale_fill_viridis_d(option = "viridis") + # Use a discrete color scale
  scale_color_manual(values = c("Yes" = "black", "No" = "white")) + # Highlight color
  labs(title = "Heatmap of Categorical Data by ID (Highlighted IDs)",
       x = "ID",
       y = "Category",
       fill = "Value",
       color = "Highlight") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # Rotate x-axis labels
  )
# # Heatmap with vertical x-axis labels and ColorBrewer palette
# ggplot(data_long, aes(x = factor(id), y = category, fill = value)) +
#   geom_tile(color = "white") + # Add white borders to tiles
#   scale_fill_brewer(palette = "Set1") + # Use ColorBrewer palette (e.g., "Set1")
#   labs(title = "Heatmap of Categorical Data by ID",
#        x = "ID",
#        y = "Category",
#        fill = "Value") +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # Rotate x-axis labels
#   )
install.packages("RColorBrewer")
library(RColorBrewer)
extended_palette <- c(brewer.pal(9, "Set1"), brewer.pal(8, "Set2"))

# Use the extended palette
ggplot(data_long, aes(x = factor(id), y = category, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = extended_palette) + # Use the extended palette
  labs(title = "Heatmap of Categorical Data by ID",
       x = "ID",
       y = "Category",
       fill = "Value") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # Rotate x-axis labels
  )
#Let's make a final category based on sleep. We define sleep in terms of the amount of time slept on average. Let's categorize 
#Sleep as Bad, Decent and Good sleep

caterogies_no_cal <- c("id", "User_category", "active_category_steps", "category_active_minutes", "Sleep_Category" )
data_no_cal <- daily_users_aggregate[caterogies_no_cal]   

data_long_no_cal <- pivot_longer(data_no_cal, cols = -id, names_to = "category", values_to = "value")
extended_palette <- c(brewer.pal(9, "Set1"), brewer.pal(8, "Set2"))

# Use the extended palette
ggplot(data_long_no_cal, aes(x = factor(id), y = category, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = extended_palette) + # Use the extended palette
  labs(title = "Heatmap of Categorical Data by ID",
       x = "ID",
       y = "Category",
       fill = "Value") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # Rotate x-axis labels
  )
numeric_categories <- c("mean_steps", "mean_distance", "mean_sleep", "mean_calories", "total_active_minutes_no_sed", "mean_sedentary", "n")
numeric_data <- daily_users_aggregate[numeric_categories]
Correlations <- cor(numeric_data)
corrplot(Correlations, method="circle", mar=c(0.5,0.5,0.5,0.5))

ggplot(daily_users_aggregate, aes(x= n, y = mean_sleep_to_bed))+
  geom_point()+
  geom_smooth(method = 'lm')



