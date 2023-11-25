#Load the packages needed for our analysis
library(tidyverse)
library(janitor)
install.packages('ggrepel')
install.packages('ggpubr')
library(ggpubr)

#Import our dataset
daily_activity <- read_csv("dailySleep_merged.csv")
hourly_steps <- read_csv('hourlySteps_merged.csv')
sleep_day <- read_csv("sleepDay_merged.csv")

daily_activity <- dailyActivity_merged
daily_sleep <- sleepDay_merged
hourly_steps <- hourlySteps_merged

#checking for unique values in our column
n_unique(daily_activity$Id)
n_unique(daily_sleep$Id)
n_unique(hourly_steps$Id)

#removing duplicates and null values
daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()
daily_sleep <- daily_sleep %>%
  distinct () %>%
  drop_na()
hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()

#cleaning and renaming columns
daily_activity <- rename_with(daily_activity, tolower)
daily_sleep <- rename_with(daily_sleep, tolower)
hourly_steps <- rename_with(hourly_steps, tolower)

#formatting our date and time columns to maintain consistency
daily_activity <- daily_activity %>%
  rename(date = activitydate) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>%
  rename(date = sleepday) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y %I:%M:%S %p", 
                        tz=Sys.timezone()))


head(daily_activity)
head(daily_sleep)

hourly_steps <- hourly_steps %>%
  rename(date_time = activityhour) %>%
  mutate(date_time = as.POSIXct(date_time, format = "%m/%d/%Y %I:%M:%S %p",
                                tz = Sys.timezone()))
head(hourly_steps)

#merging datasets
daily_activity_sleep <- merge(daily_activity, daily_sleep, 
                              by = c("id", "date"))

#viewing the merged dataset
View(daily_activity_sleep)

#calculating daily average steps by user
daily_average <- daily_activity_sleep %>%
  group_by(id) %>%
  summarise(mean_daily_steps = mean(totalsteps),
            mean_daily_calories = mean(calories),
            mean_daily_sleep = mean(totalminutesasleep))
head(daily_average)

#grading users based on their daily average steps
user_type <- daily_average %>%
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ "sedentary",
    mean_daily_steps >= 5000 & mean_daily_steps <=7499 ~ "lightly active",
    mean_daily_steps >= 7500 & mean_daily_steps <=9999 ~ "fairly active",
    mean_daily_steps >= 10000 ~ "very active"))

head(user_type)

#creating a dataframe to show percentage of users
user_type_percent <- user_type %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total))%>%
  group_by(user_type)%>%
  summarise(total_percent = total/totals)%>%
  mutate(labels = scales::percent(total_percent))

head(user_type_percent)

user_type_percent$user_type <- factor(user_type_percent$user_type, 
                                      levels = c("very active", "fairly active",
                                                 "lightly active", "sedentary"))

#chart showing the representation of each type of user
user_type_percent %>%
  ggplot(aes(x='', y=total_percent, fill=user_type))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start = 0)+
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks= element_blank (),
        axis.text.x= element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))+
  scale_fill_manual(values = c("green", "yellow", "orange", "red"))+
  geom_text(aes(label = labels), position=position_stack(vjust = 0.5))+
  labs(title= "User_type distribution")

#creating days of the week, to know which day users are more active
weekday_sleep_steps <- daily_activity_sleep %>%
  mutate(weekday = weekdays(date))
weekday_sleep_steps$weekday <- ordered(weekday_sleep_steps$weekday,
                                       levels = c("Monday","Tuesday","Wednesday",
                                                  "Thursday", "Friday", "Saturday", "Sunday"))
weekday_sleep_steps<- weekday_sleep_steps%>%
  group_by(weekday)%>%
  summarize(daily_steps = mean(totalsteps), daily_sleep = mean(totalminutesasleep))
head(weekday_sleep_steps)

#charts showing how long users walk and how much they sleep
ggarrange(
  ggplot(weekday_sleep_steps)+
    geom_col(aes(weekday, daily_steps), fill = "blue")+
    geom_hline(yintercept = 7500)+
    labs(title = "Daily Steps per Weekdays", x="", y="")+
    theme(axis.text.x = element_text(angle = 45, vjust=0.5, hjust = 1)),
 ggplot(weekday_sleep_steps,aes(weekday, daily_sleep))+
           geom_col(fill= "cyan")+
          geom_hline(yintercept = 480)+
           labs(title = "Minutes of Sleep per Weekdays", x="", y="")+
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
)


hourly_steps <- hourly_steps%>%
  separate(date_time, into = c("date", "time"), sep = " ")%>%
  mutate(date = ymd(date))
head(hourly_steps)

#chart showing steps taken by users for the 24hours in a day
hourly_steps%>%
  group_by(time)%>%
  summarise(average_steps = mean(steptotal))%>%
  ggplot()+
  geom_col(mapping = aes(x = time, y = average_steps, fill = average_steps))+
  labs(title = "Steps taken Hourly per Day", x="", y="")+
  scale_fill_gradient(low = "red", high = "green")+
  theme(axis.text.x=element_text(angle = 90))

#grouping users based on how they use the smart-device
daily_use <- daily_activity_sleep%>%
  group_by(id)%>%
  summarise(days_used = sum(n()))%>%
  mutate(usage = case_when(
    days_used >= 1 & days_used <=10 ~ "low usage",
    days_used >= 11 & days_used <= 20 ~ "moderate usage",
    days_used >= 21 & days_used <= 31 ~ "high usage"))
head(daily_use)

#calculating the percentage of the different users
daily_use_percent <- daily_use %>%
  group_by(usage)%>%
  summarise(total = n())%>%
  mutate(totals = sum(total))%>%
  group_by(usage)%>%
  summarise(total_percent = total/totals)%>%
  mutate(labels = scales::percent(total_percent))

daily_use_percent$usage <- factor(daily_use_percent$usage, levels = c("high usage",
                                                                      "moderate usage",
                                                                      "low usage"))
head(daily_use_percent)

#chart showing a representation of the different users of the smart-device
daily_use_percent %>%
  ggplot(aes(x="", y=total_percent, fill = usage))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values= c("#004d99", "#3399ff", "#cce6ff"),
                    labels = c("High usage - 21 to 31 days",
                               "Moderate usage - 11 to 20 days",
                               "Low usage - 1 to 10 days"))+
  labs(title = "Daily Usage of Smart-Watch")
  