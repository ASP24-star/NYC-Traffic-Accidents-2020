library(tidyverse)
library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)

df = read_csv("NYC Accidents 2020.csv")

#Q1. Compare the percentage ot total accidents by month. Do you notice any seasonal patterns

q1 = df %>% select(COLLISION_ID) %>% 
  mutate(date = ymd(df$`CRASH DATE`)) %>% 
  mutate_at(vars(date), funs(year, month, day))

a1 = q1 %>% mutate('MonthName' = month.name[month])

a1 = a1 %>% group_by(MonthName) %>% 
      summarise(Total_Accidents = n())

a1 = a1 %>%
  mutate(MonthName = factor(MonthName, levels = month.name)) %>%
  arrange(MonthName)

write.csv(a1, "/Users/akshaypagare/Desktop/a1.csv", row.names=FALSE)

#Q2. Break down accident frequency by day of week & hour of the day. Based on this data when do 
#    accidents occur most frequently.

q2_day = df %>% select(`CRASH DATE`, COLLISION_ID)

q2_day$weekday <- wday(df$`CRASH DATE`, label=TRUE, abbr=FALSE)

a2_day = q2_day %>% 
  group_by(weekday) %>% 
  summarise(`Total Accidents on Weekdays` = n())

write.csv(a2_day, "/Users/akshaypagare/Desktop/a2_day.csv", row.names=FALSE)


q2_hour = df %>% select(`CRASH TIME`, COLLISION_ID)

q2_hour$`Hour of the day` = hour(df$`CRASH TIME`) 

a2_hour = q2_hour %>% 
  group_by(`Hour of the day`) %>% 
  summarise(`Total Accidents at Hour` = n())

write.csv(a2_hour, "/Users/akshaypagare/Desktop/a2_hour.csv", row.names=FALSE)


#Q3. ON which particular street where the most accidents reported? What does
#   that represent as a percent of all reported accidents?


sum(is.na(df$`ON STREET NAME`))

sum(is.na(df$`CROSS STREET NAME`))

sum(is.na(df$`OFF STREET NAME`))


a3_on = df %>% select(COLLISION_ID, `ON STREET NAME`) %>% 
  group_by(`ON STREET NAME`) %>% 
  summarise(`Total Accidents` = n())%>% 
  arrange(desc(`Total Accidents`)) %>% 
  head(10) %>% 
  drop_na()

write.csv(a3_on, "/Users/akshaypagare/Desktop/a3_on.csv", row.names=FALSE)

a3_off = df %>% select(COLLISION_ID, `OFF STREET NAME`) %>% 
  group_by(`OFF STREET NAME`) %>% 
  summarise(`Total Accidents` = n()) %>% 
  arrange(desc(`Total Accidents`)) %>% 
  head(10) %>% 
  drop_na()

write.csv(a3_off, "/Users/akshaypagare/Desktop/a3_off.csv", row.names=FALSE)

a3_cross = df %>% select(COLLISION_ID, `CROSS STREET NAME`) %>% 
  group_by(`CROSS STREET NAME`) %>% 
  summarise(`Total Accidents` = n()) %>% 
  arrange(desc(`Total Accidents`)) %>% 
  head(10) %>% 
  drop_na()

write.csv(a3_cross, "/Users/akshaypagare/Desktop/a3_cross.csv", row.names=FALSE)

#Q4. What was the most common contributing factor for accidents reported in 
#    this sample (Vehicle 1)? What about the fatal accidents specifically


q4 = df %>% select(COLLISION_ID, `CONTRIBUTING FACTOR VEHICLE 1`)

a4 = q4 %>%
  group_by(`CONTRIBUTING FACTOR VEHICLE 1`) %>% 
  summarise(`Total Accidents` = n()) %>% 
  arrange(desc(`Total Accidents`))

write.csv(a4, "/Users/akshaypagare/Desktop/a4.csv", row.names=FALSE)

#Q5. Heat Map

q5 = df %>% select(COLLISION_ID, BOROUGH) %>% 
  mutate(date = ymd(df$`CRASH DATE`)) %>% 
  mutate_at(vars(date), funs(year, month, day))

a5 = q5 %>% mutate('MonthName' = month.name[month])

a5 = a5 %>% 
  group_by(BOROUGH, MonthName) %>% 
  summarise(`Total Accidents` = n()) %>% 
  drop_na()

write.csv(a5, "/Users/akshaypagare/Desktop/a5.csv", row.names=FALSE)

  



