library(dplyr)
suppressMessages(library(dplyr))
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)

data <- read.csv("KaggleV2-May-2016.csv")
data <- tbl_df(data)

#####
# Data Cleanup
#####

### Clean up Types
# to factor
data$Scholarship <- as.factor(data$Scholarship)
data$Hipertension <- as.factor(data$Hipertension)
data$Diabetes <- as.factor(data$Diabetes)
data$Alcoholism <- as.factor(data$Alcoholism)
data$Handcap <- as.factor(data$Handcap)
data$SMS_received <- as.factor(data$SMS_received)

# to Date factor, --> YYYY-MM-DD
data$ScheduledDay <- as.Date.factor(data$ScheduledDay)
data$AppointmentDay <- as.Date.factor(data$AppointmentDay)


### Clean up variable Age
range(data$Age)

# patients with error in age
ageErr <- filter(data, data$Age < 0)
ageErr

# start at age 0
data <- filter(data, data$Age >= 0)
range(data$Age)

#####
# Descriptive statistic
#####

smalldata <- head(data,n=600)
str(smalldata)

#Barplots followed. We always changed the negation from No.Show

#ShowUp Gender
plot_gender <- ggplot(as.data.frame(smalldata)) + 
  aes(x = Gender, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Gender", x="Gender", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Scholarship
plot_scholarship <- ggplot(as.data.frame(smalldata)) + 
  aes(x = Scholarship, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Scholarship", x="Scholarship", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Hipertension
plot_hipertension <- ggplot(as.data.frame(smalldata)) + 
  aes(x = Hipertension, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Hipertension", x="Hipertension", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Diabetes
plot_diabetes <- ggplot(as.data.frame(smalldata)) + 
  aes(x = Diabetes, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Diabetes", x="Diabetes", y="Appointments") + 
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Alcoholism
plot_alcoholism <- ggplot(as.data.frame(smalldata)) + 
  aes(x = Alcoholism, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Alcoholism", x="Alcoholism", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Handcap
plot_handcap <- ggplot(as.data.frame(smalldata)) + 
  aes(x = Handcap, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Handcap", x="Handcap", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp SMS received
plot_sms <- ggplot(as.data.frame(smalldata)) + 
  aes(x = SMS_received, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up SMS Received", x="SMS Received", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Age
ageData<-smalldata
ageData$group <- 0
ageData$group[which(ageData$Age %in% 0:20)] <- "0-20"
ageData$group[which(ageData$Age %in% 21:40)] <- "21-40"
ageData$group[which(ageData$Age %in% 41:60)] <- "41-60"
ageData$group[which(ageData$Age %in% 60:200)] <- "60<"

plot_age <- ggplot(as.data.frame(ageData)) + 
  aes(x = group, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Agegroup", x="Agegroup", y="Appointments") + 
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#dayDifferences
smalldata$dayDifferences <- smalldata$AppointmentDay - smalldata$ScheduledDay
histo_dayDiffernces <- ggplot(smalldata, aes(x=dayDifferences)) + 
  geom_histogram(bins=15) +
  geom_bar(position = "fill") +
  aes(fill = No.show) +
  labs(title="Distribution Day Difference", x="Day difference", y="Appointments") + 
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

  
#Show Plots
grid.arrange(plot_gender, plot_scholarship, plot_hipertension, plot_diabetes, ncol=2)
grid.arrange(plot_alcoholism, plot_handcap, plot_sms, plot_age, ncol=2)
histo_dayDiffernces

