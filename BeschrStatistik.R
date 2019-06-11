library(dplyr)
suppressMessages(library(dplyr))
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(rpart)

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

#Barplots followed. We always changed the negation from No.Show

#ShowUp Gender
plot_gender <- ggplot(as.data.frame(data)) + 
  aes(x = Gender, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Gender", x="Gender", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Scholarship
plot_scholarship <- ggplot(as.data.frame(data)) + 
  aes(x = Scholarship, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Scholarship", x="Scholarship", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Hipertension
plot_hipertension <- ggplot(as.data.frame(data)) + 
  aes(x = Hipertension, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Hipertension", x="Hipertension", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Diabetes
plot_diabetes <- ggplot(as.data.frame(data)) + 
  aes(x = Diabetes, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Diabetes", x="Diabetes", y="Appointments") + 
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Alcoholism
plot_alcoholism <- ggplot(as.data.frame(data)) + 
  aes(x = Alcoholism, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Alcoholism", x="Alcoholism", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Handcap
plot_handcap <- ggplot(as.data.frame(data)) + 
  aes(x = Handcap, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up Handcap", x="Handcap", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp SMS received
plot_sms <- ggplot(as.data.frame(data)) + 
  aes(x = SMS_received, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up SMS Received", x="SMS Received", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))

#ShowUp Age
ageData<-data
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
data$dayDifferences <- data$AppointmentDay - data$ScheduledDay
histo_dayDiffernces <- ggplot(data, aes(x=dayDifferences)) + 
  geom_histogram(bins=15) +
  geom_bar(position = "fill") +
  labs(title="Distribution Day Difference", x="Day difference", y="Appointments")

#Show Plots
histo_dayDiffernces
grid.arrange(plot_gender, plot_scholarship, plot_hipertension, plot_diabetes, ncol=2)
grid.arrange(plot_alcoholism, plot_handcap, plot_sms, plot_age, ncol=2)

#statistische Relevanz
fisher.test(data$No.show, data$Scholarship) #signifikant -> stipendiate kommen weniger 
fisher.test(data$No.show, data$Hipertension) #signifikant -> hipertensive kommen mehr
fisher.test(data$No.show, data$Diabetes) #signifikant -> diabetes kommen mehr
fisher.test(data$No.show, data$Alcoholism) #kein einfluss
fisher.test(data$No.show, data$Handcap) 
fisher.test(data$No.show, data$SMS_received) #signifikant -> sms kommen weniger

#Ab wieviel Tagesdiffernz wurde ein SMS verschickt
plot_PercSmsDayDifference <- ggplot(as.data.frame(data)) + 
  aes(x = dayDifferences, fill = SMS_received) +
  geom_bar(position = "fill") +
  labs(title="Percentage Distribution Day Difference", x="Day difference", y="Appointments")
plot_smsDayDifference <- ggplot(data) + 
  aes(x = dayDifferences, fill = SMS_received) +
  geom_bar() +
  labs(title="Distribution Day Difference", x="Day difference", y="Appointments")
grid.arrange(plot_PercSmsDayDifference, plot_smsDayDifference)
subset(data, dayDifferences<=2 & SMS_received==1) # Tagesdifferenz von zwei Tagen gibt es kein SMS
#Wie ist Verh?ltnis ohne diese kurzen Abst?nden
plot_sms_extended <- ggplot(as.data.frame(subset(data, dayDifferences>2))) + 
  aes(x = SMS_received, fill = No.show) +
  geom_bar(position = "fill") +
  labs(title="Show Up SMS Received without DayDifference<=2", x="SMS Received", y="Appointments") +  
  scale_fill_discrete(name="Show up", labels=c("Yes", "No"))
grid.arrange(plot_sms, plot_sms_extended)
fisher.test(subset(data, dayDifferences>2)$No.show, subset(data, dayDifferences>2)$SMS_received) #signifikant -> sms kommen mehr

#Fragestellung: Wann kommen die Leute weniger?
#dayDifferences hat einen grossen Einfluss
fit <- rpart(No.show ~ Age+Gender+Scholarship+Hipertension+Diabetes+Alcoholism+Handcap+SMS_received+dayDifferences, method="class", data=data, control=rpart.control(cp=0.0001,maxdepth=7))
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results # Anzahl Ebene basierend auf zuf?llige Verteilung basierend. 
summary(fit) # detailed summary of splits # wann er wirklich gesplittet hat
plot(fit, uniform=TRUE, main="Classification Tree for Biopsy")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#Extremwerte unter drei Tagen entfernen
smallData <- subset(data, dayDifferences>2)
fit <- rpart(No.show ~ Age+Gender+Scholarship+Hipertension+Diabetes+Alcoholism+Handcap+SMS_received+dayDifferences, method="class", data=smallData, control=rpart.control(cp=0.0001,maxdepth=8))
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results # Anzahl Ebene basierend auf zuf?llige Verteilung basierend. 
summary(fit) # detailed summary of splits # wann er wirklich gesplittet hat
plot(fit, uniform=TRUE, main="Classification Tree for Biopsy")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
