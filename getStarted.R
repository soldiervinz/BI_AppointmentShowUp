### Dataset 1 ###
# Import data
noShowData <- read.csv("C:/Users/SoKeY/Studium/R_workspace/analyse/KaggleV2-May-2016.csv")
# view data
View(noShowData)

str(noShowData)
### variables to change
# scheduledDay (geplanter Tag): factor, appointmentDay (Termintag): factor,

# Neighbourhood: factor - denke das macht so sinn...
summary(noShowData)

# show first entry
noShowData[1,]

# make tubel
library(dplyr)
suppressMessages(library(dplyr)) 

noshow <- tbl_df(noShowData)
glimpse(noshow)
summary(noshow$SMS_received)

# change type
# to factor
noshow$Scholarship <- as.factor(noshow$Scholarship)
noshow$Hipertension <- as.factor(noshow$Hipertension)
noshow$Diabetes <- as.factor(noshow$Diabetes)
noshow$Alcoholism <- as.factor(noshow$Alcoholism)
noshow$Handcap <- as.factor(noshow$Handcap)
# whats about $sms_received
# -->  1 or more messages sent to the patient
# but there is just 0 or 1
summary(noshow$SMS_received)
# so make $sms_received to factor
noshow$SMS_received <- as.factor(noshow$SMS_received)

# to Date factor
# --> YYYY-MM-DD
noshow$ScheduledDay <- as.Date.factor(noshow$ScheduledDay)
glimpse(noshow$ScheduledDay)

noshow$AppointmentDay <- as.Date.factor(noshow$AppointmentDay)
head(noshow$AppointmentDay)

