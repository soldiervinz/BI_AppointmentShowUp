### First Visualisation
### load Data
library(dplyr)
suppressMessages(library(dplyr)) 

noShowData <- read.csv("C:/Users/SoKeY/Studium/R_workspace/analyse/KaggleV2-May-2016.csv")
noshow <- tbl_df(noShowData)

### Clean up types
# to factor
noshow$Scholarship <- as.factor(noshow$Scholarship)
noshow$Hipertension <- as.factor(noshow$Hipertension)
noshow$Diabetes <- as.factor(noshow$Diabetes)
noshow$Alcoholism <- as.factor(noshow$Alcoholism)
noshow$Handcap <- as.factor(noshow$Handcap)

noshow$SMS_received <- as.factor(noshow$SMS_received)

# to Date factor, --> YYYY-MM-DD
noshow$ScheduledDay <- as.Date.factor(noshow$ScheduledDay)
noshow$AppointmentDay <- as.Date.factor(noshow$AppointmentDay)

### Plot no.show
### shows how many missed the appointment
str(noshow$No.show)
numberNS <- table(noshow$No.show)
barplot(numberNS, col=c("red","green"), legend = rownames(numberNS), main = "missed the appointment")

# with ggplot
ggplot(noshow, aes())


### no.show depend on Gender
# male
maleNs <- filter(noshow, noshow$Gender == "M")
maleNsYes <- filter(noshow, noshow$Gender == "M" & noshow$No.show == "Yes")
pNsMale <- length(maleNsYes$PatientId) / length(maleNs$PatientId)

# female
femaleNs <- filter(noshow, noshow$Gender == "F")
femaleNsYes <- filter(noshow, noshow$Gender == "F" & noshow$No.show == "Yes")
pNsFemale <- length(femaleNsYes$PatientId) / length(femaleNs$PatientId)

# make data frame
barplot(c(pNsMale, pNsFemale), col = c("blue", "pink"), main = "Wahrscheinlichkeit von no.show nach Geschlecht",legend.text = c("male", "female"))


### VARABLE AGE ###
range(noshow$Age)

# wrong patient
x <- filter(noshow, noshow$Age < 0)
x

# delete wrong patient
noshow <- filter(noshow, noshow$Age >= 0)
range(noshow$Age)


?hist
hist(noshow$Age,xlim = c(0, 120), breaks = 10, col = "plum")

# show plot no.show ~ date
nsYes <- noshow %>%
  filter(noshow$No.show == "Yes")
plot(nsYes$Age ~ nsYes$ScheduledDay)


