library(dplyr)
suppressMessages(library(dplyr))

data <- read.csv("KaggleV2-May-2016.csv")
data <- tbl_df(data)
glimpse(data)

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

glimpse(data$No.show)

# to Date factor, --> YYYY-MM-DD
data$ScheduledDay <- as.Date.factor(data$ScheduledDay)
data$AppointmentDay <- as.Date.factor(data$AppointmentDay)


### Clean up variable Age
range(data$Age)

# patients with error in age
ageErr <- filter(data, data$Age < 0)
glimpse(ageErr)
# start at age 0
data <- filter(data, data$Age >= 0)
range(data$Age)

### Check for NA ~ no NA
na <- sapply(data, is.na)
any(na)

### Check for NULL ~ no NULL
nu <- sapply(data, is.null)
any(nu)

### Clean up PatientId ~ nothing
summary(data$PatientId)
patNumeric <- sapply(data$PatientId, is.numeric)
all(patNumeric)

### Clean up AppointmentID ~ nothing
summary(data$AppointmentID)
appNumeric <- sapply(data$AppointmentID, is.numeric)
all(appNumeric)

### Clean up Gender ~ nothing
summary(data$Gender)

### Clean up ScheduledDay ~ nothing
class(data$ScheduledDay)
summary(data$ScheduledDay) # min: 2015-11-10
sort(data$ScheduledDay)

### Clean up AppointmentDay ~ noting
class(data$AppointmentDay)
summary(data$AppointmentDay) # min: 2016-04-29

### Clean up Neighbourhood
summary(data$Neighbourhood)
levels(data$Neighbourhood)
str(data$Neighbourhood)
# x <- data$Neighbourhood[37]
# gsub("[^A-z]", "_", x)
# gsub("")
# gsub("[:punct:]", "_", x)


### Clean up Scholarship
str(data$Scholarship)

### Clean up Hipertension ~ nothing
str(data$Hipertension)

### Clean up Diabetes ~ nothing
str(data$Diabetes)

### Clean up Alcoholism ~ nothing
str(data$Alcoholism)

### Clean up Handcap ~ levels [1 - 4] ==> level [1], and dropLevels [2 - 4]
str(data$Handcap)
summary(data$Handcap)

hc <- data$Handcap
for(i in 1:length(hc)) {
  if (as.numeric(as.character(data$Handcap[i])) > 0) {
    data$Handcap[i] = 1
  }
}

# [for loop]
data$Handcap <- droplevels(data$Handcap)
summary(data$Handcap)

### Clean up SMS_received ~ noting
str(data$SMS_received)
summary(data$SMS_received)

### Clean up No-show
str(data$No.show)
summary(data$No.show)


###
# descriptive statistics
###

str(data)
summary(data)
cbind(data$Gender,data$No.show)

smalldata <- head(data,n=600)

#Barplot Gender ShowUp
barplot(cbind(smalldata$Gender,smalldata$No.show),
  beside=TRUE, # Nebeneinander
  horiz=T, # Balken horizontal
  col=c("gray10","gray60"), # Farbwerte für die Balken
  border="white", # Farbe der Balkenränder, NA bedeutet keinen Rand
  space=c(0.1,1), # Abstand(in, out) links vom Balken in % der Balkenbreite
  legend=TRUE, # Legende drucken
  args.legend=list( # Argumente für die Legende
    x=200, # x-Position
    y=8 # y-Position
  ), 
  xlim=c(0,200), # Y-Achse skalieren (von, bis)
  xlab="Überlebende", # Beschriftung X-Achse
  ylab="Klasse", # Beschriftung der Y-Achse
  main="Überlebende Passagiere der Titanic"
)
print(barplot)

?barplot
