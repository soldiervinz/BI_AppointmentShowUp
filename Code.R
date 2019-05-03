library(dplyr)
suppressMessages(library(dplyr))

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
