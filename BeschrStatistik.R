library(dplyr)
suppressMessages(library(dplyr))
library(ggplot2)
library(tidyr)
library(scales)

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

#4 diagramm per page
par(mfrow=c(2,2))

#ShowUp Gender
barplot(table(smalldata$No.show,smalldata$Gender), #alle klassen, alle geschlechter, nur die erwachsenen, nur die �berlebenden
  col = c("red","darkblue"), # Farbwerte für die Balken
  border="white", # Farbe der Balkenränder, NA bedeutet keinen Rand
  space=c(0.1,1), # Abstand(in, out) links vom Balken in % der Balkenbreite
  legend=c("Showed up","Didnt showed Up"),  # Legende drucken
  xlab="Gender",  # Beschriftung X-Achse
  ylab="Appointments",  # Beschriftung der Y-Achse
  main="Show Up Gender",
)

#ShowUp Scholarship
barplot(table(smalldata$No.show,smalldata$Scholarship), #alle klassen, alle geschlechter, nur die erwachsenen, nur die �berlebenden
        col = c("red","darkblue"), # Farbwerte für die Balken
        border="white", # Farbe der Balkenränder, NA bedeutet keinen Rand
        space=c(0.1,1), # Abstand(in, out) links vom Balken in % der Balkenbreite
        legend=c("Showed up","Didnt showed Up"),  # Legende drucken
        xlab="Scholarship",  # Beschriftung X-Achse
        ylab="Appointments",  # Beschriftung der Y-Achse
        main="Show Up Scholarship"
)

#ShowUp Hipertension
barplot(table(smalldata$No.show,smalldata$Hipertension), #alle klassen, alle geschlechter, nur die erwachsenen, nur die �berlebenden
        col = c("red","darkblue"), # Farbwerte für die Balken
        border="white", # Farbe der Balkenränder, NA bedeutet keinen Rand
        space=c(0.1,1), # Abstand(in, out) links vom Balken in % der Balkenbreite
        legend=c("Showed up","Didnt showed Up"),  # Legende drucken
        xlab="Hipertension",  # Beschriftung X-Achse
        ylab="Appointments",  # Beschriftung der Y-Achse
        main="Show Up Hipertension"
)

#ShowUp Diabetes
barplot(table(smalldata$No.show,smalldata$Diabetes), #alle klassen, alle geschlechter, nur die erwachsenen, nur die �berlebenden
        col = c("red","darkblue"), # Farbwerte für die Balken
        border="white", # Farbe der Balkenränder, NA bedeutet keinen Rand
        space=c(0.1,1), # Abstand(in, out) links vom Balken in % der Balkenbreite
        legend=c("Showed up","Didnt showed Up"),  # Legende drucken
        xlab="Diabetes",  # Beschriftung X-Achse
        ylab="Appointments",  # Beschriftung der Y-Achse
        main="Show Up Diabetes"
)

#ShowUp Alcoholism
barplot(table(smalldata$No.show,smalldata$Alcoholism), #alle klassen, alle geschlechter, nur die erwachsenen, nur die �berlebenden
        col = c("red","darkblue"), # Farbwerte für die Balken
        border="white", # Farbe der Balkenränder, NA bedeutet keinen Rand
        space=c(0.1,1), # Abstand(in, out) links vom Balken in % der Balkenbreite
        legend=c("Showed up","Didnt showed Up"),  # Legende drucken
        xlab="Alcoholism",  # Beschriftung X-Achse
        ylab="Appointments",  # Beschriftung der Y-Achse
        main="Show Up Alcoholism"
)

#ShowUp Handcap
barplot(table(smalldata$No.show,smalldata$Handcap), #alle klassen, alle geschlechter, nur die erwachsenen, nur die �berlebenden
        col = c("red","darkblue"), # Farbwerte für die Balken
        border="white", # Farbe der Balkenränder, NA bedeutet keinen Rand
        space=c(0.1,1), # Abstand(in, out) links vom Balken in % der Balkenbreite
        legend=c("Showed up","Didnt showed Up"),  # Legende drucken
        xlab="Handcap",  # Beschriftung X-Achse
        ylab="Appointments",  # Beschriftung der Y-Achse
        main="Show Up Handcap"
)

#ShowUp SMS received
barplot(table(smalldata$No.show,smalldata$SMS_received), #alle klassen, alle geschlechter, nur die erwachsenen, nur die �berlebenden
        col = c("red","darkblue"), # Farbwerte für die Balken
        border="white", # Farbe der Balkenränder, NA bedeutet keinen Rand
        space=c(0.1,1), # Abstand(in, out) links vom Balken in % der Balkenbreite
        legend=c("Showed up","Didnt showed Up"),  # Legende drucken
        xlab="SMS_received",  # Beschriftung X-Achse
        ylab="Appointments",  # Beschriftung der Y-Achse
        main="Show Up SMS_received"
)

#ShowUp Age
ageData<-smalldata
ageData$group <- 0
ageData$group[which(ageData$Age %in% 0:20)] <- "0-20"
ageData$group[which(ageData$Age %in% 21:40)] <- "21-40"
ageData$group[which(ageData$Age %in% 41:60)] <- "41-60"
ageData$group[which(ageData$Age %in% 60:200)] <- "60<"

data_percentage=apply(data, 2, function(x){x*100/sum(x,na.rm=T)})


set.seed(1124)
data=matrix(sample(1:30,15) , nrow=3)
colnames(data)=c("A","B","C","D","E")
rownames(data)=c("var1","var2","var3")

?barplot

barplot(table(smalldata$No.show,ageData$group), #alle klassen, alle geschlechter, nur die erwachsenen, nur die �berlebenden
        col = c("red","darkblue"), # Farbwerte für die Balken
        border="white", # Farbe der Balkenränder, NA bedeutet keinen Rand
        legend=c("Showed up","Didnt showed Up"),  # Legende drucken
        xlab="Age groups",  # Beschriftung X-Achse
        ylab="Appointments",  # Beschriftung der Y-Achse
        main="Show Up Age"
)
?aes
ggplot() + geom_bar(aes(y = percentage, x = ageData$group, fill = smalldata$No.show), data = ageData,
                    stat="identity")

#Only 1 diagramm
par(mfrow=c(1,1))