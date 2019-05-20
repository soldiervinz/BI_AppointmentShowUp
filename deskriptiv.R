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
# start at age 0
data <- filter(data, data$Age >= 0)
data <- filter(data, data$Age >= 0 & data$Age <= 100)
x <- filter(data, data$Age > 100)
length(x$PatientId)
x$Age
range(data$Age)
### Clean up Handcap ~ levels [1 - 4] ==> level [1], and dropLevels [2 - 4]
str(data$Handcap)
summary(data$Handcap)

hc <- data$Handcap
for(i in 1:length(hc)) {
  if (as.numeric(as.character(data$Handcap[i])) > 0) {
    data$Handcap[i] = 1
  }
}

# drop levels which are not in use
data$Handcap <- droplevels(data$Handcap)
summary(data$Handcap)

#####
# Feature Engineering
#####
#dayDifferences
data$dayDifferences <- data$AppointmentDay - data$ScheduledDay

######
# Deskriptiv
#####
# Frauen gehen öfters zum Arzt
ggplot(data, aes(x=data$Gender, fill=data$Gender)) +
  geom_bar(position = "dodge")

ggplot(data, aes(x=data$No.show, y=data$Age, col=data$No.show)) +
  geom_boxplot() +
  labs(title="Show Up Gender", x="no show", y="Age")


# Ob man sms bekommt ist nicht abhängig vom alter
sms_no <- filter(data, data$SMS_received == 0)
summary(sms_no$Age)

sms_yes <- filter(data, data$SMS_received == 1)
summary(sms_yes$Age)


set.seed(205)
split = 0.80
trainIndex <- createDataPartition(data$No.show,p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]


library(rpart)
library(MASS)
test <- sample(1:nrow(data), 1000)
length(test)
train <- data[-test,] 
test <- data[test,]

?rpart

### rpart geht nicht - es gibt nur ein root, doch unterscheidet nicht...
fit <- rpart(train$No.show ~ train$SMS_received + train$Age, method="class", data=train)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results

print(fit)
plot(fit)
