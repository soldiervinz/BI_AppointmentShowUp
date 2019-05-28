#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
suppressMessages(library(dplyr))
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(rpart)

data <- read.csv("../KaggleV2-May-2016.csv")
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

defData <- data

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
#histo_dayDiffernces
#grid.arrange(plot_gender, plot_scholarship, plot_hipertension, plot_diabetes, ncol=2)
#grid.arrange(plot_alcoholism, plot_handcap, plot_sms, plot_age, ncol=2)


# Define UI for application that draws a histogram
ui <- navbarPage("BI",
             tabPanel("Data Cleanup",
                      mainPanel(
                
                        tableOutput("data"),
                        h2("Data Cleanup"),
                        h4("Age"),
                        verbatimTextOutput("cleanup_age"),
                        h4("Handycap"),
                        verbatimTextOutput("cleanup_handcap"),
                        verbatimTextOutput("cleanup_handcap2"),
                        verbatimTextOutput("cleanup_handcap3")
                      )),
             tabPanel("Data Aggregation", 
                      mainPanel(
                        h4("AppointmentDay vs. SheduleDay"),
                        h5("AppointmentDay ist the day, when the patient made the Appointment"),
                        h5("SheduleDay ist the day, when the patient should visit the doctor"),
                        h4("DayDifference"),
                        verbatimTextOutput("data_aggregation"),
                        plotOutput("data_aggregation_hist")
                      )),
             tabPanel("Descriptiv",
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        
                        sidebarPanel("Choose the Variables",
                                
                                     checkboxInput(inputId = "is_gender", label = "Gender", value=T),
                                     checkboxInput(inputId = "is_scholarship", label = "Scholarship", value = T),
                                     checkboxInput(inputId = "is_hipertension", label = "Hipertension", value = F),
                                     checkboxInput(inputId = "is_diabetes", label = "Diabetes", value = F),
                                     checkboxInput(inputId = "is_alcoholism", label = "Alcoholism", value = F),
                                     checkboxInput(inputId = "is_handcap", label = "Handcap", value = F)
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          column(6,plotOutput(outputId="plotgraph",height="400px", width = "800px"))
                        )
                      )
             ),
             tabPanel("ML",
                      mainPanel(
                        h5("here comes ML")
                      ))
             
)

   
   

# Define server logic required to draw a histogram
server <- function(input, output) {
   pt1 <- reactive({
     if(!input$is_gender) return(NULL)
      plot_gender
   })
   pt2 <- reactive({
     if(!input$is_scholarship) return(NULL)
      plot_scholarship
   })
   pt3 <- reactive({
     if(!input$is_hipertension) return(NULL)
     plot_hipertension
   })
   pt4 <- reactive({
     if(!input$is_diabetes) return(NULL)
     plot_diabetes
   })
   pt5 <- reactive({
     if(!input$is_alcoholism) return(NULL)
     plot_alcoholism
   })
   pt6 <- reactive({
     if(!input$is_handcap) return(NULL)
     plot_handcap
   })
   ### Output Plot
   output$plotgraph <- renderPlot({
      # generate bins based on input$bins from ui.R
      plotlist <- list(pt1(), pt2(), pt3(), pt4(), pt5(), pt6())
      # remove the null plots from plotlist
      not_null <- !sapply(plotlist, is.null)
      plotlist <- plotlist[not_null]
      if (length(plotlist)==0) return(NULL)
      grid.arrange(grobs=plotlist, ncol=length(plotlist))
   })
   
   ### Output Data
   output$data <- renderTable({
     head(data[, 3:14])
   }, spacing = "xs")
   
   ### Output Cleanup
   output$cleanup_age <- renderPrint({
     print("#Patient with error in Age")
     ageErr <- filter(defData, defData$Age < 0)
     glimpse(ageErr)
   })
   
   output$cleanup_handcap <- renderPrint({
     print("#Summary of Handcap")
     summary(defData$Handcap)
   })
   
   output$cleanup_handcap2 <- renderPrint({
     hc <- defData$Handcap
     for(i in 1:length(hc)) {
       if (as.numeric(as.character(defData$Handcap[i])) > 0) {
         defData$Handcap[i] = 1
       }
     }
     print("#Summarise Handycap 1, 2, 3 and 4 together")
     summary(defData$Handcap)
   })
   
   output$cleanup_handcap3 <- renderPrint({
     defData$Handcap <- droplevels(defData$Handcap)
     print("#Drop levels")
     summary(data$Handcap)
   })
   
   ### Data Aggregation
   output$data_aggregation <- renderPrint({
     print("#Create column dayDifference")
     summary(as.integer(data$dayDifferences))
   })
   
   output$data_aggregation_hist <- renderPlot({
     histo_dayDiffernces
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

