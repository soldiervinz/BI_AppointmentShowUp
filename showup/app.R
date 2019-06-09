## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(grid)
library(gridExtra)
library(rpart)

data <- read.csv("../KaggleV2-May-2016.csv")
data <- tbl_df(data)

#####
# Data Cleanup
#####
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

defData <- data
defData$dayDifferences <- defData$AppointmentDay - defData$ScheduledDay

### Clean up variable Age
# start at age 0
data <- filter(data, data$Age >= 0)
data <- filter(data, data$Age >= 0 & data$Age <= 100)
x <- filter(data, data$Age > 100)

hc <- data$Handcap
for(i in 1:length(hc)) {
  if (as.numeric(as.character(data$Handcap[i])) > 0) {
    data$Handcap[i] = 1
  }
}
# drop levels which are not in use
data$Handcap <- droplevels(data$Handcap)



######
# Aggregation
######
data$dayDifferences <- data$AppointmentDay - data$ScheduledDay
data <- filter(data, data$dayDifferences >= 0)
histo_dayDiffernces <- ggplot(data, aes(x=as.integer(dayDifferences))) + 
  geom_histogram(bins=15) +
  geom_bar(position = "fill") +
  labs(title="Distribution Day Difference", x="Day difference", y="Appointments")



ui <- dashboardPage(
  dashboardHeader(title = "BI - No Show"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Cleanup", tabName = "data_cleanup"),
      menuItem("Aggregation", tabName = "aggregation"),
      menuItem("Deskriptiv", tabName = "widgets"),
      menuItem("ML", tabName = "widgets")
      
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "data_cleanup",
              fluidRow(
                # Data Table
                box(width = 12, title = "Data Table", solidHeader = TRUE,
                  "The variable Age has an error", br(), "More box content",
                  tableOutput("data")
                ),
                # Age Variable
                box(width = 12, title = "Age Variable", solidHeader = TRUE,
                    "The variable Age has an error",
                    verbatimTextOutput("cleanup_age")
                ),
                # Handycap Variable
                box(width = 12, title = "Handycap Variable", solidHeader = TRUE,
                    "The variable Handycap has 4 levels",
                    verbatimTextOutput("cleanup_handcap"),
                    verbatimTextOutput("cleanup_handcap2"),
                    verbatimTextOutput("cleanup_handcap3")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "aggregation",
              fluidRow(
                # AppointmentDay / SheduleDay
                box(width = 12, title = "AppointmentDay / SheduleDay", solidHeader = TRUE,
                    "AppointmentDay ist the day, when the patient made the Appointment", br(),
                    "SheduleDay ist the day, when the patient should visit the doctor", br(), br(),
                    "Create new variable dayDifference and see the range",
                    verbatimTextOutput("data_aggregation")
                ),
                box(width = 12, title = "Histogramm of dayDifference", solidHeader = TRUE,
                    "Each row with a negativ dayDifference has been deleted",
                    plotOutput("data_aggregation_hist")
                )
              )
      )
    )
  )
  
  
)

server <- function(input, output) {
  ### Output Data
  # Data Table
  output$data <- renderTable({
    head(data[, 3:14])
  }, spacing = "xs")
  
  # Variable Age
  output$cleanup_age <- renderPrint({
    print("#Patient with error in Age")
    ageErr <- filter(defData, defData$Age < 0)
    glimpse(ageErr)
  })
  
  # Variable Handycap
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
    print("#Add up Handycap 1, 2, 3 and 4 together")
    summary(defData$Handcap)
  })

  output$cleanup_handcap3 <- renderPrint({
    defData$Handcap <- droplevels(defData$Handcap)
    print("#Drop levels")
    summary(data$Handcap)
  })
  
  ### Data Aggregation
  output$data_aggregation <- renderPrint({
    range(defData$dayDifferences)
  })
  
  output$data_aggregation_hist <- renderPlot({
    histo_dayDiffernces
  })
  
  
}

shinyApp(ui, server)