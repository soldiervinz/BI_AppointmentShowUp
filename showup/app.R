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


#####
# Descriptiv
#####
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

######
# inference
######
#statistische Relevanz

fisher.test(data$No.show, data$Scholarship) #signifikant -> stipendiate kommen weniger 
fisher.test(data$No.show, data$Hipertension) #signifikant -> hipertensive kommen mehr
fisher.test(data$No.show, data$Diabetes) #signifikant -> diabetes kommen mehr
fisher.test(data$No.show, data$Alcoholism) #kein einfluss
fisher.test(data$No.show, data$Handcap) 
fisher.test(data$No.show, data$SMS_received) #signifikant -> sms kommen weniger

######
# ML
######
fit <- rpart(No.show ~ Age+Gender+Scholarship+Hipertension+Diabetes+Alcoholism+Handcap+SMS_received+dayDifferences, method="class", data=data, control=rpart.control(cp=0.0001,maxdepth=7))

#Extremwerte unter drei Tagen entfernen
smallData <- subset(data, dayDifferences>0)
fit2 <- rpart(No.show ~ Age+Gender+Scholarship+Hipertension+Diabetes+Alcoholism+Handcap+SMS_received+dayDifferences, method="class", data=smallData, control=rpart.control(cp=0.0001,maxdepth=8))


######
# shiny App
######
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "BI - No Show"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Cleanup", tabName = "data_cleanup"),
      menuItem("Aggregation", tabName = "aggregation"),
      menuItem("Deskriptiv", tabName = "descriptiv"),
      menuItem("Inference", tabName = "inference"),
      menuItem("ML", tabName = "ml")
      
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
                    verbatimTextOutput("data_aggregation2"),
                    plotOutput("data_aggregation_hist")
                )
              )
      ),
      tabItem(tabName = "descriptiv",
              fluidRow(
                tabBox(title = "Visualisation", width = 12,
                       id="tabset",
                       tabPanel("Binary Variable",
                                sidebarLayout(
                                  sidebarPanel("Select Variable",
                                               checkboxInput(inputId = "is_gender", label = "Gender", value=T),
                                               checkboxInput(inputId = "is_scholarship", label = "Scholarship", value = T),
                                               checkboxInput(inputId = "is_hipertension", label = "Hipertension", value = F),
                                               checkboxInput(inputId = "is_diabetes", label = "Diabetes", value = F),
                                               checkboxInput(inputId = "is_alcoholism", label = "Alcoholism", value = F),
                                               checkboxInput(inputId = "is_handcap", label = "Handcap", value = F),
                                               checkboxInput(inputId = "is_sms", label = "SMS", value = F),
                                               checkboxInput(inputId = "is_age", label = "Age", value = F) 
                                  ),
                                  mainPanel(
                                    tabPanel("Plots", column(6,plotOutput(outputId="plotgraph",height="400px", width = "600px")))
                                  )
                                  
                                )
                                ),
                       tabPanel("SMS received Details",    tabPanel("SMS",
                                                                    box(
                                                                        "At first glance it seems that reveiving an sms has an negativ impact of showing up",
                                                                        plotOutput("sms_detail_sms_plot")),
                                                                    box( width = 12,
                                                                        "But in the plots you can see, that (almost) nobody gets an sms if the dayDifference is <= 3 days",
                                                                        plotOutput("sms_percent")
                                                                        ),
                                                                    box(width = 12,
                                                                        "Most of the Appointsments has an dayDifference less than 3 days",
                                                                        plotOutput("sms_absolut")
                                                                        ),
                                                                    infoBox("DayDifference","This is an importan factor", fill = TRUE, width = 12),
                                                                    box(
                                                                      plotOutput("dayDifference_factor")
                                                                    ),
                                                                    infoBox("SMS received","dayDifference affect sms received", fill = TRUE, width = 12),
                                                                    box(
                                                                      plotOutput("sms")
                                                                    ),
                                                                    box(
                                                                      plotOutput("sms_extended")
                                                                    )
                                                    )))
                
              )),
      tabItem(tabName = "inference",
              fluidPage(tabBox(title = "Exakter Fischer Test", width = 12,
                               id="tabset_fischer",
                               tabPanel("dayDifference is short",
                                        infoBox(
                                          "Significant", icon = icon("thumbs-up", lib = "glyphicon"),
                                          color = "green"
                                        ),
                                        verbatimTextOutput("fischer_dayDifference")),
                               tabPanel("Gender",
                                        infoBox(
                                          "Not Significant", icon = icon("thumbs-down", lib = "glyphicon"),
                                          color = "yellow"
                                        ),
                                        verbatimTextOutput("fischer_gender")),
                               
                               tabPanel("Scholarship",
                                        infoBox(
                                          "Significant", icon = icon("thumbs-up", lib = "glyphicon"),
                                          color = "green"
                                        ),
                                        verbatimTextOutput("fischer_scholarship")),
                               tabPanel("Hipertension",
                                        infoBox(
                                          "Significant", icon = icon("thumbs-up", lib = "glyphicon"),
                                          color = "green"
                                        ),
                                        verbatimTextOutput("fischer_hipertension")),
                               tabPanel("Diabetes",
                                        infoBox(
                                          "Significant", icon = icon("thumbs-up", lib = "glyphicon"),
                                          color = "green"
                                        ),
                                        verbatimTextOutput("fischer_diabetes")),
                               tabPanel("Alcoholism",
                                        infoBox(
                                          "Not Significant", icon = icon("thumbs-down", lib = "glyphicon"),
                                          color = "yellow"
                                        ),
                                        verbatimTextOutput("fischer_alcoholism"))
                               
                        )
              )
    ),
    tabItem(tabName = "ml",
            fluidPage(
             tabBox(title = "Tree", width = 12,
                    tabPanel("All data",
                             plotOutput("ml_tree"),
                             verbatimTextOutput("ml")),
                    tabPanel("dayDifference > 0",
                             plotOutput("ml_tree_without"),
                             verbatimTextOutput("ml_without")))
            ))
  )
  
  )
  
)

server <- function(input, output) {
  ### Output Data
  # Data Table
  temp2 = data
  temp2$ScheduledDay <- as.character(temp2$ScheduledDay)
  temp2$AppointmentDay <- as.character(temp2$AppointmentDay)
  output$data <- renderTable({
    head(temp2[, 3:14])
  }, spacing = "s")
  
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
  
  output$data_aggregation2 <- renderPrint({
    range(data$dayDifferences)
  })
  
  output$data_aggregation_hist <- renderPlot({
    histo_dayDiffernces
  })
  
  ### Deskriptiv
  # Binary Variable
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
  pt7 <- reactive({
    if(!input$is_sms) return(NULL)
    plot_sms
  })
  pt8 <- reactive({
    if(!input$is_age) return(NULL)
    plot_age
  })
  ### Output Plot
  output$plotgraph <- renderPlot({
    # generate bins based on input$bins from ui.R
    plotlist <- list(pt1(), pt2(), pt3(), pt4(), pt5(), pt6(), pt7(), pt8())
    # remove the null plots from plotlist
    not_null <- !sapply(plotlist, is.null)
    plotlist <- plotlist[not_null]
    if (length(plotlist)==0) return(NULL)
    grid.arrange(grobs=plotlist, ncol=length(plotlist))
  })
  
  # sms detail
  output$sms_detail_sms_plot <- renderPlot({
    plot_sms
  })
  
  output$sms_percent <- renderPlot({
    plot_PercSmsDayDifference
  })
  
  output$sms_absolut <- renderPlot({
    plot_smsDayDifference
  })
  
  output$sms <- renderPlot({
    plot_sms
  })
  
  output$sms_extended <- renderPlot({
    plot_sms_extended
  })
  
  output$dayDifference_factor <- renderPlot({
    temp <- data
    temp$is_dayDiff_short <- temp$dayDifferences < 3
    
    ggplot(as.data.frame(temp)) + 
      aes(x = is_dayDiff_short, fill = No.show) +
      geom_bar(position = "fill") +
      labs(title="Show Up depending on dayDifference", x="dayDifferences < 3", y="Appointments") +  
      scale_fill_discrete(name="Show up", labels=c("Yes", "No"))
  })
  
  ### Inference
  output$fischer_dayDifference <- renderPrint({
    temp <- data
    temp$is_dayDiff_short <- temp$dayDifferences < 3
    fisher.test(temp$No.show, temp$is_dayDiff_short)
  })

  
  output$fischer_gender <- renderPrint({
    fisher.test(data$No.show, data$Gender)
  })
  
  output$fischer_scholarship <- renderPrint({
    fisher.test(data$No.show, data$Scholarship)
  })
  
  output$fischer_hipertension <- renderPrint({
    fisher.test(data$No.show, data$Hipertension)
  })
  
  output$fischer_diabetes <- renderPrint({
    fisher.test(data$No.show, data$Diabetes)
  })
  
  output$fischer_alcoholism <- renderPrint({
    fisher.test(data$No.show, data$Alcoholism)
  })
  
  ### ML
  output$ml <- renderPrint({
    printcp(fit) # display the results
  })
  
  output$ml_without <- renderPrint({
    printcp(fit2) # display the results
  })
  
  
  output$ml_tree <- renderPlot({
    plot(fit, uniform=TRUE, main="Classification Tree for Biopsy")
    text(fit, use.n=TRUE, all=TRUE, cex=.8)
  })
  
  
  output$ml_tree_without <- renderPlot({
    plot(fit2, uniform=TRUE, main="Classification Tree for Biopsy")
    text(fit2, use.n=TRUE, all=TRUE, cex=.8)
  })
  

  
  
  
  
  
}

shinyApp(ui, server)