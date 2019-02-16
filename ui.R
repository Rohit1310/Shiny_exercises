library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)

sidebar <- dashboardSidebar(
  sidebarMenu(
    div(id = "input", style = "background-color: lightblue;padding-bottom: 50px;",
        numericInput("tabNums","No of tabs", 1,min = 1,max = 5)
    ),
    sidebarMenu(id = "Tab1",
                menuItem("Tab-1", tabName = "Tab-1", icon = icon("dashboard")),
                menuItem("Tab-2", tabName = "Tab-2", icon = icon("th")),
                menuItem("Tab-3", tabName = "Tab-3", icon = icon("th")),
                menuItem("Tab-4", tabName = "Tab-4", icon = icon("th")),
                menuItem("Tab-5", tabName = "Tab-5", icon = icon("th"))
    )
  )
)

body <- dashboardBody(
  
  ## Tab 1 starts here
  tabItems(
        tabItem(tabName = "Tab-1",
                fluidRow(
                       infoBox("Details", value = "Dashboard to show the histogram with random normally distributed numbers", icon = icon("info-circle"))
                       ),
            fluidRow(
              box(collapsible = TRUE,
                title = "Controls",
                radioButtons("rdtab1","DistributionType",
                            choiceNames = c("Normal", "Uniform","Poisson"),
                             choiceValues = c("nr","un","po")
                             ),
                sliderInput("slider", "Number of observations:", 1, 100, 50),
                actionButton("actionButtab1","Mean", icon = icon("eye")),
                actionButton("actionButtab1.2","Standard Deviation", icon = icon("eye")),
                actionButton("helptab1","HELP",icon = icon("question"))
              ),
              box(title="PLOT",collapsible = TRUE,plotOutput("plot1", height = 350)),
              box(title = "OUTPUT",collapsible = TRUE, textOutput("m1"))
              
            )
            ),
        
    ## Tab 2 stsrts 
    tabItem(tabName = "Tab-2",
            fluidRow(
              infoBox("Details", value = "Dashboard is to create plots with a user supplied data set.The user is responsible to ensure the validity of the data set.", icon = icon("info-circle")),width = 8),
            fluidRow(
              box(
                fileInput('reportTab2', 'Upload Report (.csv file) ',accept = c('.csv')),
                radioButtons("reporttype",
                             "Plot For:",
                             selected = NULL,
                             choiceNames = c("None","Priority", "Area","Resolution"),
                             choiceValues = c("non","pr","ar","re")
                ),
                downloadButton("downloadPlot", "Download Shown Plot"),
                actionButton("helptab2","HELP",icon = icon("question"))
                     ),
              box(title = "PLOT",collapsible = TRUE,plotOutput("plot2", height = 250)),
              box(title = "HELP", collapsible = TRUE, textOutput("m2"))
            )
            
    ),
    
    ## tab 3 starts!!
    
    tabItem(tabName = "Tab-3",
            fluidRow(
              box(
                fileInput('reportTab3', 'Upload Report (.csv file) ',accept = c('.csv')),
                box(title = "Data Table Output", DTOutput("tableTab3"))
                
              )
            )
    ),
    
    ## Tab 4 starts here
    tabItem(tabName = "Tab-4",
            fluidRow(
              box(helpText("Prediction of the child's height on the basis gender and parent's height"),
                  helpText("Parameters:"),
                  sliderInput(inputId = "inFh",
                              label = "Father's height (cm):",
                              value = 150,
                              min = 150,
                              max = 220,
                              step = 1),
                  sliderInput(inputId = "inMh",
                              label = "Mother's height (cm):",
                              value = 140,
                              min = 140,
                              max = 200,
                              step = 1),
                  radioButtons(inputId = "inGen",
                               label = "Child's gender: ",
                               choices = c("Female"="female", "Male"="male"),
                               inline = TRUE)),
              box(htmlOutput("pText")
              ),
              box(htmlOutput("pred")
              ),
              box(plotOutput("Plot")
            )
                )
            
    ),
    
    ## Tab 5 starts here
    
    tabItem(tabName = "Tab-5",
            fluidRow(
              box(title = "Input",
                  textInput("url","Provide the http URL", value = "http://soilanimals.myspecies.info/")),
              box(title = "WEB CONTENT",textOutput("webPage"))
              
            ),
            fluidRow(
              box(title = "WEB 2",tableOutput("webPage2"))
            ),
            fluidRow(
              box(title = "RAW WEB PAGE",textOutput("rawWebpage"))
            )
    )
  )
)

dashboardPage(
  dashboardHeader(title = "Shiny Exercise 1"),
  sidebar,
  body
)