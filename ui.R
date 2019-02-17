library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(V8)

## side Menu

sidebar <- dashboardSidebar(
  sidebarMenu(
    div(id = "input",
        numericInput("tabNums","Number of tabs", 0,min = 0,max = 5)
    ),
    sidebarMenu(id = "Tab1",
                
                menuItem("Welcome", tabName = "welcome", icon = icon("th")),
                menuItemOutput("menuitem1"),
                menuItemOutput("menuitem2"),
                menuItemOutput("menuitem3"),
                menuItemOutput("menuitem4"),
                menuItemOutput("menuitem5")
                
                )
    )
  )

## Body

body <- dashboardBody(
  
  ## Adding the useShinyjs function to use the javascript functionality of the shinyjs Package
  tags$head(
    tags$script(useShinyjs()
                )
    ),
  
  ## Using the extention from the shinyjs package
  extendShinyjs(script = paste0(getwd(),"/www/myfile.js")),
  
## code for the tabs:
  tabItems(
    ## welcome page
    tabItem(tabName = "welcome",
            fluidRow(
              column(12,
                     infoBox("Details", 
                             value = "To use this shiny app, Please change the value of 'Number of tabs' to see the tabs on the sidebar, thereafter you can click on the tab to see the content of the tabs",
                             icon = icon("info-circle"),
                             width = NULL
                     )
              )
            ),
            fluidRow(
              box(
                helpText("Click the below button and press any key on your keyboard"),
                actionButton("jsButton","External Java script enable button before starting",icon = icon("star")))
            )
    ),
    ## Tab 1 starts here
        tabItem(tabName = "Tab-1",
                fluidRow(
                  column(12,
                         infoBox("Details", 
                                 value = "Dashboard to show the histogram with randomly distributed numbers. Click on Mean, Standard Deviation, Help buttons to view there respective values and use the slide bar to change the number of observations for the selected distribution.",
                                 icon = icon("info-circle"),
                                 width = NULL
                                 )
                         )
                       ),
            fluidRow(
              column(3,
                     box(title = "CONTROLS",
                         radioButtons("rdtab1","DistributionType",
                                      choiceNames = c("Normal", "Uniform","Poisson"),
                                      choiceValues = c("nr","un","po")
                         ),
                         sliderInput("slider", "Number of observations:", 1, 100, 50),
                         actionButton("actionButtab1","Mean", icon = icon("eye")),
                         hr(),
                         actionButton("actionButtab1.2","Standard Deviation", icon = icon("eye")),
                         hr(),
                         actionButton("helptab1","HELP",icon = icon("question")),
                         actionButton("reset","Reset",icon = icon("redo")),width = NULL)
                     ),
              div(id = "db",box(title = "Display Box",collapsible = TRUE, textOutput("m1"),width = 9)),
              box(title="PLOT",collapsible = TRUE,plotOutput("plot1", height = 300),width = 9)
            )
            ),
        
    ## Tab 2 starts 
    tabItem(tabName = "Tab-2",
            fluidRow(
              column(12,
                     infoBox("Details",
                             value = "Dashboard is to create plots with a user supplied data set.\n
                             steps to be followed: \n
                             1. upload a .csv report with Mandatory column names: 'area','priority', 'u_resolve_code'. \n
                             2. Select the 'Plot for' in the 'CONTROL' box.
                             the plot will be displayed on the 'PLOT' box.",
                             icon = icon("info-circle"),
                             width = NULL
                             )
                     )
              ),
              
            fluidRow(
              column(3,
                     box(title = "CONTROLS",
                       fileInput('reportTab2', 'Upload Report (.csv file) ',accept = c('.csv')),
                       hr(),
                       radioButtons("reporttype",
                                    "Plot For:",
                                    selected = NULL,
                                    choiceNames = c("None","Priority", "Area","Resolution"),
                                    choiceValues = c("non","pr","ar","re")
                       ),
                       hr(),
                       downloadButton("downloadPlot", "Download Shown Plot"),
                       
                       actionButton("helptab2","HELP",icon = icon("question")),
                       actionButton("resetTab2","RESET",icon = icon("redo")),
                       width = NULL
                       )
              
                     ),
              box(title = "PLOT",collapsible = TRUE,plotOutput("plot2", height = 250),width = 9),
              div(id = "hb",box(title = "HELP", collapsible = TRUE, textOutput("m2"),width = 9))
            )
            
    ),
    
    ## tab 3 starts!!
    
    tabItem(tabName = "Tab-3",
            fluidRow(
              column(12,
                     infoBox("Details",
                             value = "Upload the .csv file and click on any of the row in the DATA TABLE OUTPUT box, to see the row-data in the SELECTED ROW information box ",
                             icon = icon("info-circle"),
                             width = NULL
                     )
              )
            ),
            
            fluidRow(
              column(12,
                     infoBoxOutput("progressBox",width = NULL))
              ),
            fluidRow(
              column(3,
                     box(title = "CONTROLS",fileInput('reportTab3', 'Upload Report (.csv file) ',accept = c('.csv')),width = NULL)
                     ),
              box(title = "Data Table Output", DTOutput("tableTab3"),width = 9)
            )
              
            
    ),
    
    ## Tab 4 starts here
    tabItem(tabName = "Tab-4",
            fluidRow(
              column(12,
                     infoBox("Details",
                             value = "Prediction of the child's height on the basis gender and parent's height using GaltonFamilies R dataset.
                             Use the 'CONTROLS' box to modify the values.",
                             icon = icon("info-circle"),
                             width = NULL
                     )
              )
            ),
            fluidRow(
              column(3,
                     box(title = "CONTROLS",
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
                                      inline = TRUE),
                         width = NULL
                     )
                     ),
              box(title = "Height Comparision",plotOutput("Plot", height = 250),
                  width = 9
            ),
            box(title = "Details", htmlOutput("pText"),
                htmlOutput("pred"),
                width = 9
            )
            )
            
    ),
    
    ## Tab 5 starts here
    
    tabItem(tabName = "Tab-5",
            fluidRow(
              column(12,
                     infoBox("Details",
                             value = "Webpage list tag, table tag and raw content can be seen -> give the URL and press ENTER thereafter select the check boxes",
                             icon = icon("info-circle"),
                             width = NULL
                     )
              )
            ),
            fluidRow(
              column(3,
                     box(title = "CONTROLS",
                         textInput("url","Provide the http URL", value = "http://iczn.org"),
                         hr(),
                         checkboxGroupInput("checktab5",
                                            "Webpage content type",
                                            choiceNames = c("Table Content","List Content","Raw Content"),
                                            choiceValues = c("tc","lc","rc")),
                         width = NULL
                     )
                     ),
              box(title = "list tag content",textOutput("webPage"), width = 9),
              box(title = "table tag content",tableOutput("webPage2"),width = 9),
              box(title = "Raw WEbpage",textOutput("rawWebpage"),width = 9)
            
            )
    )
    
  ) ## tabitems ends here
  
  
) ## body ends here

dashboardPage(
  dashboardHeader(title = "Multi Tab Shiny Dashboard"),
  sidebar,
  body
)