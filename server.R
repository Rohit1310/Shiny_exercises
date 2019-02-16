library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(curl)
library(XML)
library(HistData)
data(GaltonFamilies)
library(dplyr)

function(input, output) {
  
## Tab 1 function:
  d <- reactiveValues(data = NULL) # used to update the data used using the reactivity
  
  getDistType <- reactive({input$rdtab1})
  
  output$plot1 <- renderPlot({
    
  if(getDistType() == "nr"){
    
    set.seed(122)
    histdata <- rnorm(500)
    d$data <- histdata[seq_len(input$slider)]
    hist(d$data)
  }
    else if( getDistType() == "un"){
      set.seed(122)
      histdata <- runif(500)
      d$data <- histdata[seq_len(input$slider)]
      hist(d$data)
    }
    else if(getDistType() == "po"){
      set.seed(122)
      histdata <- rpois(500,lambda = 20)
      d$data <- histdata[seq_len(input$slider)]
      hist(d$data)
    }
  })
  
  observeEvent(input$actionButtab1,
               
               output$m1 <- renderText({
                 paste("Mean:",round(mean(d$data),2),"(Approximated to 2 decimal places)")
               })
               )
  
  observeEvent(input$actionButtab1.2,
               
               output$m1 <- renderText({
                 paste("Standard Deviation:", round(sd(d$data),2),"(Approximated to 2 decimal places)")
               })
  )
  
  observeEvent(input$helptab1,
               output$m1 <- renderText({
                 paste("Help  Content can be give here...\n due to time constrain i am not including the content")
               })
    
  )
  
  
  ## Tab 2 Functions:
  
  getinputdata <- reactive({
    infile <- input$reportTab2
    
    if(is.null(infile)){
      return <-  NULL
    }
    else{
      read.csv(infile$datapath)
    }
  })
  
  getreportfor <- reactive(input$reporttype)
  
  
  output$plot2 <- renderPlot({
    
    if(getreportfor() == "pr"){
      
      prPlot <- ggplot(getinputdata(),aes(priority, fill = priority))+
        geom_bar() + 
        coord_flip() + 
        guides(fill=FALSE)+ 
        theme_minimal() +
        labs(title = "Priority Plot",x = "Priority",y = "Count")+
        theme(plot.title=element_text(size=25, face="bold"),
              axis.text.x = element_text( size = 15,angle = 90,colour = "black"),
              axis.title.x = element_text( size = 20,face = "bold",colour = "black"),
              axis.text.y  = element_text( size = 15,colour = "black"),
              axis.title.y = element_text( size = 20,face = "bold",colour = "black")
        )
      
      print(prPlot)
      
      ggsave("myplot.png")  # saves the last plot.
      ggsave("myplot.png", plot=prPlot)
      
      
    }else if(getreportfor() == "ar"){
      
      arPlot <- ggplot(getinputdata(),aes(area, fill = priority))+
        geom_bar() + 
        coord_flip() +
        theme_minimal() +
        labs(title = "Area Plot",x = "Area",y = "Count")+
        theme(plot.title=element_text(size=25, face="bold"),
              axis.text.x = element_text( size = 15,angle = 90,colour = "black"),
              axis.title.x = element_text( size = 20,face = "bold",colour = "black"),
              axis.text.y  = element_text( size = 15,colour = "black"),
              axis.title.y = element_text( size = 20,face = "bold",colour = "black")
        )
      
      print(arPlot)
      
      ggsave("myplot.png")  # saves the last plot.
      ggsave("myplot.png", plot=arPlot)
      
    }else if(getreportfor() == "re"){
      
      rePlot <- ggplot(getinputdata(),aes(u_resolve_code, fill = priority))+
        geom_bar() + 
        coord_flip() +
        theme_minimal() +
        labs(title = "Area Plot",x = "Resolution Code",y = "Count")+
        theme(plot.title=element_text(size=25, face="bold"),
              axis.text.x = element_text( size = 15,angle = 90,colour = "black"),
              axis.title.x = element_text( size = 20,face = "bold",colour = "black"),
              axis.text.y  = element_text( size = 15,colour = "black"),
              axis.title.y = element_text( size = 20,face = "bold",colour = "black")
        )
      
      print(rePlot)
      
      ggsave("myplot.png")  # saves the last plot.
      ggsave("myplot.png", plot=rePlot)
    }
    
    output$downloadPlot <- downloadHandler(
      filename = function() {'plot.png'},
      content = function(file){
        plotZip <- file.path(getwd(), "myplot.png")
        file.copy(plotZip, file)
      },
      contentType = "image/png"
    )
    
    observeEvent(input$helptab2,
                 output$m2 <- renderText({
                   paste("Mandatory Columns in Report: area,priority,u_resolve_code")
                 })
                 )
    
    
  })
  
  ## Tab 3:
  
  getinputdatatab3 <- reactive({
    infile <- input$reportTab3
    
    if(is.null(infile)){
      return <-  NULL
    }
    else{
      read.csv(infile$datapath)
    }
  })
  
  output$tableTab3 <- renderDT(getinputdatatab3(),options = list(
    initComplete = JS("function(settings, json) {",
      "var rows = $(this.api().table().rows());",
      "for (var i = 0; i < rows.length; i++){ ",
      "var row = rows[i];",
      "row.css({'background-color': '#000', 'color': '#f00'})",
      "}",
      "}"),selection = 1
  )
  )
  
  
  ## Tab 4
  
  # 1st step: pass inches to cm
  gf <- GaltonFamilies
  gf <- gf %>% mutate(father=father*2.54,
                      mother=mother*2.54,
                      childHeight=childHeight*2.54)
  
  # simple linear regression 
  model1 <- lm(childHeight ~ father + mother + gender, data=gf)
  
  output$pText <- renderText({
    paste("Father's height is",
          strong(round(input$inFh, 1)),
          "cm, and mother's height is",
          strong(round(input$inMh, 1)),
          "cm, then:")
  })
  
  output$pred <- renderText({
    df <- data.frame(father=input$inFh,
                     mother=input$inMh,
                     gender=factor(input$inGen, levels=levels(gf$gender)))
    ch <- predict(model1, newdata=df)
    kid <- ifelse(
      input$inGen=="female",
      "Daugther",
      "Son"
    )
    paste0(em(strong(kid)),
           "'s predicted height is going to be around ",
           em(strong(round(ch))),
           " cm"
    )
  })
  
  output$Plot <- renderPlot({
    kid <- ifelse(
      input$inGen=="female",
      "Daugther",
      "Son"
    )
    df <- data.frame(father=input$inFh,
                     mother=input$inMh,
                     gender=factor(input$inGen, levels=levels(gf$gender)))
    ch <- predict(model1, newdata=df)
    yvals <- c("Father", kid, "Mother")
    df <- data.frame(
      x = factor(yvals, levels = yvals, ordered = TRUE),
      y = c(input$inFh, ch, input$inMh))
    ggplot(df, aes(x=x, y=y, fill = x)) +
      geom_bar(stat="identity", width=0.5) +
      geom_text(aes(label=paste(round(y),"cm")), vjust= 1, hjust = 3, color="black", size=5)+
      xlab("") +
      ylab("Height (cm)") +
      theme_minimal() +
      theme(legend.position="none") +
      coord_flip()
    
  })
  
  ## TAb 5
  
  get.input.url <- reactive({input$url})
  
  
  
  
  output$webPage <- renderPrint({
    url <- get.input.url()
    dt <- readHTMLList(url)
    sapply(dt,print)
  })
  
  output$webPage2 <- renderTable({
    url <- get.input.url()
    dt <- readHTMLTable(url)
  })
  
  output$rawWebpage <- renderText({
    url <- get.input.url()
    con <- GET(url)
    webpage <- content(con,"text")
  })
  
  
}