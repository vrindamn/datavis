
#initialize
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)

#importing data and sanitising it
product<-na.omit(read_excel("VAA.xlsx", range = "A3:A18"))
data1 <-na.omit(read_excel("VAA.xlsx", range = "A3:M36",col_names = FALSE))
sale<-na.omit(read_excel("VAA.xlsx", range = "A3:M18"))
prof<-na.omit(read_excel("VAA.xlsx", range = "A21:M36"))
data2<-sale
group<-c(1,4,2,2,11,1)
plottypes<-list(histogram = "histogram",boxplot = "boxplot", line = "line", bar = "bar", scatter="scatter",pie="pie")
#setting plotting theme
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)

# Define UI for application
ui<-(pageWithSidebar(
  #title
  headerPanel("Select Options"),
  
  #input
  sidebarPanel
  (
    radioButtons("var", "Variable:",
                 choices = c(sale = "sale",
                             profit = "prof"),
                 selected = NULL),
    selectInput("product","Product:", choices =product),
    selectInput("plot.type","Plot Type:",choices=plottypes),
    checkboxInput("show.points", "show points", TRUE)
  ),
  
  # output plot
  mainPanel(
    h3(textOutput("caption")),
    h2(textOutput("caption2")),
    uiOutput("plot") # depends on input
  )
))


# shiny server side code for each call
server<-(function(input, output, session){
  output$caption<-renderText({
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph",
           "scatter"='Scatter plot',
           "pie"='Pie Chart')
  })
  output$caption2<-renderText(input$prod)
  observe({
    #browser()
    var.opts<-colnames(data2)
    updateSelectInput(session, "variable", choices = var.opts)
    updateSelectInput(session, "product", choices = product)
    
  })
  output[['plot']] <- renderUI({
    plotOutput("p")
  })
  get_data<-reactive({
    if(input[['var']]=='sale')
      data2<-data.frame(sale)
    else
      data2=data.frame(prof)
    a<-input$prod
    group<-subset(data2[data2[,1] == a,])
    group<-group[2,]
    #obj<-list(data2,variable=input[['var']],group)
    #var<-colnames(data2)
    
    #require all to be set to proceed
    if(any(sapply(prod,check))) return()
    #make sure choices had a chance to update
    
  })
  output$p <- renderPlot({
    #plot.obj<-get_data()
    #if(is.null(plot.obj)) return()
    if(input$plot.type=="boxplot")	{	
      boxplot(group) 
    }
    else if(input$plot.type=="histogram")	{	
      hist(group) 
    }
    else if(input$plot.type=="line")	{	
      plot(group,type="l") 
    }
    else if(input$plot.type=="bar")	{	
      barplot(group)
    }
    else if(input$plot.type=="scatter")	{	
      plot(x=group)
    }
    else if(input$plot.type=="pie")	{	
      #pie(as.data.frame(table(unlist(data2))))
      pie(group)
    }
    
    .theme
    print(p)
  })
})
# Create Shiny app ----
shinyApp(ui, server)