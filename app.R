library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(shinythemes)

source('global.R')

ui = navbarPage(title = "Covid-19 Visualizer", theme = shinytheme("cerulean"),
                
                #Evolution dans le monde
                
                tabPanel(title = "World Overview",
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             fluidRow(
                               column(12,
                                      
                                      radioButtons(inputId = "level", "Choose a graph:",
                                                   choices = c("Cumulative", "Daily variation"),
                                                   selected = c("Cumulative")),
                                      helpText("Choose a graphic type to display")
                               )
                             ),
                             
                           ),
                           
                           
                           mainPanel(
                             
                             fluidRow(
                               column(9, offset = 3,
                                      titlePanel(tags$h3("Covid19 evolution in the world\n")))
                             ),
                             tags$hr(),
                             tags$br(),
                             
                             fluidRow(
                               column(12,
                                      plotlyOutput("plotmonde"))
                             ),
                             
                           )
                         )
                ),
                
                
                #Evolution par pays   
                
                tabPanel(title = "Country overview",
                         
                         sidebarLayout(
                           
                           sidebarPanel(
                             
                             fluidRow(
                               
                               column(12,
                                      
                                      selectInput(inputId = "pays", "Choose a country:",   
                                                  choices = df$Country, 
                                                  selected = df$Country[0]
                                      ),
                                      helpText("Choose a country to see covid19 evolution.")
                               )
                               
                             ),
                             
                             
                           ),
                           
                           mainPanel(
                             
                             fluidRow(
                               column(8, offset=3,
                                      titlePanel(tags$h3("Covid19 evolution by country"))
                               )
                             ),
                             
                             tags$hr(),
                             tags$br(),
                             
                             fluidRow(
                               
                               column(12, plotlyOutput("plot1"))
                             ),
                             
                             fluidRow(
                               
                               column(12, plotlyOutput("plot2"))
                             )
                           )
                           
                         )
                         
                )
                
)

server = function(input, output){
  

  
  # Données mondiales
  
  output$plotmonde = renderPlotly({
    
    #Cumulatif
    
    if(input$level == "Cumulative"){
      
      g1 = ggplot(Molten, aes(x = Date, y = value, colour = variable)) +
        geom_point(size = 2) + geom_line(size = 1) +
        ggtitle('Cumulative evolution\n') +
        ylab("Number of cases") +
        scale_colour_manual("", 
                            values = c("Confirmed cases"="blue", "Deaths"="red", 
                                       "Recovered"="green", "Active cases" = "orange")) +
        theme(plot.title = element_text(color="black", size=12, face="italic"))
      
      ggplotly(g1)
      
      
    }
    
    
    #variation journaliere
    
    else if(input$level == "Daily variation"){
      
      g2 = ggplot(Molten2, aes(x = Date, y = value, colour = variable)) +
        geom_point(size = 2) + geom_line(size = 1) +
        ggtitle("Daily variation\n") +
        xlab("Date") + ylab("Number of cases") +
        scale_colour_manual("", 
                            values = c("New Cases"="blue", "Deaths"="red", 
                                       "Recovered"="green")) +
        theme(plot.title = element_text(color="black", size=12, face="italic"))
      
      
      ggplotly(g2)
      
    }
    
    
  })
  
  
  # Donnees par pays
  
  output$plot1 = renderPlotly({
    
    countries = df[df$Country == input$pays, ]
    
    plot1 = ggplot(countries, aes(x = as.Date(Date))) + 
      geom_line(aes(y = Confirmed, colour = "Confirmed cases"), size = 1) + geom_point(aes(y = Confirmed, colour = "Confirmed cases"), size = 2) +
      geom_line(aes(y = Deaths, colour = "Deaths"), size = 1) + geom_point(aes(y = Deaths, colour = "Deaths"), size = 2) + 
      geom_line(aes(y = Recovered, colour = "Recovered"), size = 1) + geom_point(aes(y = Recovered, colour = "Recovered"), size = 2) +
      geom_line(aes(y = Active, colour = "Active cases"), size = 1) + geom_point(aes(y = Active, colour = "Active cases"), size = 2) +
      scale_colour_manual("", 
                          values = c("Confirmed cases"="blue", "Deaths"="red", 
                                     "Recovered"="green", "Active cases"="orange")) +
      ggtitle(paste('Evolution cumulative:\n'), input$pays) +
      xlab("Date") + ylab("Number of cases") +
      theme(plot.title = element_text(color="black", size=12, face="italic"))
    
    ggplotly(plot1)
    
    
  })
  
  output$plot2 = renderPlotly({
    
    countries = df[df$Country == input$pays, ]
    
    countries$NewCases <- ave(countries$Confirmed, FUN=function(x) c(0, diff(x)))
    countries$NewDeaths <- ave(countries$Deaths, FUN=function(x) c(0, diff(x)))
    countries$NewRec <- ave(countries$Recovered, FUN=function(x) c(0, diff(x)))
    
    
    plot2 = ggplot(countries, aes(x = as.Date(Date))) + 
      geom_line(aes(y = NewCases, colour = "New cases"), size = 1) + geom_point(aes(y = NewCases, colour = "New cases"), size = 2) +
      geom_line(aes(y = NewDeaths, colour = "Deaths"), size = 1) + geom_point(aes(y = NewDeaths, colour = "Deaths"), size = 2) + 
      geom_line(aes(y = NewRec, colour = "Recovered"), size = 1) + geom_point(aes(y = NewRec, colour = "Recovered"), size = 2) +
      scale_colour_manual("", 
                          values = c("New cases"="blue", "Deaths"="red", 
                                     "Recovered"="green")) +
      ggtitle(paste('Daily variation:\n'), input$pays) +
      xlab("Date") + ylab("Number of cases") +
      theme(plot.title = element_text(color="black", size=12, face="italic"))
    
    ggplotly(plot2)
    
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
