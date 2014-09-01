
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("KORA Data"),

  # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30)
#     ),

    # Show a plot of the generated distribution
    mainPanel(
        
        tabsetPanel(
          ## first tabPanel for plot
          tabPanel("Hist plot", 
                   sidebarLayout(
                     sidebarPanel(
                       textInput("KORA.var", label = "Name of the variable you want to display", value = "utbmi"),
                       sliderInput("bins",
                                   "Number of bins:",
                                   min = 1,
                                   max = 50,
                                   value = 30)
                     ),
                     mainPanel(plotOutput("histPlot", height = "400px", width = "400px"))
                   )
          ),
          
          tabPanel("X-Y scatter plot", 
              sidebarLayout(
                  sidebarPanel(
                      textInput("KORA.x", label = "X = "),
                      textInput("KORA.y", label = "Y = "),
                      textInput("KORA.f", label = "Colored By"),
                      textInput("KORA.logscale", label = "logarithm transform")
                    ),
                  mainPanel(plotOutput("scatterplot", height = "400px", width = "400px"))
                )        
          ),
          
          ## second tab panel for showing data
          tabPanel("KORA Data", dataTableOutput("dataTable"))
        ), 
       width = "700px"
       
    )
))
