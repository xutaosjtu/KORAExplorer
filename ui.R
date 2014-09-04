
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
                       textInput("KORA.var", label = "Variables", value = "utbmi"),
                       sliderInput("bins",
                                   "Number of bins:",
                                   min = 1,
                                   max = 50,
                                   value = 30)
                     ),
                     mainPanel(
                       plotOutput("histPlot", height = "400px", width = "400px")                       
                     )
                   )
          ),
          
          tabPanel("X-Y plot", 
              sidebarLayout(
                  sidebarPanel(
                      textInput("KORA.x", label = "X = ", value = "utalter"),
                      textInput("KORA.y", label = "Y = ", value = "utbmi"),
                      textInput("KORA.f", label = "Colored By", value = "ucsex"),
                      textInput("KORA.logscale", label = "logarithm transform"),
                      selectInput("plot.type", label = "Type of plot to use", 
                                  choices = c("scatterplot", 
                                              "boxplot"), 
                                  selected = "scatterplot")
                    ),
                  mainPanel(
                    plotOutput("xyplot", height = "400px", width = "800px"),
                    helpText("The association between the two variables in the whole population and in the subgroups are"),
                    tableOutput("asso.xy")
                    )
                )        
          ),
          
          tabPanel("Correlation with metabolites", 
              sidebarLayout(
                sidebarPanel(
                  helpText("Please put the names of the phenotypes whose correaltions with metabolites you would like to investigate (Please separate the phenotype by ',')"),
                  textInput("KORA.covaraites", label = "The intermediate phenotypes", 
                            value = "utalter,utbmi"),
                  selectInput(inputId= "cormethod", 
                              label = "Choose a methods to calculate correlations",
                              choices = c("Pearon's correlation"="pearson", 
                                          "Spearman correlation"="spearman"), 
                              selected = "spearman")                 
                  ),
                mainPanel(
                  dataTableOutput("cor_metabo")
                  )
                )
          ),
          
          ## second tab panel for showing data
          tabPanel("KORA Data", dataTableOutput("dataTable"))
        ), 
       width = "1500px"
       
    )
))
