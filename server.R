
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(RODBC)

ch = odbcConnect("KORA_remote")
odbcQuery(ch, "use KORA")
KORA = sqlFetch(ch, sqtable = "S4F4")
odbcClose(ch)

shinyServer(function(input, output) {
  
  library(ggplot2)

  ## Histogram to show the variable distributions
  output$histPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- KORA[, input$KORA.var]
    bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', xlab = input$KORA.var, main = paste("Histogram of", input$KORA.var))

  })
  
  ## Scatter plot to show variable x against variable y
  output$scatterplot <- renderPlot({
    
    if(Reduce("&", 
              Map(f = function(x) return(x %in% colnames(KORA)), 
                  x = c(input$KORA.x, input$KORA.y, input$KORA.f))
              )
       ){
      # retrieve the x and y values based on input$KORA.x and input$KORA.y from ui.R
      x = KORA[, input$KORA.x]
      y = KORA[, input$KORA.y]
      f = KORA[, input$KORA.f]
      
      #draw the scatter plot with coloring by f
      plot(x, y, col = as.factor(f), pch = 19, log = input$KORA.logscale, main = paste("Scatter plot of", input$KORA.x, "against", input$KORA.x), xlab = input$KORA.x, ylab = input$KORA.y)
      legend("topright", legend = paste(input$KORA.f, levels(as.factor(f))), col = levels(as.factor(f)), pch =19)
      
    }
    
  })
  
  ## Data table
  output$dataTable <- renderDataTable({
    ## display the whole KORA survey 4 data set, with 4000 individuals
    KORA
  }, options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10))

})
