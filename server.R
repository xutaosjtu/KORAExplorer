
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
KORA.S4_bioc =  sqlQuery(ch, "select * from s4_bioc,s4f4 where s4_bioc.s4metabo_zz=s4f4.zz_nr_s4_bio")
metabo.s4 = as.character(sqlQuery(ch, "show columns from s4_bioc")[-1,1])
KORA.F4_bioc =  sqlQuery(ch, "select * from f4_bioc,s4f4 where f4_bioc.f4metabo_zz=s4f4.zz_nr_f4_bio")
metabo.f4 = as.character(sqlQuery(ch, "show columns from f4_bioc")[-1,1])
odbcClose(ch)

trim <- function (x) gsub("^\\s+|\\s+$", "", x) ## trim white space before or after a string

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
  output$xyplot <- renderPlot({
        
      if(Reduce("&", 
                Map(f = function(x) return(x %in% colnames(KORA)), 
                    x = c(input$KORA.x, input$KORA.y, input$KORA.f))
      )
      ){
        # retrieve the x and y values based on input$KORA.x and input$KORA.y from ui.R
        x = KORA[, input$KORA.x]
        y = KORA[, input$KORA.y]
        f = KORA[, input$KORA.f]
        data = data.frame(x=x, y=y, f=f)
        
        if(input$plot.type=="boxplot"){
        
          #draw the scatter plot with coloring by f
          p = qplot(as.factor(x), y, data, colour=as.factor(f),
                    ,xlab =input$KORA.x, ylab = input$KORA.y, log = input$KORA.logscale
                    , geom = c("boxplot")
                    )
          p = p + labs(colour = input$KORA.f)
          print(p)
        }
        else if(input$plot.type=="scatterplot"){
          p = qplot(x, y, data, colour=as.factor(f), facets = .~ f, xlab =input$KORA.x, ylab = input$KORA.y, log = input$KORA.logscale)
          p = p+ stat_smooth(mapping = aes(x, y, color = as.factor(f)), method = "lm") + labs(colour = input$KORA.f)
          print(p)
        }
        #plot(x, y, col = as.factor(f), pch = 19, log = input$KORA.logscale, main = paste("Scatter plot of", input$KORA.x, "against", input$KORA.x), xlab = input$KORA.x, ylab = input$KORA.y)
        
        #legend("topright", legend = paste(input$KORA.f, levels(as.factor(f))), col = levels(as.factor(f)), pch =19)
    }
      
  })
  
  
  ##correlations between two variables
  output$asso.xy = renderTable({
    
    if(Reduce("&", 
              Map(f = function(x) return(x %in% colnames(KORA)), 
                  x = c(input$KORA.x, input$KORA.y, input$KORA.f))
    )
    ){
      
      
      x = KORA[, input$KORA.x]
      y = KORA[, input$KORA.y]
      f = as.factor(KORA[, input$KORA.f])
      
      if(input$plot.type=="boxplot"){x = as.factor(x)}
      
#       cor.all = cor.test(x, y, method = "spearman", use = "pair")
#       cor.all = c(cor.all$estimate, cor.all$p.value)
#       cor.sub = tapply(1:length(x), INDEX = f, 
#                        function(t) {
#                          tmp = cor.test(x[t], y[t], method = "spearman", use = "pair")
#                          return(c(tmp$estimate, tmp$p.value))
#                        }
#                        )
#       cor.sub = matrix(unlist(cor.sub), ncol = 2, byrow = T)
#       rownames(cor.sub) = paste(input$KORA.f,"=", levels(f))
#       cors = rbind(cor.all, cor.sub)
#       rownames(cors)[1] = "all"
#       colnames(cors) = c("correlation", "p value")
#       
      ## linear model
      model = lm(y ~ x)
      rst.lm.all = cbind(coef(model),confint(model), summary(model)$coef[,4])
      rownames(rst.lm.all)[2] = input$KORA.x
      rownames(rst.lm.all) = paste(rownames(rst.lm.all), "all", sep = "_")
      rst.lm.sub = tapply(1:length(x), INDEX = f, 
                       function(t) {
                         model = lm(y[t]~ x[t])
                         return(cbind(coef(model),confint(model), summary(model)$coef[,4]))
                       }
      )
      for(i in 1:length(rst.lm.sub)){
        rownames(rst.lm.sub[[i]])[2] = input$KORA.x
        rownames(rst.lm.sub[[i]]) = paste(rownames(rst.lm.sub[[i]]), input$KORA.f, levels(f)[i], sep = "_")
        rst.lm.all = rbind(rst.lm.all, rst.lm.sub[[i]])
      }
      colnames(rst.lm.all)[c(1,4)] = c("Estiamtes", "P value")
      
      #rst = cbind(rst.lm, cors)
      rst = rst.lm.all
      
      rst = apply(rst, c(1,2), 
                  function(x){
                    if(abs(x)<0.001){
                      format(x, scientific = T, digits = 3)
                    } 
                    else{
                      round(x, 3)
                    }
                  }
      )
      return(rst)      
    }
  })
  
  ## Correlation of variables with metabolites
  output$cor_metabo = renderDataTable({
    
    require(corrplot)
    vars = sapply(strsplit(input$KORA.covaraites, split = ","), trim)
    print(metabo.s4)
        
    cors = cor(KORA.S4_bioc[, metabo.s4], KORA.S4_bioc[,vars], use = "pair", method = input$cormethod)
    ## format the correlation output
    cors = apply(cors, c(1,2), 
                 function(x){
                   if(abs(x)<0.001){
                     format(x, scientific = T, digits = 3)
                   } 
                   else{
                     round(x, 3)
                   }
                 }
                 )
    cors = cbind(Metabolites = metabo.s4, cors)
    return(cors)
  })
  
  ## Data table
  output$dataTable <- renderDataTable({
    ## display the whole KORA survey 4 data set, with 4000 individuals
    KORA
  }, options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10))

})
