## Load libraries;
library(rCharts)
library(grid)

# Highcharts has a bug for the option center of pie, replace the following code (line 224) with the second one. 

# (g?[d,b,f,e[2]][h]*C(i)/100:C(i))+(a?c:0);
# 
# (g?[d,b,f,e[2]][h]*parseFloat(i)/100:parseFloat(i))+(a?c:0)
#
# this.chartWidth=t(0,b||this.containerWidth||600);this.chartHeight=t(0,p(a,this.containerHeight>19?this.containerHeight:400))
#
# this.chartWidth=t(0,b||this.userOptions.w||this.containerWidth||600);this.chartHeight=t(0,this.userOptions.h||p(a,this.containerHeight>19?this.containerHeight:400))
#

shinyUI(bootstrapPage(
  ## Include custom css and js;
  tagList(
    tags$head(
      tags$style("hr {margin: 5px 0;} #hospital, #cancer{height: 110px;}"),
      tags$link(rel="stylesheet", type="text/css",href="style.css")
    )
  ),
  
  # Database, Group, Subgroup and Year; 
  div(class = "input",
      div(class = "wPanel",
          numericInput('nrow', 'Number of rows', 8, min = 4, max = 16),
          numericInput('ncol', 'Number of column', 14, min = 4, max = 18)
      )
  ),
  
  div(class = "output", showOutput("fig", lib = "highcharts"))
  
))
