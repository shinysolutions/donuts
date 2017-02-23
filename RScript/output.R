###############################
output$fig <- renderChart({ 
  H <- Highcharts$new()  
  
  n_row = input$nrow
  n_col = input$ncol
  
  xLabels <- paste("Column", seq(1, n_col), sep = "_")
  yLabels <- paste("Row", seq(1, n_row), sep = "_")
  
  bgColor = c(rgb(0.9, 0.9, 0.9), rgb(0.95, 0.95, 0.95))
  
  marginLeft = 50 + 5
  marginBottom = 30
  marginRight = 20
  marginTop = 20
  unit = 100
  Width <- length(xLabels) * 1*unit + marginLeft + marginRight 
  Height<- length(yLabels) * 1*unit  + marginTop + marginBottom
  
  H$chart(marginLeft = marginLeft, marginBottom = marginBottom, marginRight = marginRight, marginTop = marginTop)
  H$addParams(h=Height, w = Width, dom = "fig")  
  H$tooltip(formatter = tooltip.formatter)
  H$legend(enabled = FALSE)
  H$exporting(sourceWidth = Width, sourceHeight= Height, enabled = TRUE)
  
  xplotBands = list()
  for (i in 1:length(xLabels)) {
    xplotBands[[i]] = list(color = bgColor[i %% 2 + 1], from = i-2.5, to = i-.5)
  }
  H$xAxis(categories = xLabels, labels = list(rotation = 0), gridLineWidth = 1, plotBands=xplotBands, 
          gridLineColor= rgb(0.925, 0.925, 0.925))
  
  
  yplotBands = list()
  for (i in 1:length(yLabels)) {
    yplotBands[[i]] = list(from = i-1, to = i, label = list(align = "left", textAlign="right", x = -10, text = yLabels[i]))
  }
  H$yAxis(min = 0, max = length(yLabels), allowDecimals = FALSE, tickInterval = 1, labels = list(enabled = FALSE), title = list(enabled = FALSE), plotBands = yplotBands)
  
  H$plotOptions(pie = list(innerSize = "60%", size = 50, slicedOffset = 0, dataLabels = list(enabled = FALSE)))
  H$series(data = rep(NA, length(xLabels)), type = "column")
  
  d = 0
  for (m in 1:length(yLabels)) {
    for (n in 1:length(xLabels)) {
      y = paste((length(yLabels)-m+0.5)/length(yLabels)*100, "%", sep = "")
      x = paste((n-0.5)/length(xLabels)  *100, "%", sep = "")
      d = d + 1
      data = abs(runif(5))
      H$series(type = "pie", center = c(x, y), 
               data = data,
               name = xLabels[n],
               hf = yLabels[m],
               events = list(afterAnimate = sprintf(textCenter, d, round(data[1]/sum(data)*100))))
      
    }
  } 
  
  H
}) 


