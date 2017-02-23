
xAxis.extreme <- "#! 
  function() {
var chart = $('#fig').highcharts();
var extremes = chart.xAxis[0].getExtremes();
chart.xAxis[0].setExtremes(extremes.min,extremes.max);
chart.xAxis[1].setExtremes(extremes.min-0.5,extremes.max+0.5);
//chart.xAxis[1].setExtremes(extremes.min,extremes.max);
console.log(chart)

chart.renderer.text('Colorectal cancdr', 1, chart.chartHeight).css({ 
color: '#4572A7',
'text-align': 'center',
fontSize: '10px'
}).attr({
zIndex: 999
}).add();

}
!#"


# translates values into names;
tr <- function(value, site = ""){ 
  fun <- function(x) {
    if (x %in% meldingsnr$value) {
      if (site == "Barnekreft") {
        y <- meldingsnr$name[which(meldingsnr$value == x & meldingsnr$cancer == "Barnekreft")]
      } else {
        y <- meldingsnr$name[which(meldingsnr$value == x & meldingsnr$cancer == "Other")]
      }
      
    } else if (x %in% kreft$value) {
      y <- kreft$name[which(kreft$value == x)]
    } else if (x %in% sykehus$value) {
      y <- sykehus$name[which(sykehus$value == x)]
    } else {
      y <- x
    }
    return(y)
  }
  sapply(value, fun, USE.NAMES=FALSE)
}


textCenter <- "#! 
function(d){
var d = %s 
var t = '%.0f%%'
var a = t.toString().length
var xpos = this.chart.series[d].center[0] 
var ypos = this.chart.series[d].center[1] 
var ptop = this.chart.plotTop 
var plft = this.chart.plotLeft 
//console.log(this.chart)
//console.log(xpos)
//console.log(ypos)
//console.log(this.chart.series[d])
this.chart.renderer.text(t, xpos+plft-6-4*(a-2), ypos+ptop+4).css({ 
color: '#4572A7',
'text-align': 'center',
fontSize: '10px'
}).attr({
zIndex: 999
}).add();

}
!#"

tooltip.formatter <- "#! 
function() {
return(this.series.userOptions.hf +'<br />'+ 
this.series.name + ': <b>'+ this.y + '</b><br />'+ 
'Rate:<b>'+ Highcharts.numberFormat(this.y/this.series.total * 100, 2) + '%'  +'</b><br />');
} !#"

###############################
output$fig <- renderChart({ 
  H <- Highcharts$new()  
  H$addParams(dom = "fig")  # width = 600, height = 1450, 
  H$tooltip(formatter = tooltip.formatter)
  H$title(text = "Rapporteringsgrad") 
  H$legend(enabled = FALSE)
  
  Dat_100 <- p$C100
  Dat_200 <- p$C200
  
  dat_100_Rep <- with(Dat_100, aggregate(list(Rep_100=Key), list(Cancer = Cancer, HF = HF), FUN = function(x){length(which(x == 1 | x == 2))}))
  dat_100_Un  <- with(Dat_100, aggregate(list(Un_100 =Key), list(Cancer = Cancer, HF = HF), FUN = function(x){length(which(is.na(x)))}))
  dat_100 <- merge(dat_100_Rep, dat_100_Un, all = TRUE)
  
  dat_200_Rep <- with(Dat_200, aggregate(list(Rep_200=Key), list(Cancer = Cancer, HF = HF), FUN = function(x){length(which(x == 1 | x == 2))}))
  dat_200_Un  <- with(Dat_200, aggregate(list(Un_200 =Key), list(Cancer = Cancer, HF = HF), FUN = function(x){length(which(is.na(x)))}))
  dat_200 <- merge(dat_200_Rep, dat_200_Un, all = TRUE)
  
  dat <- merge(dat_100, dat_200, all = TRUE)
  
  
  if (nrow(dat) > 0) {      
    Cancers = sort(unique(dat$Cancer))
    HF_list = sort(unique(dat$HF))
    
    categ <- paste(rep(tr(Cancers), each = 2), c("Utredning", "Kirurgi"), sep = " ")
    print(categ)
    
    sl <- max(nchar(tr(HF_list)))
    sb <- max(nchar(tr(categ)))
    H$chart(marginLeft = sl*7, marginBottom = sb*2, marginRight = 10, marginTop = 50)
    
    H$exporting(sourceWidth = length(Cancers) * 120 + sl*7 + 10, sourceHeight= length(HF_list) * 60  + sb*2 + 50, enabled = FALSE)
    
    myColor = c(rgb(0.9, 0.9, 0.9), rgb(0.95, 0.95, 0.95))
    
    xplotBands = list()
    for (i in 1:length(categ)) {
      xplotBands[[i]] = list(color = myColor[i %% 2 + 1], from = 2*i-2.5, to = 2*i-.5)
    }
    msg <- rep(c("Utredning", "Kirurgi"), length(Cancers))
    
    csr <- rep(tr(Cancers), each = 2); csr[seq(2, length(csr), by = 2)] <- ""
    H$xAxis(categories = csr, labels = list(rotation = 16), overflow = FALSE, tickLength = 0, plotBands=xplotBands)
    
    H$xAxis(categories = msg, labels = list(rotation = 0, y = -5), gridLineWidth = 1, gridLineColor= rgb(0.925, 0.925, 0.925), tickLength = -5, replace = FALSE, opposite = TRUE)
    H$chart(replace = FALSE, events = list(load = xAxis.extreme))
    
    
    yplotBands = list()
    for (i in 1:length(HF_list)) {
      yplotBands[[i]] = list(from = i-1, to = i, label = list(align = "left", textAlign="right", x = -10, text = tr(HF_list[i])))
    }
    H$yAxis(min = 0, max = length(HF_list), allowDecimals = FALSE, tickInterval = 1, labels = list(enabled = FALSE), title = list(enabled = FALSE), plotBands = yplotBands)
    H$plotOptions(pie = list(innerSize = "60%", size = 40, slicedOffset = 0, dataLabels = list(enabled = FALSE)))
    
    H$series(data = rep(NA, length(categ)), type = "column")
    
    
    partColor = function(x) {
      if (length(x)>0) {
        if (x > 0.9) {
          return ('#85e72a')
        } else if (x <= 0.9 & x >= 0.7) {
          return ("#fffb23")
        } else if (x < 0.7) {
          return ("#fe0211")
        }
      } 
    }
    
    d = 0
    for (m in 1:length(HF_list)) {
      dat_hf <- dat[which(dat$HF == HF_list[m]), ]
      
      for (n in 1:length(Cancers)) {
        id = which(dat_hf$Cancer == Cancers[n])
        if (length(id) > 0) {
          tmp_100 <- dat_hf[id, c("Rep_100", "Un_100")]
          tmp_200 <- dat_hf[id, c("Rep_200", "Un_200")]
          
          y = paste((length(HF_list)-m+0.5)/length(HF_list)*100, "%", sep = "")
          x_100 = paste((2*n-1.5)/length(categ)  *100, "%", sep = "")
          x_200 = paste((2*n-0.5)/length(categ)  *100, "%", sep = "")
          
          
          if (!any(is.na(tmp_100))) {
            d = d + 1
            p1 = tmp_100[1,1]/sum(tmp_100[1,])
            H$series(type = "pie", center = c(x_100, y), 
                     data = c(tmp_100$Rep_100[1], tmp_100$Un_100[1]),
                     colors = c(partColor(p1), rgb(0.7, 0.7, 0.7)),
                     name = Cancers[n],
                     borderColor=  partColor(p1),
                     hf = tr(dat_hf$HF[1]),
                     events = list(afterAnimate = sprintf(textCenter, d, round(p1*100))))
          }
          
          if (!any(is.na(tmp_200))) {
            d = d + 1
            p2 = tmp_200[1,1]/sum(tmp_200[1,])
            H$series(type = "pie", center = c(x_200, y), 
                     data = c(tmp_200$Rep_200[1], tmp_200$Un_200[1]),
                     colors = c(partColor(p2), rgb(0.7, 0.7, 0.7)),
                     name = Cancers[n],
                     borderColor=  partColor(p2),
                     hf = tr(dat_hf$HF[1]),
                     events = list(afterAnimate = sprintf(textCenter, d, round(p2*100))))
          }
          
        }
      }
    } 
    
  }
  H
  
  
}) 


