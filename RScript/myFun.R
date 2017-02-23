textCenter <- "#! 
  function(d){
    var d = %s 
    var t = '%.0f%%'
    var a = t.toString().length
    var xpos = this.chart.series[d].center[0] 
    var ypos = this.chart.series[d].center[1] 
    var ptop = this.chart.plotTop 
    var plft = this.chart.plotLeft 
    
    this.chart.renderer.text(t, xpos+plft-6-4*(a-2), ypos+ptop+4).css({ 
      //color: '#4572A7',
      color: 'red',
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



addXlabels <- "#! 
  function() {

    var groups = %s
    var labels = groups.split(';');

    var pHeight = this.plotHeight
    var pTop    = this.plotTop
    var y = pHeight + pTop
    var x = this.plotLeft

    var n = labels.length
    var interval = this.plotWidth / n

    for (i = 0; i < n; i++) { 
      this.renderer.label(labels[i], x+i*interval + interval/2, y).css({ 
        'font-family':'Lucida Grande, Lucida Sans Unicode, Arial, Helvetica, sans-serif',
        'fill': '#080808',
        'text-anchor':'middle',
        'fontSize': '12px'
      }).attr({
        zIndex: 999
      }).add();
    }

  }
!#"
