
## Select HFORETAK;
output$uiHforetak <- renderUI({
  if (p$user %in% unique(SYKEHUS$HREGION)) {
    Items <- sort(unique(SYKEHUS$HFORETAK[which(SYKEHUS$HREGION == p$user)]))
    names(Items) <- tr(Items)
    selectInput(inputId   = "Hforetak", 
                label     = "Helseforetak", 
                choices   = Items, 
                selectize = FALSE,
                multiple  = TRUE,
                selected  = Items)
  } 
})


## Select Hospital
output$uiHospital <- renderUI({
  if (p$user %in% c(unique(SYKEHUS$HREGION), unique(SYKEHUS$HFORETAK))) {
    Items <- NULL
    if (p$user %in% unique(SYKEHUS$HFORETAK)) {
      Items <- sort(unique(SYKEHUS$KODE[which(SYKEHUS$HFORETAK %in% p$user)]))
    } else if (p$user %in% unique(SYKEHUS$HREGION)) {
      if (!is.null(input$Hforetak)) {
        Items <- sort(unique(SYKEHUS$KODE[which(SYKEHUS$HFORETAK %in% input$Hforetak)]))
      }
    }
    
    if (!is.null(Items)) {
      names(Items) <- tr(Items)
      selectInput(inputId   = "Hospital", 
                  label     = "Hospitalet", 
                  choices   = Items, 
                  selectize = FALSE,
                  multiple  = TRUE,
                  selected  = Items)
    }  
  }
})


# Select cancers;
output$uiCancer <- renderUI({
  Items <- c( "Barnekreft", "Colorectal", "LymfomKLL", "Lunge", "Mamma", "Melanom", "OsofagusVentrikkel", "Ovarier", "Prostata" )
  names(Items) <- tr(Items)
  selectInput(inputId   = "Cancer", 
              label     = "Kreftformer", 
              choices   = Items, 
              selectize = FALSE,
              size      = length(Items),
              multiple  = TRUE,
              selected  = Items)
  
})

