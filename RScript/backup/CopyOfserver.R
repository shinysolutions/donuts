
shinyServer(function(input, output, session) {
  ## Load datasets;
  load("Data/misc.RData")
  load("Data/Coverage.RData")
  Cov_100 <- Coverage[["100"]]
  Cov_200 <- Coverage[["200"]]
  
  
  ## Initialize reactiveValues:p
  p <- reactiveValues()
  observe({
    user <- parseQueryString(session$clientData$url_search)$user
    if (!is.null(user)) {
      field <- "HF"
      if (user %in% unique(SYKEHUS$HREGION)) {field <- "HREGION"}
      if (user %in% unique(SYKEHUS$HFORETAK)) {field <- "HFORETAK"}
      
      C100 <<- Cov_100[grepl(user, Cov_100[, field]), ]
      C200 <<- Cov_200[grepl(user, Cov_200[, field]), ]
      p$user <- user
    } 
  })
  
  
  
  observe({
    if (!is.null(input$HFORETAK)) {
      if (!input$HFORETAK) {
        if (!is.null(input$Hforetak)) {
          C100 <- C100[which(C100$HFORETAK %in% input$Hforetak), ]
          C200 <- C200[which(C200$HFORETAK %in% input$Hforetak), ]
        }
      }
    }
    if (!is.null(input$HOSPITAL)) {
      if (!input$HOSPITAL) {
        if (!is.null(input$Hospital)) {
          C100 <- C100[which(C100$HF %in% input$Hospital), ]
          C200 <- C200[which(C200$HF %in% input$Hospital), ]
        }
      }
    }
    if (!is.null(input$Cancer)) {
      if(input$Cancer != "Alle") {
        C100 <- C100[which(C100$Cancer %in% input$Cancer), ]
        C200 <- C200[which(C200$Cancer %in% input$Cancer), ]   
      }
    }
    print("p$C100")
    p$C100 <- C100
    p$C200 <- C200
  })
  
  
  source("RScript/input.R", local = TRUE)
  source("RScript/output.R", local = TRUE)
  
})