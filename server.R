shinyServer(function(input, output, session) {
  source("RScript/myFun.R", local = TRUE)
  source("RScript/output.R", local = TRUE)
})