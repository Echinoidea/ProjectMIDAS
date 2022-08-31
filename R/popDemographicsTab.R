# submenuitem for the school tab


popDemographicsTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "popDemographicsTab",
    
    p("hi")
  )
}

# ---- SERVER ----
popDemographicsTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
  })
}