# submenuitem for the school tab


popSaebersTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "popSaebersTab",
    
    p("hi")
  )
}

# ---- SERVER ----
popSaebersTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
  })
}