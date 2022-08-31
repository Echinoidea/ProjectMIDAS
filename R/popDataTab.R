# submenuitem for the school tab


popDataTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "popDataTab",
    
    p("hi")
  )
}

# ---- SERVER ----
popDataTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
  })
}