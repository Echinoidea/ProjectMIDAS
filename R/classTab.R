classTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "classTab",
          column(
            6,
            div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 5%;")
          ),
          column(
            6,
            div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 90%;")
          ))
}

classTabServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
    }
  )
}