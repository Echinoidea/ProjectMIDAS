archiveTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "archiveTab",
          column(
            12,
            div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 100%;"),
            actionButton(NS(id, "testButton"), "test")
          ))
}

archiveTabServer <- function(id, uploadedData) {
  moduleServer(
    id,
    
    function(input, output, session) {
      observeEvent(input$testButton, {
        print(uploadedData())
      })
    }
  )
}
