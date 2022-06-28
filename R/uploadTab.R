uploadTabUI <- function(id, label = "Upload Tab") {
  ns <- NS(id)
  
  tabItem(tabName = "Upload",
          fluidRow(box(
            width = 12,
            fileInput(
              NS(id, "file1"),
              "Please Upload File",
              multiple = FALSE,
              accept = c(
                "text/csv",
                ".xlsx",
                "text/comma-separated-values,text/plain",
                ".csv",
                '.xlsx'
              )
            ),
            radioButtons(
              NS(id, "level"),
              "What Total Risk Levels should the Table display?",
              c(
                "All" = "all",
                "Low" = "low",
                "Some" = "some",
                "High" = "high"
              )
            )
          )))
}

uploadTabServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      print("Upload Tab Test")
    }
  )
}
