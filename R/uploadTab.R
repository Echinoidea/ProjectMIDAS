uploadTabUI <- function(id, label = "Upload Tab") {
  ns <- NS(id)
  
  tabItem(tabName = "uploadTab",
          fluidRow(box(
            width = 12,
            
            # Input to select data file to upload
            fileInput(
              inputId = NS(id, "uploadFile"),
              label = "Please Upload File",
              multiple = FALSE,
              accept = c(
                ".xls",
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
          ))
          )
}

uploadTabServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      uploadedData <- reactive({
        # Check if uploadFile is not NULL, then set object to the uploadFile
        req(input$uploadFile)
        upFile <- input$uploadFile
        
        # Get the filepath and extension of uploaded file
        path <- upFile$datapath
        ext <- tools::file_ext(upFile$name)
        
        # Switch statement to use appropriate read function depending on extension
        switch(ext,
          csv = readr::read_csv(path),
          xls = readxl::read_xls(path),
          xlsx = readxl::read_xlsx(path)
        )
      })
      
      
      # Create reactive variable for the uploaded data and return at the end of 
      # the function. This returned value will be passed into each server
      # module in app.R to access the school data.
      data <- reactive({
        uploadedData()
      })
      
      return(data)
    }
  )
}
