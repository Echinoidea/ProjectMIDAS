# Loading in CSV and creating autocomplete list -----
studentData <- read.csv("data/dummy_midas_data2.csv")
studentData$ID <- sample(100:999, 50, replace = FALSE)


# studentTab =====
studentViewTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "studentTab",
          # This column contains: student image, name, age, gender, ethnicity, grade, and special ed status
          fluidRow(
            column(
              4,
              style = "background-color: #d0df92; padding: 5px; border-radius: 25px;",
              align = "center",
              
              # # Center and size for image
              # tags$head(
              #   tags$style(
              #     type = "text/css",
              #     "#studentImage img {max-width: 100%; width: 100%, height: auto;}"
              #   )
              # ),
              # 
              # # Image
              # div(style = "height: 100px; width: 100px;",
              #     imageOutput("studentImage")),
              # br(),
              # br(),
              
              # Name search (changed to autocomplete to avoid crashes)
              selectizeInput(
                NS(id, "txtinStudentID"),
                label = "Student ID Number",
                choices = c("", studentData$ID),
                selected = '',
                multiple = FALSE,
                options = list(create = FALSE)
              ),
              
              actionButton(NS(id, "btnStudentID"), label = "Search"),
              br(),
              br(),
              
              # Student demographic data
              fluidRow(
                column(
                  12,
                  align = "center",
                  
                  p(tags$b("Gender")),
                  textOutput(NS(id, "studentGender")),
                  br(),
                  
                  p(tags$b("Ethnicity")),
                  textOutput(NS(id, "studentEthnicity")),
                  br(),
                  
                  p(tags$b("Grade")),
                  textOutput(NS(id, "studentGrade")),
                  br(),
                  
                  p(tags$b("Special Education")),
                  textOutput(NS(id, "studentSpecialEd"))
                )
              )
            ),
            #MIDAS Assessments
            column(
              8,
              div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 90%;")
            )
          ))
}

studentViewTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
      rStudentData <- reactiveValues(data = studentData)
      selectedStudent <- reactiveValues(data = NULL)
      
      searchID <- reactiveVal()
      
      observeEvent(input$btnStudentID, {
        searchID(input$txtinStudentID)
        selectedStudent$data <- subset(rStudentData$data, ID == searchID())
      })
      
      output$studentGender <- renderText({
        selectedStudent$data$gender
      })
      
      output$studentEthnicity <- renderText({
        selectedStudent$data$ethnicity
      })
      
      output$studentGrade <- renderText({
        selectedStudent$data$grade
      })
      
      output$studentSpecialEd <- renderText({
        selectedStudent$data$specialEducation
      })
    }
  )
}