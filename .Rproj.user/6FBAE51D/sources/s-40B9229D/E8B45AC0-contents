library(shiny)
library(shinydashboard)
library(stringr)

studentTab <- tabItem(tabName = "studentTab",
                      # This column contains: student image, name, age, gender, ethnicity, grade, and special ed status
                      fluidRow(
                        column(
                          4,
                          style = "background-color: #d0df92; padding: 5px; border-radius: 25px;",
                          align = "center",
                          
                          # Center and size for image
                          tags$head(tags$style(
                            type = "text/css",
                            "#studentImage img {max-width: 100%; width: 100%, height: auto;}"
                          )),
                          
                          # Image
                          div(style = "height: 100px; width: 100px;", 
                              imageOutput("studentImage")),
                          br(), br(),
                          
                          # Name search
                          textInput("txtinStudentName", label = "", placeholder = "Last, First"),
                          actionButton("btnStudentName", label = "Search"),
                          br(), br(),
                          
                          # Student demographic data
                          fluidRow(
                            column(
                              6,
                              align = "left",
                              
                              p("Gender"),
                              br(),
                              
                              p("Ethnicity"),
                              br(),
                              
                              p("Grade"),
                              br(),
                              
                              p("Special Education")
                            ),
                            column(
                              6,
                              align = "right",
                              
                              textOutput("studentGender"),
                              br(),
                              
                              textOutput("studentEthnicity"),
                              br(),
                              
                              textOutput("studentGrade"),
                              br(),
                              
                              textOutput("studentSpecialEd"),
                            )
                          )
                        ),
                        column(
                          8,
                          div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 90%;")
                        ))
                      )
                      

classTab <- tabItem(tabName = "classTab",
                    column(
                      6,
                      div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 5%;")
                    ),
                    column(
                      6,
                      div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 90%;")
                    ))

archiveTab <- tabItem(tabName = "archiveTab",
                      column(
                        12,
                        div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 100%;")
                      ))

ui <- dashboardPage(
  dashboardHeader(title = "MIDAS"),
  dashboardSidebar(sidebarMenu(
    menuItem("Student", tabName = "studentTab"),
    menuItem("Class", tabName = "classTab"),
    menuItem("Archive", tabName = "archiveTab")
  )),
  dashboardBody(tabItems(
    studentTab,
    classTab,
    archiveTab
  ))
)

studentData <- read.csv("data/dummy_midas_data2.csv")
# studentData$lastName <- randomNames::randomNames(nrow(studentData), which.names = "last")
# studentData$firstName <- randomNames::randomNames(nrow(studentData), which.names = "first")

server <- function(input, output) {
  # Image path should be a column in the df
  output$studentImage <- renderImage({
    # Load student image
    filename <- normalizePath(file.path('assets/images',
                                        paste('student-clip-art', '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image not available"))
  }, deleteFile = FALSE)
  
  # Reactive values for name searching
  first <- reactiveVal()
  last <- reactiveVal()
  # Reactive data frames for searching
  rStudentData <- reactiveValues(data = studentData)
  selectedStudent <- reactiveValues(data = NULL)
  
  # On search button press, split the name and subset dataframe
  observeEvent(input$btnStudentName, {
    last(strsplit(input$txtinStudentName, ",")[[1]][1])
    first(str_trim(strsplit(input$txtinStudentName, ",")[[1]][2]))
    selectedStudent$data <- subset(rStudentData$data, lastName == last() & firstName == first())
  })
  
  # Render student data
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

shinyApp(ui = ui, server = server)


# Things to work on:
# Make divs extend to bottom of page
# Name search error handling - invalid name, invalid format, multiple same name
# Make pretty
# Make modular
# Merge
# Add images and image path? Will that be accessible data?
# Update graphs to ggplot