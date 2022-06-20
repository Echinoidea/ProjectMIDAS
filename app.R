library(shiny)
library(shinydashboard)

studentTab <- tabItem(tabName = "studentTab",
                      # This column contains: student image, name, age, gender, ethnicity, grade, and special ed status
                      fluidRow(
                        column(
                          4,
                          align = "center",
                          tags$head(tags$style(
                            type = "text/css",
                            "#studentImage img {max-width: 100%; width: 100%, height: auto;}"
                          )),
                          div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px;",
                              div(style = "height: 100px; width: 100px;", 
                                  imageOutput("studentImage")),
                              br(), br(),
                              
                              textOutput("studentName"),
                              br(), br(),
                              
                              textOutput("studentAge"),
                              br(), br(),
                              
                              textOutput("studentGender"),
                              br(), br(),
                              
                              textOutput("studentEthnicity"),
                              br(), br(),
                              
                              textOutput("studentGrade"),
                              br(), br(),
                              
                              textOutput("studentSpecialEd"),
                              br(), br(),
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

studentData <- read.csv("614upd_dummy_midas_data.csv")

server <- function(input, output) {
  output$studentImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('assets/images',
                                        paste('student-clip-art', '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number"))
  }, deleteFile = FALSE)
  
  output$studentName <- renderText({
    "Foo Bar"
  })
  
  output$studentAge <- renderText({
    
  })
}

shinyApp(ui = ui, server = server)
