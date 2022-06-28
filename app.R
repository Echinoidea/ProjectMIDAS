library(shiny) #library for running Shiny Webapp
library(shinyjs) #library for js functions
library(shinydashboard) #dashboarding library
library(readxl) #library for taking in XLS/XLSX
library(ggplot2) #library for basic plots
library(tidyverse) #megapackage for analysis/operations
library(readr) #library for taking in 

# Loading in CSV and creating autocomplete list -----
studentData <- read.csv("data/dummy_midas_data2.csv")
rStudentData <- reactiveValues(data = studentData)
selectedStudent <- reactiveValues(data = NULL)
autocomplete_list <- paste0(studentData$lastName, ",", studentData$firstName)


# *Tab definitions* -----
# schoolTab =====
schoolTab <- tabItem(tabName = "Dashboard",
                     actionButton("Warning", "Click Here To Upload School Files in Upload Files"),
                     fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                          plotOutput("totalBar"),
                                          plotOutput("socialBar"))
                              ),
                     fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                          plotOutput("academicBar"),
                                          plotOutput("emotionalBar"))
                              ),
                     fluidRow(splitLayout(cellwidths = c("50%", "50%"),
                                          radioButtons("level", "What Total Risk Levels should the Table display?",
                                                      c("All" = "alltotal",
                                                        "Low" = "lowtotal",
                                                        "Some" = "sometotal",
                                                        "High" = "hightotal")),
                                         radioButtons("specialed", "Show",
                                                      c("All Students" = "high",
                                                        "No Special Education" = "nspecialed",
                                                        "Only Special Education" = "yspecialed"))
                                         )
                              ),
                     div(style='height:100%; width:100%; overflow: scroll; background-color: #dce0b4',
                         tableOutput("contentsTable"))
)
# uploadTab =====
uploadTab <- tabItem(tabName = "Upload",
                     fluidRow(
                       box(width = 12,
                           fileInput("file1", "Please Upload File (.csv, .xls, .xlsx)",
                                     multiple = FALSE,
                                     accept = c("text/csv", ".xlsx",
                                                "text/comma-separated-values,text/plain",
                                                ".csv",
                                                ".xlsx")
                           )
                       )
                       #end of box
                     )
                     #end of fluidRow
)
# studentTab =====
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
                          
                          # Name search (changed to autocomplete to avoid crashes)
                          selectizeInput(
                            "txtinStudentName",
                            label = "Student Name (Last, First)",
                            choices = c("", autocomplete_list),
                            selected = '',
                            multiple = FALSE,
                            options = list(create = FALSE)
                          ),
                          actionButton("btnStudentName", label = "Search"),
                          br(), br(),
                          
                          # Student demographic data
                          fluidRow(
                            column(
                              12,
                              align = "center",
                              
                              p(tags$b("Gender")),
                              textOutput("studentGender"),
                              br(),
                              
                              p(tags$b("Ethnicity")),
                              textOutput("studentEthnicity"),
                              br(),
                              
                              p(tags$b("Grade")),
                              textOutput("studentGrade"),
                              br(),
                              
                              p(tags$b("Special Education")),
                              textOutput("studentSpecialEd")
                            )
                          )
                        ),
                        #MIDAS Assessments
                        column(
                          8,
                          div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 90%;")
                        ))
)

# classTab =====
classTab <- tabItem(tabName = "classTab",
                    column(
                      12,
                      div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 5%;",
                          p("Class Information To Go Here (Data by Class)"),
                          br(),
                          p("This should look similar to School View, but allow selections by teacher/class"))
                    ))
# archiveTab =====
archiveTab <- tabItem(tabName = "archiveTab",
                      column(
                        12,
                        div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 100%;")
                      ))
# faqTab =====
faqTab <- tabItem(tabName = "faqTab",
                  column(
                    12,
                    div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 100%;",
                        p("FAQ:"))
                  ))
# interpretTab =====
interpretTab <- tabItem(tabName = "interpretTab",
                        column(
                          12,
                          div(style = "background-color: #d0df92; padding: 10px; border-radius: 25px; height: 100%;",
                              HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/0x5OG2yBSF8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
                        ))


# *Core Shiny UI and Server* -----
# UI -----
ui <- dashboardPage(
  dashboardHeader(
    title = "Project MIDAS"
  ),
  # Sidebar =====
  sidebar = dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Upload Data", tabName = "Upload", icon = icon("upload")),
                menuItem("School", tabName = "Dashboard", icon = icon("school")),
                menuItem("Class", tabName = "classTab", icon = icon("users")),
                menuItem("Student", tabName = "studentTab", icon = icon("user-graduate")),
                menuItem("Archive", tabName = "archiveTab", icon = icon("archive")),
                menuItem("Frequently Asked Questions", tabName = "faqTab", icon = icon("question-circle-o")),
                menuItem("How To Interpret This Data", tabName = "interpretTab", icon = icon("info"))
    )
  ),
  
  # Body =====
  body = dashboardBody(
    shinyjs::useShinyjs(),
    fluidPage(
      tabItems(
        #Upload - Uploading Data to be sent to Dashboard
        uploadTab,
        #School - Displaying School Data from Upload
        schoolTab,
        #Student - Displaying student data
        studentTab,
        #Class - Displaying class data
        classTab,
        #Archive - under construction?
        archiveTab,
        #FAQ - Frequently Asked Questions (to be filled out by College of Ed.)
        faqTab,
        #Intepret - How the data is interpreted (to contain videos by CoE)
        interpretTab
      )
      #end of tabs
    )
  )
)

# Server -----
server <- function(input, output, session) {
  #Risk minimums reflect total score minimums from fastbridge site Thomas sent.
  lowRiskMin <- 37
  someRiskMin <- 24
  first <- reactiveVal()
  last <- reactiveVal()
  
  observeEvent(input$Warning, {
    updateTabItems(session, "tabs", selected = "Upload")
  })
  
  observeEvent(input$file1, {
    removeUI( selector = "#Warning",
              multiple = F
              )
    updateTabItems(session, "tabs", selected = "Dashboard")
  })
  
  observeEvent(input$btnStudentName, {
    last(strsplit(input$txtinStudentName, ",")[[1]][1])
    first(str_trim(strsplit(input$txtinStudentName, ",")[[1]][2]))
    selectedStudent$data <- subset(rStudentData$data, lastName == last() & firstName == first())
  })
  
  output$studentGender <- renderText({
    selectedStudent$data$gender
  })
  
  output$studentImage <- renderImage({
    # Load student image
    filename <- normalizePath(file.path('assets/images',
                                        paste('student-clip-art', '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image not available"))
  }, deleteFile = FALSE)
  
  output$studentEthnicity <- renderText({
    selectedStudent$data$ethnicity
  })
  
  output$studentGrade <- renderText({
    selectedStudent$data$grade
  })
  
  output$studentSpecialEd <- renderText({
    selectedStudent$data$specialEducation
  })
  
  output$totalBar <- renderPlot({
    req(input$file1)
    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
    df_tbrange <- df %>% mutate(ranges = cut(totalBehavior, c(0, 24, 37, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_tbrange
    totalbar <- barplot(df_tbrange$n, main = "Total Behavior Distribution", 
                        xlab="Percentage of School",
                        col="darkred",
                        names.arg=c("High Risk", "Some Risk", "Low Risk")
    )
  })
  
  output$socialBar <- renderPlot({
    req(input$file1)
    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
    df_sbrange <- df %>% mutate(ranges = cut(socialBehavior, c(0, 9, 12, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_sbrange
    totalbar <- barplot(df_sbrange$n, main = "Social Behavior Distribution", 
                        xlab="Percentage of School",
                        col="darkred",
                        names.arg=c("High Risk", "Some Risk", "Low Risk")
    )
  })
  
  output$academicBar <- renderPlot({
    req(input$file1)
    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
    df_abrange <- df %>% mutate(ranges = cut(academicBehavior, c(0, 6, 9, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_abrange
    totalbar <- barplot(df_abrange$n, main = "Academic Behavior Distribution", 
                        xlab="Percentage of School",
                        col="darkred",
                        names.arg=c("High Risk", "Some Risk", "Low Risk")
    )
  })
  
  output$emotionalBar <- renderPlot({
    req(input$file1)
    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
    df_ebrange <- df %>% mutate(ranges = cut(emotionalBehavior, c(0, 7, 10, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_ebrange
    totalbar <- barplot(df_ebrange$n, main = "Emotional Behavior Distribution", 
                        xlab="Percentage of School",
                        col="darkred",
                        names.arg=c("High Risk", "Some Risk", "Low Risk")
    )
  })
  
  output$contentsTable <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, all rows that reflect the selected risk level will be shown.
    
    req(input$file1)
    
    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
    options = list(scrollX = TRUE)
    # This switch is used for demo purposes, needs to be changed for our dashboarding needs
    switch(input$level,
           # Depending on radiobutton selected, the returned table reflects one risk group.
           "lowtotal" = return(df[df$totalBehavior > lowRiskMin,]),
           "sometotal" = return(df[df$totalBehavior > someRiskMin & df$totalBehavior < lowRiskMin,]),
           "hightotal" = return(df[df$totalBehavior < someRiskMin,]),
           "alltotal" = return(df))
  })
}



# *Shiny Initialization* ---- 
shinyApp(ui = ui, server = server)