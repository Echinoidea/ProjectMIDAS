#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) #library for running Shiny Webapp
library(shinyjs) #library for js functions
library(shinydashboard) #dashboarding library
library(readxl) #library for taking in XLS/XLSX
library(ggplot2) #library for basic plots
library(tidyverse) #megapackage for analysis/operations
library(readr) #library for taking in 
#library(shinyWidgets)
options(spinner.color="white")

#load data from .csv file 
selectedStudent <- reactiveValues(data = NULL)

# uploadTab =====
uploadTab <- tabItem(tabName = "Upload",
                     fluidRow(
                       box(width = 12,
                           fileInput("file1", "Please Upload File (.csv, .xls, .xlsx)",
                                     multiple = FALSE,
                                     accept = c("text/csv", ".xls",
                                                "text/comma-separated-values,text/plain",
                                                ".csv",
                                                ".xlsx")
                           )
                       )
                       #end of box
                     )
                     #end of fluidRow
)

studentTab <- tabItem(tabName = "studentTab",
# This column contains: student image, name, age, gender, ethnicity, grade, and special ed status
  fluidRow(
    #white padding
    column(
      12,
      style = "background-color:white; padding: 15px; border-radius: 25px; height: 100%; ",
      
      column(
        4,
        style = "background-color: #d0df92; padding: 100px; border-radius: 25px; height: 100%; ",
        align = "center",
        # Center and size for image
        tags$head(
          tags$style(
            type = "text/css",
            "#studentImage img {max-width: 100%; width: 100%, height: auto;}"
            )
          ),
        # Image
        div(style = "height: 100px; width: 100px;", 
          imageOutput("studentImage")),
          br(), 
          br(),
                              
        # Name search (changed to autocomplete to avoid crashes)
        textInput(
          "txtinStudentName",
          label = "Student ID",
          placeholder = "   s###"
        ),
        actionButton("btnStudentName", label = "Search"),
        br(), br(),
          
      
          # Student demographic data
            fluidRow(
              column(
                12,
                align = "center",
                p(tags$b("First Name")),
                textOutput("studentFirstName"),
                br(),
                
                p(tags$b("Last Name")),
                textOutput("studentLastName"),
                br(),
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
              ),
            
            )
          ),
      column(1),
        #MIDAS Assessments
        column(
          7,
          style = "background-color:#d0df92; padding: 15px; border-radius: 25px; height: 100%; ",
          
          verticalLayout(
            column(
              12,
              
              column(2,
                     p(tags$b("Test Score"))),
              column(5,style = "background-color:white; padding: 20px; border-radius: 25px; height: 100%; border-style: solid;",
                     align = "center")
              
              
            ),
            
            
            br(),
            p(tags$b("SAEBRS-TRS")),
            column(
              4,
              style = "background-color:white; padding: 20px; border-radius: 25px; width:100%; height: 100%; border-style: solid;",
              align = "center",
              splitLayout
              (
                column(
                  4,
                  p(tags$b("Total TRS")),
                  textOutput("TotalTRS"),
                  br(),
                  p(tags$b("Social TRS")),
                  textOutput("SocialTRS"),
                  br(),
                  p(tags$b("Social TRS")),
                  textOutput("AcademicTRS"),
                  br(),
                  p(tags$b("Social TRS")),
                  textOutput("EmotionalTRS")
                ),
                plotOutput("trstotalBar")
              )
            ),
            br(),
            p(tags$b("mySAEBRS")),
            column(
              4,
              style = "background-color:white; padding: 20px; border-radius: 25px;width:100%; height: 100%; border-style: solid;",
              align = "center",
              splitLayout
              (
                column(
                  4,
                  p(tags$b("Total")),
                  textOutput("TotalSAEBRS"),
                  br(),
                  p(tags$b("Social")),
                  textOutput("SocialSAEBRS"),
                  br(),
                  p(tags$b("Social")),
                  textOutput("AcademicSAEBRS"),
                  br(),
                  p(tags$b("Social")),
                  textOutput("EmotionalSAEBRS")
                ),
                plotOutput("totalBar")
              )
            ),fluid = TRUE
          )
        )
    )
  )
)

# schoolTab =====
schoolTab <- tabItem(tabName = "schoolTab",
                     actionButton("Warning", "Click Here To Upload School Files in Upload Files"),
                     # 4-4 split bars, 
  column(
    12,
    style = "background-color:white; padding: 15px; border-radius: 25px; height: 100%; ",
    verticalLayout(
      p(tags$b("SAEBRS-TRS")),
      column(
        12,
        style = "background-color: #d0df92; padding: 5px; border-radius: 25px; wight:100% height: 90%;",
        fluidRow(
          splitLayout(
            splitLayout(
              plotOutput("trssocialBar"),
              plotOutput("trsacademicBar"),
              plotOutput("trsemotionalBar")
              )))
      ),
      br(),
      p(tags$b("mySAEBRS")),
      column(
        12,
        style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 90%;",
        
        ),
      br(),
      br()
    )
  )
)
 
faqTab <- tabItem(tabName = "faqTab",
                  fluidRow(
                  column(
                    12,
                    style = "background-color: #white; padding: 5px; border-radius: 25px; height: 100%;",
                    column(
                      12,
                      style = "background-color: #white; padding: 5px; border-radius: 25px; height: 100%;",
                        p("FAQ:"),
                        uiOutput("faqtext")
                    
                     )
                    )
                  ) 
)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(
  
    dropdownMenu(
      type = "tasks",
      headerText = "Help",
      icon = icon("question"),
      badgeStatus = NULL,
      notificationItem(
        text ="\tAbout MIDAS",
        icon = icon("child")
      ),
      notificationItem(
        text ="\tAbout SAEBRS",
        icon = icon("child")
      )
    ),
    title  =div(img(src="midaslogo.png",height=30), "  ")), #insert image into title
    
# Sidebar =====
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Student dashboard", tabName = "studentTab",icon = icon("child")),
    menuItem("School dashboard", tabName = "schoolTab", icon = icon("school")),
    menuItem("FAQ", tabName = "faqTab", icon = icon("question")),
    menuItem("Upload Data", tabName = "Upload", icon = icon("upload"),selected = TRUE)
    )
  ),
  body <- dashboardBody(
    shinyjs::useShinyjs(),
    #fonts
    tags$head(
      includeCSS("www/css.css")
    ),
    
    tags$style(HTML('body {font-family:"Poppins"}')),
    #color the body
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #d0df92;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #d0df92;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #d0df92;
                              }        
        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #d0df92;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #d0df92;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #d0df92;
                              color: black;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #d0df92;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #d0df92;
         }
        /* body */
        .content-wrapper, .right-side {
                                background-color: #d0df92;
                                }
                              '))),
  
    fluidPage(
      
      tabItems(
        #Upload - Uploading Data to be sent to Dashboard
        uploadTab,
        #School - Displaying School Data from Upload
        
        #Student - Displaying student data
        studentTab,
        #School - Displaying school data
        schoolTab,
        #FAQ
        faqTab
       
      )
      #end of tabs
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  #Risk minimums reflect total score minimums from fastbridge site Thomas sent.
  lowRiskMin <- 37
  someRiskMin <- 24
  first <- reactiveVal()
  last <- reactiveVal()
  
  #On Clicking warning button, sends user to upload page.
  observeEvent(input$Warning, {
    updateTabItems(session, "tabs", selected = "Upload")
  })
  observeEvent(input$WarningStudent, {
    updateTabItems(session, "tabs", selected = "Upload")
  })
  
  #On Upload, sends user to School Page.
  observeEvent(input$file1, {
    removeUI( selector = "#Warning",
              multiple = F
    )
    removeUI( selector = "#WarningStudent",
              multiple = F
    )
    updateTabItems(session, "tabs", selected = "studentTab")
  })
  
  #On Upload, distributes this dataframe to all necessary functions at once.
  df = eventReactive(input$file1, {
    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
  })
  
  #On user searching a name, find student in the user's provided data and assign it for display
  observeEvent(input$btnStudentName, {
    selectedStudent$data <- subset(df(), sID == input$txtinStudentName)
  })
  
  output$faqtext <- renderUI({
    filename <- normalizePath(file.path('www', paste('faq', '.txt', sep='')))
    fileText <- readLines(filename)
    splitText <- stringi::stri_split(str = fileText, regex = '\\n')
    replacedText <- lapply(splitText, p)
    replacedText
  })
  
  #Student Demographic Characteristics as text output.
  output$studentGender <- renderText({
    selectedStudent$data$gender
  })
  output$studentImage <- renderImage({
    # Load student image
    filename <- normalizePath(file.path('www',
                                        paste('student', '.png', sep='')))
    
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
  output$studentFirstName <- renderText({
    selectedStudent$data$firstName
  })
  output$studentLastName <- renderText({
    selectedStudent$data$lastName
  })
  
  output$SocialTRS <- renderText({
    selectedStudent$data$socialBehavior
  })
  output$AcademicTRS <- renderText({
    selectedStudent$data$academicBehavior
  })
  output$EmotionalTRS <- renderText({
    selectedStudent$data$emotionalBehavior
  })
  #TRS VALUES
  output$SocialTRS <- renderText({
    selectedStudent$data$TRSsocialBehavior
  })
  output$AcademicTRS <- renderText({
    selectedStudent$data$TRSacademicBehavior
  })
  output$EmotionalTRS <- renderText({
    selectedStudent$data$TRSemotionalBehavior
  })
  #mySAEBRS values
  output$SocialSAEBRS <- renderText({
    selectedStudent$data$socialBehavior
  })
  output$AcademicSAEBRS <- renderText({
    selectedStudent$data$academicBehavior
  })
  output$EmotionalSAEBRS <- renderText({
    selectedStudent$data$emotionalBehavior
  })
  
  #School-wide TRS bargraph outputs
  output$trstotalBar <- renderPlot({
    df_abrange <- df() %>% mutate(ranges = cut(academicBehavior, c(-1, 6, 9, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_abrange
    studenttotalplot <- ggplot(df_abrange, aes(x = ranges, y = n)) +
      geom_bar(stat = 'identity', aes(fill=ranges)) +
      geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none") + 
      ggtitle("TRS-TOTAL Avarage Score") + 
      xlab("Risk Levels") + ylab("Number of Students") + 
      scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk")+ 
                         theme(
                           panel.grid.minor = element_blank(), 
                           panel.grid.major = element_blank(),
                           panel.background = element_blank(),
                           plot.background = element_blank()
                         ))
    return(studenttotalplot)
  },bg="transparent")
  output$trssocialBar <- renderPlot({
    df_sbrange <- df() %>% mutate(ranges = cut(TRSsocialBehavior, c(-1, 9, 12, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    studenttotalplot <- ggplot(df_sbrange, aes(x = ranges, y = n)) +
      geom_bar(stat = 'identity', aes(fill=ranges)) +
      geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none") + 
      ggtitle("TRS Social Score Distribution") + 
      xlab("Risk Levels") + ylab("Number of Students") + 
      scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk"))
    return(studenttotalplot)
  })
  output$trsacademicBar <- renderPlot({
    df_abrange <- df() %>% mutate(ranges = cut(TRSacademicBehavior, c(-1, 6, 9, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    studenttotalplot <- ggplot(df_abrange, aes(x = ranges, y = n)) +
      geom_bar(stat = 'identity', aes(fill=ranges)) +
      geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none") + 
      ggtitle("TRS Academic Score Distribution") + 
      xlab("Risk Levels") + ylab("Number of Students") + 
      scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk"))
    return(studenttotalplot)
  })
  output$trsemotionalBar <- renderPlot({
    df_ebrange <- df() %>% mutate(ranges = cut(TRSemotionalBehavior, c(-1, 7, 10, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    studenttotalplot <- ggplot(df_ebrange, aes(x = ranges, y = n)) +
      geom_bar(stat = 'identity', aes(fill=ranges)) +
      geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none") + 
      ggtitle("TRS Emotional Score Distribution") + 
      xlab("Risk Levels") + ylab("Number of Students") + 
      scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk"))
    return(studenttotalplot)
  })
  
  #School-wide MySAEBRS bargraph outputs
  output$totalBar <- renderPlot({
    df_tbrange <- df() %>% mutate(ranges = cut(totalBehavior, c(-1, 24, 37, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    studenttotalplot <- ggplot(df_tbrange, aes(x = ranges, y = n)) +
      geom_bar(stat = 'identity', aes(fill=ranges)) +
      geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none") + 
      ggtitle("MySAEBRS Total Score Distribution") + 
      xlab("Risk Levels") + ylab("Number of Students") + 
      scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk"))
    return(studenttotalplot)
  })
  output$socialBar <- renderPlot({
    df_sbrange <- df() %>% mutate(ranges = cut(socialBehavior, c(-1, 9, 12, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    studenttotalplot <- ggplot(df_sbrange, aes(x = ranges, y = n)) +
      geom_bar(stat = 'identity', aes(fill=ranges)) +
      geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none") + 
      ggtitle("MySAEBRS Social Score Distribution") + 
      xlab("Risk Levels") + ylab("Number of Students") + 
      scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk"))
    return(studenttotalplot)
  })
  output$academicBar <- renderPlot({
    df_abrange <- df() %>% mutate(ranges = cut(academicBehavior, c(-1, 6, 9, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_abrange
    studenttotalplot <- ggplot(df_abrange, aes(x = ranges, y = n)) +
      geom_bar(stat = 'identity', aes(fill=ranges)) +
      geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none") + 
      ggtitle("MySAEBRS Academic Score Distribution") + 
      xlab("Risk Levels") + ylab("Number of Students") + 
      scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk")+ 
                         theme(
                           panel.grid.minor = element_blank(), 
                           panel.grid.major = element_blank(),
                           panel.background = element_blank(),
                           plot.background = element_blank()
                         ))
    return(studenttotalplot)
  },bg="transparent")
  output$emotionalBar <- renderPlot({
    df_ebrange <- df() %>% mutate(ranges = cut(emotionalBehavior, c(-1, 7, 10, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_ebrange
    studenttotalplot <- ggplot(df_ebrange, aes(x = ranges, y = n)) +
      geom_bar(stat = 'identity', aes(fill=ranges)) +
      geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none") + 
      ggtitle("MySAEBRS Emotional Score Distribution") + 
      xlab("Risk Levels") + ylab("Number of Students") + 
      scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk"))
    return(studenttotalplot)
  })
  
  #School-wide MySAEBRS Scores as reactive functions. 
  low_total_MS <- reactive({
    tmp <- df()
    tmp[tmp$totalBehavior > lowRiskMin,]
  })
  some_total_MS <- reactive({
    tmp <- df()
    tmp[tmp$totalBehavior > someRiskMin & tmp$totalBehavior < lowRiskMin,]
  })
  high_total_MS <- reactive({
    tmp <- df()
    tmp[tmp$totalBehavior < someRiskMin,]
  })
  all_total_MS <- reactive({
    tmp <- df()
  })
  
  #School-wide TRS Scores as reactive functions.
  low_total_TRS <- reactive({
    tmp <- df()
    tmp[tmp$TRStotalBehavior > lowRiskMin,]
  })
  some_total_TRS <- reactive({
    tmp <- df()
    tmp[tmp$TRStotalBehavior > someRiskMin & tmp$TRStotalBehavior < lowRiskMin,]
  })
  high_total_TRS <- reactive({
    tmp <- df()
    tmp[tmp$TRStotalBehavior < someRiskMin,]
  })
  all_total_TRS <- reactive({
    tmp <- df()
  })
  
  #Selectable dataframe display of CSV contents
  output$contentsTable <- renderTable({
    options = list(scrollX = TRUE)
    # if a specific grade is selected, subset the list with that grade as a conditional.
    if(input$grade != "allgrades"){
      switch(input$level,
             # Depending on radiobutton selected, the returned table reflects one risk group.
             "lowtotalms" = return(low_total_MS()[low_total_MS()$grade == input$grade,]),
             "sometotalms" = return(some_total_MS()[some_total_MS()$grade == input$grade,]),
             "hightotalms" = return(high_total_MS()[high_total_MS()$grade == input$grade,]),
             "alltotalms" = return(all_total_MS()[all_total_MS()$grade == input$grade,])
      )
    }
    # if all grades are selected, simply return MS without conditional
    else{
      switch(input$level,
             # Depending on radiobutton selected, the returned table reflects one risk group.
             "lowtotalms" = return(low_total_MS()),
             "sometotalms" = return(some_total_MS()),
             "hightotalms" = return(high_total_MS()),
             "alltotalms" = return(all_total_MS())
      )
    }
    
  })
  
  
  
  #Student MySAEBRS and TRS bargraph outputs
  output$saeberstudentBar <- renderPlot({
    #ensure the user has uploaded data and selected a student before attempting a plot render
    validate(need(!is.null(selectedStudent$data), ''))
    #establish categories for plotting, then subset the selected student data with these categories.
    vars <- c("totalBehavior", "emotionalBehavior", "academicBehavior", "socialBehavior")
    MySAEBRSstats <- selectedStudent$data[vars]
    #convert df to long df with category and corresponding score as our columns.
    long_df <- MySAEBRSstats %>% gather(Category, Score)
    #create ggplot with data, coloring, adjusting axis data and data presentation.
    studenttotalplot <- ggplot(long_df, aes(x = Category, y = Score)) +
      geom_bar(stat = 'identity', aes(fill=Category)) +
      geom_text(aes(label=Score),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none") + 
      ggtitle("mySAEBER Scores") + 
      scale_x_discrete(labels=c("Total", "Emotional", "Academic", "Social"))
    return(studenttotalplot)
  })
  output$trsstudentBar <- renderPlot({
    #ensure the user has uploaded data and selected a student before attempting a plot render
    validate(need(!is.null(selectedStudent$data), ''))
    #establish categories for plotting, then subset the selected student data with these categories.
    vars <- c("TRStotalBehavior", "TRSemotionalBehavior", "TRSacademicBehavior", "TRSsocialBehavior")
    trsstats <-  selectedStudent$data[vars]
    #convert df to long df with category and corresponding score as our columns.
    long_df <- trsstats %>% gather(Category, Score)
    #create ggplot with data, coloring, adjusting axis data and data presentation.
    studenttotalplot <- ggplot(long_df, aes(x = Category, y = Score)) +
      geom_bar(stat = 'identity', aes(fill=Category)) +
      geom_text(aes(label=Score),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none", axis.title.x = element_blank()) + 
      ggtitle("TRS Scores") + 
      scale_x_discrete(labels=c("Total", "Emotional", "Academic", "Social"))
    return(studenttotalplot)
  })
  output$tableTRS <- renderTable(iris)
}
# Run the application 
shinyApp(ui = ui, server = server)
