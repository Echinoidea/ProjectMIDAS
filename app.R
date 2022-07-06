library(shiny) #library for running Shiny Webapp
library(reshape2) #library for dataframe and matrix operations
library(stringi) #library for string operations
library(shinyjs) #library for js functions
library(shinydashboard) #dashboarding library
library(readxl) #library for taking in XLS/XLSX
library(ggplot2) #library for basic plots
library(tidyverse) #megapackage for analysis/operations
library(readr) #library for taking in CSV

# Loading in CSV and creating autocomplete list -----
#studentData <- read.csv("data/dummy_midas_data2.csv") #attempting to remove this line.
selectedStudent <- reactiveValues(data = NULL)
#autocomplete_list <- paste0(studentData$lastName, ",", studentData$firstName)
#rStudentData <- reactiveValues(data = studentData)

# *Tab definitions* -----
# schoolTab =====
schoolTab <- tabItem(tabName = "Dashboard",
                     actionButton("Warning", "Click Here To Upload School Files in Upload Files"),
                     # 4-4 split bars, 
                     fluidRow(splitLayout(plotOutput("trstotalBar"),
                                          plotOutput("totalBar"))
                              ),
                     fluidRow(splitLayout(splitLayout(box(background = ,plotOutput("trssocialBar"),width = 12),
                                                      plotOutput("trsacademicBar"),
                                                      plotOutput("trsemotionalBar")),
                                          splitLayout(plotOutput("socialBar"),
                                                      plotOutput("academicBar"),
                                                      plotOutput("emotionalBar"))
                                          )
                              ),
                     fluidRow(splitLayout(cellwidths = c("50%", "50%"),
                                          radioButtons("level", "Show ___ MySAEBRS Risk Levels",
                                                      c("All" = "alltotalms",
                                                        "Low" = "lowtotalms",
                                                        "Some" = "sometotalms",
                                                        "High" = "hightotalms")),
                                          #To-Do: Grade Level Selection(6,7,8 grade)
                                         radioButtons("grade", "Show",
                                                      c("All Grades" = "allgrades",
                                                        "6th Grade" = "6",
                                                        "7th Grade" = "7",
                                                        "8th Grade" = "8"))
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
# studentTab =====


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
                                12,
                                style = "background-color:white; padding: 20px; border-radius: 25px; width:100%; height: 100%; border-style: solid;",
                                align = "center",
                                splitLayout
                                (
                                  
                                    
                                    verticalLayout(fluid = TRUE,
                                      valueBoxOutput("totalBox"),
                                  
                                    
                                    
                                    
                                      valueBoxOutput("socialBox"),
                                   
                                    
                                    
                                    
                                    
                                      valueBoxOutput("emotionalBox"),
                                   
                                    
                                   # p(tags$b("Social TRS")),
                                   # textOutput("AcademicTRS"),
                                   
                                   
                                   valueBoxOutput("academicBox"),
                                  
                                   
                                   
                                   # p(tags$b("Academic TRS")),
                                   # textOutput("AcademicTRS")
                                    ),
                                  
                                  box(background = "maroon", plotOutput("trsstudentBar"),width = 12)
                                  )
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
                                  box(background = "maroon", plotOutput("saeberstudentBar"),width = 12)
                                )
                              ),fluid = TRUE
                            )
                          )
                        )
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
                        p("FAQ:"),
                        uiOutput("faqtext"))
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
  sidebar = dashboardSidebar(
    sidebarMenu(id = "tabs", 
                menuItem("Upload Data", tabName = "Upload", icon = icon("upload")),
                menuItem("School", tabName = "Dashboard", icon = icon("school")),
                
                menuItem("Student", tabName = "studentTab", icon = icon("user-graduate")),
               
                menuItem("Frequently Asked Questions", tabName = "faqTab", icon = icon("question-circle")),
                menuItem("How To Interpret This Data", tabName = "interpretTab", icon = icon("info"))
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
        schoolTab,
        #Student - Displaying student data
        studentTab,
        #Class - Displaying class data
        #classTab,
        #Archive - under construction?
        #archiveTab,
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
    updateTabItems(session, "tabs", selected = "Dashboard")
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
    filename <- normalizePath(file.path('data', paste('faq', '.txt', sep='')))
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
  
  #School-wide TRS bargraph outputs
  output$trstotalBar <- renderPlot({
    df_tbrange <- df() %>% mutate(ranges = cut(TRStotalBehavior, c(-1, 24, 37, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    studenttotalplot <- ggplot(df_tbrange, aes(x = ranges, y = n)) +
      theme_classic() +
      geom_bar(stat = 'identity', aes(fill=ranges)) +
      geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.25) +
      theme(legend.position = "none") + 
      ggtitle("TRS Total Score Distribution") + 
      xlab("Risk Levels") + ylab("Number of Students") + 
      scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk"))
    return(studenttotalplot)
  })
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
      scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk"))
    return(studenttotalplot)
  })
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
                        scale_x_discrete(labels=c("Social", "Academic", "Emotional", "Total"))
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
                        scale_x_discrete(labels=c("Social", "Academic", "Emotional", "Total"))
    return(studenttotalplot)
  })
  
  #value boxes
  output$totalBox <- renderValueBox({
    valueBox(
      selectedStudent$data$TRStotalBehavior, "Total TRS", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$socialBox <- renderValueBox({
    valueBox(
      selectedStudent$data$TRSsocialBehavior, "Social TRS", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$academicBox <- renderValueBox({
    valueBox(
      selectedStudent$data$TRSacademicBehavior, "Academic TRS", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$emotionalBox <- renderValueBox({
    valueBox(
      selectedStudent$data$TRSemotionalBehavior, "Emotional TRS", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
}



# *Shiny Initialization* ---- 
shinyApp(ui = ui, server = server)