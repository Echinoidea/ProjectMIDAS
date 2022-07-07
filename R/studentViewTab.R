# Loading in CSV and creating autocomplete list -----
# studentData <- read.csv("data/dummy_midas_data2.csv")
# studentData$ID <- sample(100:999, 50, replace = FALSE)

studentViewTabUI <- function(id, uploadedData) {
  ns <- NS(id)
  
  tabItem(tabName = "studentTab",
          # Student ID, Demographics, and Test Score header box, divided into 6 columns
          fluidRow(
            # ID Search Box
            box(
              title = "Student ID#",
              fluidRow(
                column(
                  8,
                  style = "padding-top: 5px; padding-left:10px; padding-bottom:1px; padding-right: 1px;",
                  uiOutput(NS(id, "inputStudentID"))
                ),
                column(
                  4,
                  offset = 0,
                  style = "padding:5px; padding-bottom:1px;",
                  actionButton(NS(id, "btnStudentID"), label = "Search")
                )
              ),
              
              width = 2
            ),
            
            # Grade box
            box(
              title = "Grade",
              textOutput(NS(id, "studentGrade")),
              width = 2
            ),
            
            # Gender box
            box(
              title = "Gender",
              textOutput(NS(id, "studentGender")),
              width = 2
            ),
            
            # Ethnicity box
            box(
              title = "Ethnicity",
              textOutput(NS(id, "studentEthnicity")),
              width = 2
            ),
            
            # Special Education Status box
            box(
              title = "Special Education",
              textOutput(NS(id, "studentSpecialEd")),
              width = 2
            ),
            
            # Test score box
            box(
              title = "Test Score",
              textOutput(NS(id, "studentTestScore")),
              width = 2
            )
          ),
          
          fluidRow(
            # SAEBRS-TRS
            column(
              6,
              verticalLayout(
                box(
                  title = "SAEBRS-TRS",
                  fluidRow(
                    valueBoxOutput(NS(id, "trsTotalBox"), width = 12)
                  ),
                  fluidRow(
                    valueBoxOutput(NS(id, "trsSocialBox")),
                    valueBoxOutput(NS(id, "trsAcademicBox")),
                    valueBoxOutput(NS(id, "trsEmotionalBox"))
                  ),
                  width = 12
                ),
                # SAEBRS-TRS barplot
                box(
                  title = "TRS-TOTAL Average Score",
                  plotOutput(NS(id, "trsTotalBar")),
                  width = 12
                )
              )
            ),
            # MySAEBRS
            column(
              6,
              verticalLayout(
                box(
                  title = "MySAEBRS",
                  fluidRow(
                    valueBoxOutput(NS(id, "myTotalBox"), width = 12)
                  ),
                  fluidRow(
                    valueBoxOutput(NS(id, "mySocialBox")),
                    valueBoxOutput(NS(id, "myAcademicBox")),
                    valueBoxOutput(NS(id, "myEmotionalBox"))
                  ),
                  width = 12
                ),
                # SAEBRS-TRS barplot
                box(
                  title = "MySAEBRS - Total Score Distribution",
                  plotOutput(NS(id, "myTotalBar")),
                  width = 12
                )
              )
            )
          )
          #             p(tags$b("Total")),
          #             textOutput(NS(id, "totalSAEBRS")),
          #             
          #             br(),
          #             
          #             p(tags$b("Social")),
          #             textOutput(NS(id, "socialSAEBRS")),
          #             
          #             br(),
          #             
          #             p(tags$b("Social")),
          #             textOutput(NS(id, "academicSAEBRS")),
          #             
          #             br(),
          #             
          #             p(tags$b("Social")),
          #             textOutput(NS(id, "emotionalSAEBRS"))
          #           ),
          #           plotOutput(NS(id, "totalBar"))
          # 
          
          # SAEBRS-TRS and MySAEBRS Scores and Graphs
          # fluidRow(
          #   
          # )
          # 
          # 
          # fluidRow(
          #   column(
          #     4,
          #     #style = "background-color: #d0df92; padding: 5px; border-radius: 25px;",
          #     style = "background-color: #CCEBC5; padding: 15px;",
          #     align = "center",
          #     
          #     # Render the selectizeInput from the server function to allow accessing uploadedData() IDs
          #     uiOutput(NS(id, "inputStudentID")),
          #     
          #     actionButton(NS(id, "btnStudentID"), label = "Search"),
          #     
          #     br(),
          #     br(),
          #     
          #     box(
          #       title = "Gender",
          #       textOutput(NS(id, "studentGender"))
          #     ),
          #     box(
          #       title = "Ethnicity",
          #       textOutput(NS(id, "studentEthnicity"))
          #     ),
          #     box(
          #       title = "Grade",
          #       textOutput(NS(id, "studentGrade"))
          #     ),
          #     box(
          #       title = "Special Education",
          #       textOutput(NS(id, "studentSpecialEd"))
          #     )
          #   ),
          #   
          #   #MIDAS Assessments
          #   column(
          #     8,
          #     #style = "background-color:#d0df92; padding: 15px; border-radius: 25px; height: 100%;",
          #     style = "background-color: #CCEBC5; padding: 15px;",
          #     
          #     verticalLayout(
          #       column(
          #         12,
          #         
          #         column(2,
          #                p(tags$b("Test Score"))),
          #         column(10,
          #                style = "background-color:white; padding: 20px; border-radius: 25px; height: 100%; border-style: solid;",
          #                align = "center")
          #       ),
          #       
          #       br(),
          #       
          #       fluidRow(
          #         # SAEBRS-TRS scores valueBoxes
          #         box(
          #           title = "SAEBRS-TRS",
          #           
          #           fluidRow(
          #             valueBoxOutput(NS(id, "trsTotalBox"), width = 12)
          #           ),
          #           
          #           fluidRow(
          #             valueBoxOutput(NS(id, "trsSocialBox")),
          #             valueBoxOutput(NS(id, "trsAcademicBox")),
          #             valueBoxOutput(NS(id, "trsEmotionalBox"))
          #           ) 
          #         ),
          #         
          #         # SAEBRS-TRS barplot
          #         box(
          #           plotOutput(NS(id, "trsTotalBar"))
          #         )
          #       ),
          #       
          #       br(),
          #       
          #       p(tags$b("mySAEBRS")),
          #       
          #       column(
          #         12,
          #         style = "background-color:white; padding: 20px; border-radius: 25px;width:100%; height: 100%; border-style: solid;",
          #         align = "center",
          #         
          #         splitLayout
          #         (
          #           column(
          #       
          #         )
          #       ),fluid = TRUE
          #     )
          #   )
          # )
          )
}

studentViewTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
    # Render dynamic UI elements on load ----
    output$inputStudentID <- renderUI({
      selectizeInput(
        NS(id, "txtinStudentID"),
        label = NULL,
        choices = uploadedData()$ID,
        selected = NULL,
        multiple = FALSE,
        options = list(
          create = FALSE
        )
      )
      # textInput(
      #   NS(id, "txtinStudentID"),
      #   label = NULL,
      #   placeholder = "S###"
      # )
    })

    selectedStudent <- reactiveValues(data = NULL)
    
    searchID <- reactiveVal()
    
    observeEvent(input$btnStudentID, {
      print(uploadedData()$ID)
      searchID(input$txtinStudentID)
      print(searchID())
      selectedStudent$data <- subset(uploadedData(), ID == searchID())
      print(selectedStudent$data$firstName)
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
    
    # ---- RENDER TRS SCORE BOXES ----
    
    output$trsTotalBox <- renderValueBox({
      valueBox(
        selectedStudent$data$trsTotalBehavior,
        "TRS Total",
        width = 12,
        color = "olive"
      )
    })
    
    output$trsSocialBox <- renderValueBox({
      valueBox(
        selectedStudent$data$trsSocialBehavior,
        "TRS Social",
        color = "green"
      )
    })
    
    output$trsAcademicBox <- renderValueBox({
      valueBox(
        selectedStudent$data$trsAcademicBehavior,
        "TRS Academic",
        color = "green"
      )
    })
    
    output$trsEmotionalBox <- renderValueBox({
      valueBox(
        selectedStudent$data$emotionalBehavior,
        "TRS Emotional",
        color = "green"
      )
    })
    
    
    # ---- RENDER MySAEBRS BOXES ----
    
    output$socialTRS <- renderText({
      selectedStudent$data$trsSocialBehavior
    })
    
    output$academicTRS <- renderText({
      selectedStudent$data$trsAcademicBehavior
    })
    
    output$emotionalTRS <- renderText({
      selectedStudent$data$trsEmotionalBehavior
    })
    
    output$socialSAEBRS <- renderText({
      selectedStudent$data$socialBehavior
    })
    
    output$academicSAEBRS <- renderText({
      selectedStudent$data$academicBehavior
    })
    
    output$emotionalSAEBRS <- renderText({
      selectedStudent$data$emotionalBehavior
    })
      # 
      # #School-wide TRS bargraph outputs
    output$trsTotalBar <- renderPlot({
      df_abrange <- uploadedData() %>% 
        mutate(ranges = cut(academicBehavior, c(-1, 6, 9, Inf))) %>%
        group_by(ranges) %>% 
        tally() %>% 
        as.data.frame()
      
      df_abrange
      
      ggplot(df_abrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill=ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width=0.9),
                  vjust = -0.25) +
        labs(title = "", 
             x = "Risk Levels", 
             y = "Number of Students") +
        scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk")) +
        scale_fill_manual(values = c("#FB8072", "#BEBADA", "#80B1D3")) +
        theme_hc() +
        theme(legend.position = "none", title = element_blank())
    }, bg="transparent")

    output$totalBar <- renderPlot({
      df_tbrange <- uploadedData() %>% mutate(ranges = cut(totalBehavior, c(-1, 24, 37, Inf))) %>%
        group_by(ranges) %>% tally() %>% as.data.frame()
      
      ggplot(df_tbrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill = ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width=0.9),
                  vjust=-0.25) +
        
        labs(title = "",
             x = "Risk Levels",
             y = "Number of Students") +
        scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk")) +
        scale_fill_manual(values = c("#FB8072", "#BEBADA", "#80B1D3")) +
        theme_hc() +
        theme(legend.position = "none", theme = element_blank())
    })
  })
}
