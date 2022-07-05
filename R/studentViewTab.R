# Loading in CSV and creating autocomplete list -----
# studentData <- read.csv("data/dummy_midas_data2.csv")
# studentData$ID <- sample(100:999, 50, replace = FALSE)

studentViewTabUI <- function(id, uploadedData) {
  ns <- NS(id)
  
  tabItem(tabName = "studentTab",
          fluidRow(
            column(
              4,
              #style = "background-color: #d0df92; padding: 5px; border-radius: 25px;",
              style = "background-color: #CCEBC5; padding: 15px;",
              align = "center",
              
              # Render the selectizeInput from the server function to allow accessing uploadedData() IDs
              uiOutput(NS(id, "inputStudentID")),
              
              actionButton(NS(id, "btnStudentID"), label = "Search"),
              
              br(),
              br(),
              
              # Student demographic data
              fluidRow(
                column(
                  6,
                  align = "left",
                  
                  p(tags$b("Gender")),
                  br(),
                  
                  p(tags$b("Ethnicity")),
                  br(),
                  
                  p(tags$b("Grade")),
                  br(),
                  
                  p(tags$b("Special Education")),
                ),
                column(
                  6,
                  align = "left",
                  
                  textOutput(NS(id, "studentGender")),
                  br(),
                  
                  textOutput(NS(id, "studentEthnicity")),
                  br(),
                  
                  textOutput(NS(id, "studentGrade")),
                  br(),
                  
                  textOutput(NS(id, "studentSpecialEd"))
                )
              )
            ),
            #MIDAS Assessments
            column(
              8,
              #style = "background-color:#d0df92; padding: 15px; border-radius: 25px; height: 100%;",
              style = "background-color: #CCEBC5; padding: 15px;",
              
              verticalLayout(
                column(
                  12,
                  
                  column(2,
                         p(tags$b("Test Score"))),
                  column(5,
                         style = "background-color:white; padding: 20px; border-radius: 25px; height: 100%; border-style: solid;",
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
                      12,
                      align = "left",
                      
                      column(
                        4,
                        align = "left",
                        
                        p(tags$b("Total TRS")),
                        br(),
                        
                        p(tags$b("Social TRS")),
                        br(),
                        
                        p(tags$b("Academic TRS")),
                        br(),
                        
                        p(tags$b("Emotional TRS")),
                        
                      ),
                      column(
                        8,
                        align = "left",
                        
                        textOutput(NS(id, "totalTRS")),
                        br(),
                        
                        textOutput(NS(id, "socialTRS")),
                        br(),
                        
                        textOutput(NS(id, "academicTRS")),
                        br(),
                        
                        textOutput(NS(id, "emotionalTRS"))
                      )
                    ),
                    
                    plotOutput(NS(id, "trsTotalBar"))
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
                      textOutput(NS(id, "totalSAEBRS")),
                      
                      br(),
                      
                      p(tags$b("Social")),
                      textOutput(NS(id, "socialSAEBRS")),
                      
                      br(),
                      
                      p(tags$b("Social")),
                      textOutput(NS(id, "academicSAEBRS")),
                      
                      br(),
                      
                      p(tags$b("Social")),
                      textOutput(NS(id, "emotionalSAEBRS"))
                    ),
                    plotOutput(NS(id, "totalBar"))
                  )
                ),fluid = TRUE
              )
            )
          ))
}

studentViewTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
    output$inputStudentID <- renderUI({
      selectizeInput(
        NS(id, "txtinStudentID"),
        label = "Student ID Number",
        choices = c("", uploadedData()$ID),
        selected = '',
        multiple = FALSE,
        options = list(create = FALSE)
      )
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
      df_abrange <- uploadedData() %>% mutate(ranges = cut(academicBehavior, c(-1, 6, 9, Inf))) %>%
        group_by(ranges) %>% tally() %>% as.data.frame()
      
      df_abrange
      
      ggplot(df_abrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill=ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width=0.9),
                  vjust = -0.25) +
        labs(title = "TRS-TOTAL Average Score", 
             x = "Risk Levels", 
             y = "Number of Students") +
        scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk")) +
        scale_fill_manual(values = c("#FB8072", "#BEBADA", "#80B1D3")) +
        theme_hc() +
        theme(legend.position = "none")
    }, bg="transparent")

    output$totalBar <- renderPlot({
      df_tbrange <- uploadedData() %>% mutate(ranges = cut(totalBehavior, c(-1, 24, 37, Inf))) %>%
        group_by(ranges) %>% tally() %>% as.data.frame()
      
      ggplot(df_tbrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill = ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width=0.9),
                  vjust=-0.25) +
        
        labs(title = "MySAEBRS Total Score Distribution",
             x = "Risk Levels",
             y = "Number of Students") +
        scale_x_discrete(labels=c("High Risk", "Some Risk", "Low Risk")) +
        scale_fill_manual(values = c("#FB8072", "#BEBADA", "#80B1D3")) +
        theme_hc() +
        theme(legend.position = "none")
    })
  })
}

#shinyApp(ui = studentViewTabUI("id"), server = studentViewTabServer("id"))