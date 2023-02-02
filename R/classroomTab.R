classroomTabUI <- function(id, uploadedData) {
  ns <- NS(id)
  
  tabItem(
    tabName = "classroomTab",
    
    
    #' Three columns: Classroom summary (teacher, classroom stats),
    #' Student stats (saebrs scores, summary and graph),
    #' Student list (list students and click to view their studentView)
    
    column(
      3,
      # Classroom summary
      verticalLayout(
        box(
          fluidRow(
            valueBoxOutput(NS(id, "homeroomTeacherIdBox"), width = 6),
            valueBoxOutput(NS(id, "classIdBox"), width = 6)
          ),
          fluidRow(
            valueBoxOutput(NS(id, "averageRisk"), width = 12)
          ),
          width = 12,
          id = NS(id, "teacherBox")
        ),
        
        # Classroom summary stats
        box(
          # Num of students, grade
          verticalLayout(
            fluidRow(
              valueBoxOutput(NS(id, "numStudentsBox"), width = 12)
            ),
            fluidRow(
              valueBoxOutput(NS(id, "gradeBox"), width = 12)
            )
          ),
          
          
          width = 12,
          id = NS(id, "classStatsBox")
        ),
        
        box(
          id = NS(id, "studentNavBox"),
          uiOutput(NS(id, "studentList")),
          actionButton(NS(id, "studentNavButton"), label = "Go to Student Dashboard"),
          width = 12
        )
        
      )
    ),
    
    column(
      9,
      verticalLayout(
        box(
          fluidRow(
            
            
            column(
              6,
              fluidRow(
                valueBoxOutput(NS(id, "avgTrs1"), width = 12)
              ),
              fluidRow(
                column(
                  4,
                  valueBoxOutput(NS(id, "avgTrs1Social"), width = 12)
                ),
                column(
                  4,
                  valueBoxOutput(NS(id, "avgTrs1Academic"), width = 12)
                ),
                column(
                  4,
                  valueBoxOutput(NS(id, "avgTrs1Emotional"), width = 12)
                )
              )
            ),
            
            column(
              6,
              fluidRow(
                valueBoxOutput(NS(id, "avgTrs2"), width = 12)
              ),
              fluidRow(
                column(
                  4,
                  valueBoxOutput(NS(id, "avgTrs2Social"), width = 12)
                ),
                column(
                  4,
                  valueBoxOutput(NS(id, "avgTrs2Academic"), width = 12)
                ),
                column(
                  4,
                  valueBoxOutput(NS(id, "avgTrs2Emotional"), width = 12)
                )
              )
            )
          
          ),
          
          width = 12,
          id = NS(id, "scoreSummaryBox")  
        ),
        
        box(
          title = "Teacher-assigned Behavior Scores",
          plotOutput(NS(id, "studentScorePlot")),
          width = 12,
          id = NS(id, "plotBox")
          
        )
      )
    ),
    
    tags$head(tags$style(HTML('
                              #classroomTab-teacherBox .box-header {
                              display: none;
                              }
                              
                              #classroomTab-classStatsBox .box-header {
                              display: none;
                              }
                              
                              #classroomTab-studentNavBox .box-header {
                              display: none;
                              }
                              
                              #classroomTab-plotBox .box-header {
                              display: none;
                              }
                              
                              .small-box {
                              background-color: #106849 !important
                              };
                              
                              #classroomTab-averageRisk .small-box {
                              width: 225px;
                              height: 100px;
                              }
                              
                              #classroomTab-averageRisk .inner {
                              align-text: left;
                              padding: 50px;
                              }
                              
                              #classroomTab-averageRisk h3 {
                              font-size: 40px;
                              text-align: center;
                              }
                              
                              #classroomTab-averageRisk p {
                              font-size: 20px;
                              text-align: center;
                              }
                              ')))
  )
}

# .small-box {
#   background-color: #106849 !important
# };


#' TODO:
#' Questions:
#' Need more information on how the homeroom score and subject room score works.
#'   - does each student have a score for every subject they're in? Or is just one selected?
#'     - How will the code be able to display what subject (or homeroom) by inputting only one ID?
#'   - This feels like data that would be optimal in a relational database, not a csv. But we don't
#'     know what form the data is coming in as.


#' CLASSROOM, HOMEROOM, TEACHER DATA SET PROPERTIES
#' HomeroomId and SubjectRoomId are unique. HomeroomId = 1:1000, subjectRoomId = 2000:3000
#' Teacher IDs are NOT UNIQUE. Many students may have the same teacher. Many classrooms may have the same teacher
#' There are 100 teachers in this set. So ID ranges from 1:100


classroomTabServer <- function(id, uploadedData, parent_session) {
  moduleServer(id, function(input, output, session, parent = parent_session) {
    # Select the current teacher and room
    # Pass in teacher ID in the function params when the classroom is selected
    classId <- reactiveVal(1) # Temporary value : Room ID
    
    # Subset the data 
    selectedClass <- reactiveValues(data = NULL)

    observe({
      selectedClass$data <- as.data.frame(subset(uploadedData(), homeroomId == classId()))
      
    })
    
    observeEvent(input$studentNavButton, {
      studentToView <- input$studentInput
      print(studentToView)
      updateTabItems(session = parent, "tabs", "studentTab") # WOrks now
      # TODO: After navigating to page, load the appropriate student ID to view
      #studentViewTabServer("studentViewTab", uploadedData())
      print("Navigating...")
    })
      
    # ---- RENDER VALUE / INFO BOXES ----
    
    output$homeroomTeacherIdBox <- renderValueBox(
      valueBox(
        selectedClass$data$homeroomTeacherId[1],
        "Teacher ID"
      )
    )
    
    output$classIdBox <- renderValueBox(
      valueBox(
        selectedClass$data$homeroomId[1],
        "Class ID"
      )
    )
    
    output$averageRisk <- renderValueBox(
      valueBox(
        mean(selectedClass$data$homeroomScore),
        "Average Risk Score"
      )
    )
    
    output$numStudentsBox <- renderValueBox(
      valueBox(
        length(selectedClass$data$ID),
        "Student Count"
      )
    )
    
    output$gradeBox <- renderValueBox(
      valueBox(
        selectedClass$data$grade[1],
        "Grade",
      )
    )
    
    output$avgMidasRisk <- renderValueBox(
      valueBox(
        mean(selectedClass$data$midasRisk),
        "Avg. MIDAS Risk"
      )
    )
    
    output$avgTrs1 <- renderValueBox(
      valueBox(
        mean(selectedClass$data$trsTotalBehavior),
        "Avg. TRS-1 Total"
      )
    )
    
    output$avgTrs1Social <- renderValueBox(
      valueBox(
        mean(selectedClass$data$trsSocialBehavior),
        "Avg. TRS-1 Social"
      )
    )
    
    output$avgTrs1Academic <- renderValueBox(
      valueBox(
        mean(selectedClass$data$trsAcademicBehavior),
        "Avg. TRS-1 Academic"
      )
    )
    
    output$avgTrs1Emotional <- renderValueBox(
      valueBox(
        mean(selectedClass$data$trsEmotionalBehavior),
        "Avg. TRS-1 Emotional"
      )
    )
    
    output$avgTrs2 <- renderValueBox(
      valueBox(
        mean(selectedClass$data$totalBehavior),
        "Avg. TRS-2 Total"
      )
    )
    
    output$avgTrs2Social <- renderValueBox(
      valueBox(
        mean(selectedClass$data$socialBehavior),
        "Avg. TRS-2 Social"
      )
    )
    
    output$avgTrs2Academic <- renderValueBox(
      valueBox(
        mean(selectedClass$data$academicBehavior),
        "Avg. TRS-2 Academic"
      )
    )
    
    output$avgTrs2Emotional <- renderValueBox(
      valueBox(
        mean(selectedClass$data$emotionalBehavior),
        "Avg. TRS-2 Emotional"
      )
    )
    
    output$studentScorePlot <- renderPlot({
      ggplot(selectedClass$data, aes(x = midasRisk)) +
        geom_density(adjust = 0.5) +
        # xlim(0, 60) +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "",
             x = "MIDAS Risk Score",
             y = "Number of Students") +
        theme_bw() +
        theme(
          legend.position = "none"
        )
    })
    
    output$studentList <- renderUI({
      selectizeInput(
        inputId = NS(id, "studentInput"),
        label = NULL,
        choices = selectedClass$data$ID,
        selected = NULL,
        multiple = FALSE,
        options = list(
          create = FALSE
        )
      )
    })
  })
}