classroomTabUI <- function(id, uploadedData) {
  ns <- NS(id)
  
  tabItem(
    tabName = "classroomTab",
    
    
    #' Three columns: Classroom summary (teacher, classroom stats),
    #' Student stats (saebrs scores, summary and graph),
    #' Student list (list students and click to view their studentView)
    
    column(
      12,
      # Classroom summary
      verticalLayout(
        box(
          fluidRow(
            valueBoxOutput(NS(id, "homeroomTeacherId"))
          ),
          fluidRow(
            valueBoxOutput(NS(id, "teacherSubjectBox")),
            valueBoxOutput(NS(id, "classIdBox"))
          ),
          width = 4
        ),
        
        # Classroom summary stats
        box(
          # Num of students, grade
          infoBoxOutput(NS(id, "numStudentsBox")),
          infoBoxOutput(NS(id, "gradeBox")),
          width = 4
        )
        
      )
    )
  )
}


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


classroomTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    # Select the current teacher and room
    # Pass in teacher ID in the function params when the classroom is selected
    classId <- reactiveVal(1) # Temporary value : Room ID
    
    # Subset the data 
    selectedClass <- reactiveValues(data = NULL)

    observe({
      selectedClass$data <- as.data.frame(subset(uploadedData(), homeroomId == classId()))

    })
      
    # ---- RENDER VALUE / INFO BOXES ----
    output$homeroomTeacherId <- renderValueBox(
      valueBox(
        selectedClass$data$homeroomTeacherId[1],
        "Homeroom Teacher ID"
      )
    )
  })
}