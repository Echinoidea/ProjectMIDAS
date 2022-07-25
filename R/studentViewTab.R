# Loading in CSV and creating autocomplete list -----
# studentData <- read.csv("data/dummy_midas_data2.csv")
# studentData$ID <- sample(100:999, 50, replace = FALSE)

studentViewTabUI <- function(id, uploadedData) {
  ns <- NS(id)
  
  tabItem(tabName = "studentTab",
          # ---- STUDENT INFORMATION ROW ----
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
          
          # ---- SAEBERS SCORE BOXES AND PLOTS ----
          fluidRow(
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
                  tabsetPanel(
                    id = NS(id, "trsTabset"),
                    
                    tabPanel(
                      id = NS(id, "trsBoxPlotTab"),
                      "Box Plot",
                      plotOutput(NS(id, "trsBoxPlot"))
                    ),
                    
                    tabPanel(
                      id = NS(id, "trsDensityPlotTab"),
                      "Density",
                      plotOutput(NS(id, "trsDensityPlot"))
                    )
                  ),
                  title = "SAEBERS-TRS/SRS Total Score Distribution",
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
                  tabsetPanel(
                    id = NS(id, "myTabset"),
                    
                    tabPanel(
                      id = NS(id, "myBoxPlotTab"),
                      "Box Plot",
                      plotOutput(NS(id, "myBoxPlot"))
                    ),
                    
                    tabPanel(
                      id = NS(id, "myDensityPlotTab"),
                      "Density",
                      plotOutput(NS(id, "myDensityPlot"))
                    )
                  ),
                  title = "MySAEBRS Total Score Distribution",
                  width = 12
                )
              )
            )
          )
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
        selectedStudent$data$trsEmotionalBehavior,
        "TRS Emotional",
        color = "green"
      )
    })
    
    
    # ---- RENDER MySAEBRS BOXES ----
    
    output$myTotalBox <- renderValueBox({
      valueBox(
        selectedStudent$data$totalBehavior,
        "Total",
        color = "olive"
      )
    })
    
    output$mySocialBox <- renderValueBox({
      valueBox(
        selectedStudent$data$socialBehavior,
        "Social",
        color = "green"
      )
    })
    
    output$myEmotionalBox <- renderValueBox({
      valueBox(
        selectedStudent$data$emotionalBehavior,
        "Emotional",
        color = "green"
      )
    })
    
    output$myAcademicBox <- renderValueBox({
      valueBox(
        selectedStudent$data$academicBehavior,
        "Academic",
        color = "green"
      )
    })
    
    
    
    output$myBoxPlot <- renderPlot({
      df <- uploadedData()
      
      myTotalBoxPlot <- ggplot(df, aes(x = totalBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        geom_point(aes(x = selectedStudent$data$totalBehavior, y = 0), color = "#FB8072") +
        geom_text(aes(label = ..x.., x = selectedStudent$data$totalBehavior, y = 0), vjust = -3.1, color = "#FB8072", size = 5) +
        
        # Stat summary to label min, Q1, Q2, Q3, and max
        stat_summary(
          geom = "text",
          fun = quantile, #function(x) boxplot.stats(x)$stats,
          aes(label = ..x.., y = 0),
          position = position_nudge(y = -0.25),
          size = 4,
          color = "#FB8072",
          orientation = "y"
        ) +
        
        # Visuals
        xlim(0, 60) +
        ylim(-1, 1) +
        labs(title = "",
             x = "mySAEBERS Total Behavior Score",
             y = "") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      
      myTotalBoxPlot
    })
    
    
    output$myDensityPlot <- renderPlot({
      df <- uploadedData()
      
      myTotalDensity <- ggplot(df, aes(x = totalBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        geom_vline(xintercept = selectedStudent$data$totalBehavior, color = "#FB8072") +
        geom_text(aes(x = selectedStudent$data$totalBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#FB8072", size = 5) +
        
        #scale_y_continuous(labels = scales::percent) +
        xlim(0, 60) +
        labs(title = "",
             x = "mySAEBERS Total Behavior Score",
             y = "Number of Students") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
        )
      
      myTotalDensity
    })
    
    
    output$trsBoxPlot <- renderPlot({
      df <- uploadedData()
      
      trsTotalBoxPlot <- ggplot(df, aes(x = trsTotalBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        geom_point(aes(x = selectedStudent$data$trsTotalBehavior, y = 0), color = "#FB8072") +
        geom_text(aes(label = ..x.., x = selectedStudent$data$trsTotalBehavior, y = 0), vjust = -3.1, color = "#FB8072", size = 5) +
        
        # Stat summary to label min, Q1, Q2, Q3, and max
        stat_summary(
          geom = "text",
          fun = quantile, #function(x) boxplot.stats(x)$stats,
          aes(label = ..x.., y = 0),
          position = position_nudge(y = -0.25),
          size = 4,
          color = "#FB8072",
          orientation = "y"
        ) +
        
        # Visuals
        xlim(0, 60) +
        ylim(-1, 1) +
        labs(title = "",
             x = "mySAEBERS Total Behavior Score",
             y = "") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      
      trsTotalBoxPlot
    })
    
    
    output$trsDensityPlot <- renderPlot({
      df <- uploadedData()
      
      trsTotalDensity <- ggplot(df, aes(x = trsTotalBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        geom_vline(xintercept = selectedStudent$data$trsTotalBehavior, color = "#FB8072") +
        geom_text(aes(x = selectedStudent$data$trsTotalBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#FB8072", size = 5) +
        
        # Visuals
        #scale_y_continuous(labels = scales::percent) +
        xlim(0, 60) +
        labs(title = "",
             x = "mySAEBERS Total Behavior Score",
             y = "Number of Students") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
        )
      
      trsTotalDensity
    })
    
    # Add option to see density and boxplot
  })
}
