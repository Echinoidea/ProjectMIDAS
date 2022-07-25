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
          
          # This row contains 2 columns which both contain a vertical layout 
          # consisting of the SAEBERS scores and boxes containing plots below
          fluidRow(
            # SAEBERS-TRS/SRS scores and plots
            column(
              6,
              verticalLayout(
                box(
                  title = "SAEBRS-TRS/SRS",
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
                
                # SAEBRS-TRS/SRS plot box
                box(
                  id = NS(id, "trsSaebersPlotBox"),
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
                  title = textOutput(outputId = NS(id, "trsPlotBoxTitle")),
                  width = 12,
                  
                  # Drop down menu to select which SAEBERS score to plot
                  dropdownMenu = boxDropdown(
                    boxDropdownItem(id = NS(id, "trsDropdownTotal"),
                                    "Total"),
                    boxDropdownItem(id = NS(id, "trsDropdownSocial"),
                                    "Social"),
                    boxDropdownItem(id = NS(id, "trsDropdownAcademic"),
                                    "Academic"),
                    boxDropdownItem(id = NS(id, "trsDropdownEmotional"),
                                    "Emotional")
                  )
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
                  id = NS(id, "mySaebersPlotBox"),
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
                  
                  # textOutput for the title so it can be dynamically changed
                  # when the user changes which SAEBERS score to display.
                  title = textOutput(outputId = NS(id, "myPlotBoxTitle")),
                  width = 12,
                  
                  # Drop down menu to select which SAEBERS score to plot
                  dropdownMenu = boxDropdown(
                    boxDropdownItem(id = NS(id, "myDropdownTotal"),
                                    "Total"),
                    boxDropdownItem(id = NS(id, "myDropdownSocial"),
                                    "Social"),
                    boxDropdownItem(id = NS(id, "myDropdownAcademic"),
                                    "Academic"),
                    boxDropdownItem(id = NS(id, "myDropdownEmotional"),
                                    "Emotional")
                  )
                )
              )
            )
          )
          )
}

# <button type="button" data-toggle="dropdown" class="btn btn-box-tool btn-sm dropdown-toggle" aria-haspopup="true" aria-expanded="false">
# <i class="fa fa-wrench" role="presentation" aria-label="wrench icon"></i>
#   </button>

studentViewTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
    # ---- INITIALIZATION ----
    
    # Render dynamic UI elements on load
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
    
    selectedMyDropdown <- reactiveVal("default")  # Which MySAEBERS score to plot
    selectedTrsDropdown <- reactiveVal("default") # which SAEBERS-TRS/SRS score to plot
    
    output$myPlotBoxTitle <- renderText({"MySAEBERS TOTAL Behavior Distribution"})
    output$trsPlotBoxTitle <- renderText({"SAEBERS-TRS/SRS TOTAL Behavior Distribution"})
    
    
    # ---- EVENT OBSERVERS ----
    
    observeEvent(input$btnStudentID, {
      print(uploadedData()$ID)
      searchID(input$txtinStudentID)
      print(searchID())
      selectedStudent$data <- subset(uploadedData(), ID == searchID())
      print(selectedStudent$data$firstName)
    })
    
    
    # Observe drop down for MySAEBERS plot box
    
    observeEvent(input$myDropdownTotal, {
      showNotification("Viewing MySAEBERS total score", duration = 2, type = "message")
      output$myPlotBoxTitle <- renderText({"MySAEBERS TOTAL Behavior Score"})
      selectedMyDropdown("total")
    })
    
    observeEvent(input$myDropdownSocial, {
      showNotification("Viewing MySAEBERS social score", duration = 2, type = "message")
      output$myPlotBoxTitle <- renderText({"MySAEBERS SOCIAL Behavior Score"})
      selectedMyDropdown("social")
    })
    
    observeEvent(input$myDropdownAcademic, {
      showNotification("Viewing MySAEBERS academic score", duration = 2, type = "message")
      output$myPlotBoxTitle <- renderText({"MySAEBERS ACADEMIC Behavior Score"})
      selectedMyDropdown("academic")
    })
    
    observeEvent(input$myDropdownEmotional, {
      showNotification("Viewing MySAEBERS emotional score", duration = 2, type = "message")
      output$myPlotBoxTitle <- renderText({"MySAEBERS EMOTIONAL Behavior Score"})
      selectedMyDropdown("emotional")
    })
    
    
    # Observe dropdown for TRS-SAEBERS plot box
    
    observeEvent(input$trsDropdownTotal, {
      showNotification("Viewing SAEBERS-TRS/SRS total score", duration = 2, type = "message")
      output$trsPlotBoxTitle <- renderText({"SAEBERS-TRS/SRS TOTAL Behavior Score"})
      selectedTrsDropdown("total")
    })
    
    observeEvent(input$trsDropdownSocial, {
      showNotification("Viewing SAEBERS-TRS/SRS social score", duration = 2, type = "message")
      output$trsPlotBoxTitle <- renderText({"SAEBERS-TRS/SRS SOCIAL Behavior Score"})
      selectedTrsDropdown("social")
    })
    
    observeEvent(input$trsDropdownAcademic, {
      showNotification("Viewing SAEBERS-TRS/SRS academic score", duration = 2, type = "message")
      output$trsPlotBoxTitle <- renderText({"SAEBERS-TRS/SRS ACADEMIC Behavior Score"})
      selectedTrsDropdown("academic")
    })
    
    observeEvent(input$trsDropdownEmotional, {
      showNotification("Viewing SAEBERS-TRS/SRS emotional score", duration = 2, type = "message")
      output$trsPlotBoxTitle <- renderText({"SAEBERS-TRS/SRS EMOTIONAL Behavior Score"})
      selectedTrsDropdown("emotional")
    })

    
    # ---- STUDENT INFO ROW ----
    
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
    
    # ---- RENDER TRS/SRS SCORE BOXES ----
    
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
    
    # ---- RENDER PLOTS ----
    
    
    # MySAEBERS BOX plots selecion
    
    selectedMyBoxPlot <- reactive({
      if (selectedMyDropdown() == "total") {
        return(myTotalBoxPlot())
      }
      else if (selectedMyDropdown() == "social") {
        return(mySocialBoxPlot())
      }
      else if (selectedMyDropdown() == "academic") {
        return(myAcademicBoxPlot())
      }
      else if (selectedMyDropdown() == "emotional") {
        return(myEmotionalBoxPlot())
      }
      else {
        if (is.null(searchID())) {
          return(myTotalBoxPlotDefault())
        }
        else {
          return(myTotalBoxPlot())
        }
      }
    })
    
    output$myBoxPlot <- renderPlot({
      selectedMyBoxPlot()
    })
    
    
    # SAEBERS-TRS/SRS BOX plots selection
    
    selectedTrsBoxPlot <- reactive({
      if (selectedTrsDropdown() == "total") {
        return(trsTotalBoxPlot())
      }
      else if (selectedTrsDropdown() == "social") {
        return(trsSocialBoxPlot())
      }
      else if (selectedTrsDropdown() == "academic") {
        return(trsAcademicBoxPlot())
      }
      else if (selectedTrsDropdown() == "emotional") {
        return(trsEmotionalBoxPlot())
      }
      else {
        if (is.null(searchID())) {
          return(trsTotalBoxPlotDefault())
        }
        else {
          return(trsTotalBoxPlot())
        }
      }
    })
    
    output$trsBoxPlot <- renderPlot({
      selectedTrsBoxPlot()
    })
    
    
    # MySAEBERS DENSITY plots selecion
    
    selectedMyDensityPlot <- reactive({
      if (selectedMyDropdown() == "total") {
        return(myTotalDensityPlot())
      }
      else if (selectedMyDropdown() == "social") {
        return(mySocialDensityPlot())
      }
      else if (selectedMyDropdown() == "academic") {
        return(myAcademicDensityPlot())
      }
      else if (selectedMyDropdown() == "emotional") {
        return(myEmotionalDensityPlot())
      }
      else {
        if (is.null(searchID())) {
          return(myTotalDensityPlotDefault())
        }
        else {
          return(myTotalDensityPlot())
        }
      }
    })
    
    output$myDensityPlot <- renderPlot({
      selectedMyDensityPlot()
    })
    
    
    # SAEBERS-TRS/SRS DENSITY plots selection
    
    selectedTrsDensityPlot <- reactive({
      if (selectedTrsDropdown() == "total") {
        return(trsTotalDensityPlot())
      }
      else if (selectedTrsDropdown() == "social") {
        return(trsSocialDensityPlot())
      }
      else if (selectedTrsDropdown() == "academic") {
        return(trsAcademicDensityPlot())
      }
      else if (selectedTrsDropdown() == "emotional") {
        return(trsEmotionalDensityPlot())
      }
      else {
        if (is.null(searchID())) {
          return(trsTotalDensityPlotDefault())
        }
        else {
          return(trsTotalDensityPlot())
        }
      }
    })
    
    output$trsDensityPlot <- renderPlot({
      selectedTrsDensityPlot()
    })
    
    
    # ---- DEFAULT BOX AND DENSITY PLOTS ----
    myTotalBoxPlotDefault <- reactive({ggplot(uploadedData(), aes(x = totalBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        # geom_point(aes(x = selectedStudent$data$totalBehavior, y = 0), color = "#FB8072") +
        # geom_text(aes(label = ..x.., x = selectedStudent$data$totalBehavior, y = 0), vjust = -3.1, color = "#FB8072", size = 5) +
        
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
    })
    
    myTotalDensityPlotDefault <- reactive({ggplot(uploadedData(), aes(x = totalBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        # geom_vline(xintercept = selectedStudent$data$totalBehavior, color = "#FB8072") +
        # geom_text(aes(x = selectedStudent$data$totalBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#FB8072", size = 5) +
        
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
    })
    
    trsTotalBoxPlotDefault <- reactive({ggplot(uploadedData(), aes(x = trsTotalBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        # geom_point(aes(x = selectedStudent$data$totalBehavior, y = 0), color = "#FB8072") +
        # geom_text(aes(label = ..x.., x = selectedStudent$data$totalBehavior, y = 0), vjust = -3.1, color = "#FB8072", size = 5) +
        
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
    })
    
    trsTotalDensityPlotDefault <- reactive({ggplot(uploadedData(), aes(x = trsTotalBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        # geom_vline(xintercept = selectedStudent$data$totalBehavior, color = "#FB8072") +
        # geom_text(aes(x = selectedStudent$data$totalBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#FB8072", size = 5) +
        
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
    })
    
    
    # ---- MySAEBERS BOX PLOTS ----
    
    # TOTAL BOXPLOT
    myTotalBoxPlot <- reactive({
      ggplot(uploadedData(), aes(x = totalBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        
        # In-line conditional to check if there is any selected searchID, if not,
        # don't add the geom_point or geom_text because it would throw an error.
        # This is so there is never a weird empty box with a message where a plot
        # should be.
        {if (is.null(searchID()) == FALSE) 
          list(
            geom_point(aes(x = selectedStudent$data$totalBehavior, y = 0), color = "#FB8072"),
            geom_text(aes(label = ..x.., x = selectedStudent$data$totalBehavior, y = 0), vjust = -3.1, color = "#FB8072", size = 5)
        )} +
        
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
             x = "",
             y = "") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    })
    
    # SOCIAL BOXPLOT
    mySocialBoxPlot <- reactive({
      ggplot(uploadedData(), aes(x = socialBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_point(aes(x = selectedStudent$data$socialBehavior, y = 0), color = "#80B1D3"),
            geom_text(aes(label = ..x.., x = selectedStudent$data$socialBehavior, y = 0), vjust = -3.1, color = "#80B1D3", size = 5)
        )} +
        
        # Stat summary to label min, Q1, Q2, Q3, and max
        stat_summary(
          geom = "text",
          fun = quantile, #function(x) boxplot.stats(x)$stats,
          aes(label = ..x.., y = 0),
          position = position_nudge(y = -0.25),
          size = 4,
          color = "#80B1D3",
          orientation = "y"
        ) +
        
        # Visuals
        xlim(0, 20) +
        ylim(-1, 1) +
        labs(title = "",
             x = "",
             y = "") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    })
    
    # ACADEMIC BOXPLOT
    myAcademicBoxPlot <- reactive({
      ggplot(uploadedData(), aes(x = academicBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_point(aes(x = selectedStudent$data$academicBehavior, y = 0), color = "#41AB5D"),
            geom_text(aes(label = ..x.., x = selectedStudent$data$academicBehavior, y = 0), vjust = -3.1, color = "#41AB5D", size = 5)
        )} +
        
        # Stat summary to label min, Q1, Q2, Q3, and max
        stat_summary(
          geom = "text",
          fun = quantile, #function(x) boxplot.stats(x)$stats,
          aes(label = ..x.., y = 0),
          position = position_nudge(y = -0.25),
          size = 4,
          color = "#41AB5D",
          orientation = "y"
        ) +
        
        # Visuals
        xlim(0, 20) +
        ylim(-1, 1) +
        labs(title = "",
             x = "",
             y = "") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    })
    
    # EMOTIONAL BOXPLOT
    myEmotionalBoxPlot <- reactive({ggplot(uploadedData(), aes(x = emotionalBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_point(aes(x = selectedStudent$data$emotionalBehavior, y = 0), color = "#BC80BD"),
            geom_text(aes(label = ..x.., x = selectedStudent$data$emotionalBehavior, y = 0), vjust = -3.1, color = "#BC80BD", size = 5)
        )} +
        
        # Stat summary to label min, Q1, Q2, Q3, and max
        stat_summary(
          geom = "text",
          fun = quantile, #function(x) boxplot.stats(x)$stats,
          aes(label = ..x.., y = 0),
          position = position_nudge(y = -0.25),
          size = 4,
          color = "#BC80BD",
          orientation = "y"
        ) +
        
        # Visuals
        xlim(0, 21) +
        ylim(-1, 1) +
        labs(title = "",
             x = "",
             y = "") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    })
    
    
    
    # ---- MySAEBERS DENSITY PLOTS ----
    
    # TOTAL DENSITY
    myTotalDensityPlot <- reactive({ggplot(uploadedData(), aes(x = totalBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_vline(xintercept = selectedStudent$data$totalBehavior, color = "#FB8072"),
            geom_text(aes(x = selectedStudent$data$totalBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#FB8072", size = 5) 
        )} +
        
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
    })
    
    # SOCIAL DENSITY
    mySocialDensityPlot <- reactive({ggplot(uploadedData(), aes(x = socialBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_vline(xintercept = selectedStudent$data$socialBehavior, color = "#80B1D3"),
            geom_text(aes(x = selectedStudent$data$socialBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#80B1D3", size = 5) 
        )} +
        
        #scale_y_continuous(labels = scales::percent) +
        xlim(0, 20) +
        labs(title = "",
             x = "mySAEBERS Social Behavior Score",
             y = "Number of Students") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
        )
    })
    
    # ACADEMIC DENSITY
    myAcademicDensityPlot <- reactive({ggplot(uploadedData(), aes(x = academicBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_vline(xintercept = selectedStudent$data$academicBehavior, color = "#41AB5D"),
            geom_text(aes(x = selectedStudent$data$academicBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#41AB5D", size = 5) 
        )} +
        
        #scale_y_continuous(labels = scales::percent) +
        xlim(0, 20) +
        labs(title = "",
             x = "mySAEBERS Academic Behavior Score",
             y = "Number of Students") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
        )
    })
    
    # EMOTIONAL DENSITY
    myEmotionalDensityPlot <- reactive({ggplot(uploadedData(), aes(x = emotionalBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_vline(xintercept = selectedStudent$data$emotionalBehavior, color = "#BC80BD"),
            geom_text(aes(x = selectedStudent$data$emotionalBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#BC80BD", size = 5)
        )} +
        
        #scale_y_continuous(labels = scales::percent) +
        xlim(0, 21) +
        labs(title = "",
             x = "mySAEBERS Emotional Behavior Score",
             y = "Number of Students") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
        )
    })
    
    
    # ---- SAEBERS-TRS/SRS BOXPLOTS ---- 
    
    # TOTAL BOXPLOT
    trsTotalBoxPlot <- reactive({ggplot(uploadedData(), aes(x = trsTotalBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_point(aes(x = selectedStudent$data$trsTotalBehavior, y = 0), color = "#FB8072"),
            geom_text(aes(label = ..x.., x = selectedStudent$data$trsTotalBehavior, y = 0), vjust = -3.1, color = "#FB8072", size = 5)
        )} +
        
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
             x = "SAEBERS-TRS/SRS Total Behavior Score",
             y = "") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    })
    
    # SOCIAL BOXPLOT
    trsSocialBoxPlot <- reactive({ggplot(uploadedData(), aes(x = trsSocialBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_point(aes(x = selectedStudent$data$trsSocialBehavior, y = 0), color = "#80B1D3"),
            geom_text(aes(label = ..x.., x = selectedStudent$data$trsSocialBehavior, y = 0), vjust = -3.1, color = "#80B1D3", size = 5)
        )} +
        
        # Stat summary to label min, Q1, Q2, Q3, and max
        stat_summary(
          geom = "text",
          fun = quantile, #function(x) boxplot.stats(x)$stats,
          aes(label = ..x.., y = 0),
          position = position_nudge(y = -0.25),
          size = 4,
          color = "#80B1D3",
          orientation = "y"
        ) +
        
        # Visuals
        xlim(0, 20) +
        ylim(-1, 1) +
        labs(title = "",
             x = "SAEBERS-TRS/SRS Social Behavior Score",
             y = "") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    })
    
    # ACADEMIC BOXPLOT
    trsAcademicBoxPlot <- reactive({ggplot(uploadedData(), aes(x = trsAcademicBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_point(aes(x = selectedStudent$data$trsAcademicBehavior, y = 0), color = "#41AB5D"),
            geom_text(aes(label = ..x.., x = selectedStudent$data$trsAcademicBehavior, y = 0), vjust = -3.1, color = "#41AB5D", size = 5)
        )} +
        
        # Stat summary to label min, Q1, Q2, Q3, and max
        stat_summary(
          geom = "text",
          fun = quantile, #function(x) boxplot.stats(x)$stats,
          aes(label = ..x.., y = 0),
          position = position_nudge(y = -0.25),
          size = 4,
          color = "#41AB5D",
          orientation = "y"
        ) +
        
        # Visuals
        xlim(0, 20) +
        ylim(-1, 1) +
        labs(title = "",
             x = "SAEBERS-TRS/SRS Academic Behavior Score",
             y = "") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    })
    
    # EMOTIONAL BOXPLOT
    trsEmotionalBoxPlot <- reactive({ggplot(uploadedData(), aes(x = trsEmotionalBehavior, y = 0)) +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(width = 0.3) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_point(aes(x = selectedStudent$data$trsEmotionalBehavior, y = 0), color = "#BC80BD"),
            geom_text(aes(label = ..x.., x = selectedStudent$data$trsEmotionalBehavior, y = 0), vjust = -3.1, color = "#BC80BD", size = 5)
        )} +
        
        # Stat summary to label min, Q1, Q2, Q3, and max
        stat_summary(
          geom = "text",
          fun = quantile, #function(x) boxplot.stats(x)$stats,
          aes(label = ..x.., y = 0),
          position = position_nudge(y = -0.25),
          size = 4,
          color = "#BC80BD",
          orientation = "y"
        ) +
        
        # Visuals
        xlim(0, 21) +
        ylim(-1, 1) +
        labs(title = "",
             x = "SAEBERS-TRS/SRS Emotional Behavior Score",
             y = "") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    })
    
    
    
    # ---- SAEBERS-TRS/SRS DENSITY PLOTS ----
    
    # TOTAL DENSITY
    trsTotalDensityPlot <- reactive({ggplot(uploadedData(), aes(x = trsTotalBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_vline(xintercept = selectedStudent$data$trsTotalBehavior, color = "#FB8072"),
            geom_text(aes(x = selectedStudent$data$trsTotalBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#FB8072", size = 5)
        )} +
        
        #scale_y_continuous(labels = scales::percent) +
        xlim(0, 60) +
        labs(title = "",
             x = "SAEBERS-TRS Total Behavior Score",
             y = "Number of Students") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
        )
    })
    
    # SOCIAL DENSITY
    trsSocialDensityPlot <- reactive({ggplot(uploadedData(), aes(x = trsSocialBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_vline(xintercept = selectedStudent$data$trsSocialBehavior, color = "#80B1D3"),
            geom_text(aes(x = selectedStudent$data$trsSocialBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#80B1D3", size = 5)
        )} +
        
        #scale_y_continuous(labels = scales::percent) +
        xlim(0, 20) +
        labs(title = "",
             x = "mySAEBERS Social Behavior Score",
             y = "Number of Students") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
        )
    })
    
    # ACADEMIC DENSITY
    trsAcademicDensityPlot <- reactive({ggplot(uploadedData(), aes(x = trsAcademicBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_vline(xintercept = selectedStudent$data$trsAcademicBehavior, color = "#41AB5D"),
            geom_text(aes(x = selectedStudent$data$trsAcademicBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#41AB5D", size = 5)
        )} +
        
        #scale_y_continuous(labels = scales::percent) +
        xlim(0, 20) +
        labs(title = "",
             x = "SAEBERS-TRS/SRS Academic Behavior Score",
             y = "Number of Students") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
        )
    })
    
    # EMOTIONAL DENSITY
    trsEmotionalDensityPlot <- reactive({ggplot(uploadedData(), aes(x = trsEmotionalBehavior)) +
        # Geom layer - Density, vertical line, label
        geom_density(adjust = 0.5) +
        
        {if (is.null(searchID()) == FALSE)
          list(
            geom_vline(xintercept = selectedStudent$data$trsEmotionalBehavior, color = "#BC80BD"),
            geom_text(aes(x = selectedStudent$data$trsEmotionalBehavior, y = 0.02, label = ..x..), hjust = -1.75, color = "#BC80BD", size = 5)
        )} +
        
        #scale_y_continuous(labels = scales::percent) +
        xlim(0, 21) +
        labs(title = "",
             x = "SAEBERS-TRS/SRS Emotional Behavior Score",
             y = "Number of Students") +
        theme_bw() +
        theme(
          legend.position = "none",
          title = element_blank(),
        )
    })

  })
}
