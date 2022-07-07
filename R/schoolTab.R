schoolTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "schoolTab",
          actionButton("Warning", "Click Here To Upload School Files in Upload Files"),
          
          # 4-4 split bars, 
          fluidRow(
            splitLayout(
              plotOutput(NS(id, "trstotalBar")),
              plotOutput(NS(id, "totalBar"))
            )
          ),
          fluidRow(
            splitLayout(
              splitLayout(
                plotOutput(NS(id, "trssocialBar")),
                plotOutput(NS(id, "trsacademicBar")),
                plotOutput(NS(id, "trsemotionalBar"))
              ),
              splitLayout(
                plotOutput(NS(id, "socialBar")),
                plotOutput(NS(id, "academicBar")),
                plotOutput(NS(id, "emotionalBar"))
              )
            )
          ),
          fluidRow(
            splitLayout(
              cellwidths = c("50%", "50%"),
                radioButtons(
                  NS(id, "level"), 
                  "Show ___ MySAEBRS Risk Levels",
                  c("All" = "alltotalms",
                    "Low" = "lowtotalms",
                    "Some" = "sometotalms",
                    "High" = "hightotalms")
                ),
              
                #To-Do: Grade Level Selection(6,7,8 grade)
                radioButtons(
                  NS(id, "grade"), 
                  "Show",
                  c("All Grades" = "allgrades",
                    "6th Grade" = "6",
                    "7th Grade" = "7",
                    "8th Grade" = "8")
                )
            )
          ),
          div(style='height:100%; width:100%; overflow: scroll; background-color: #dce0b4',
            tableOutput(NS(id, "contentsTable")))
  )
}


schoolTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    #School-wide TRS bargraph outputs
    output$trstotalBar <- renderPlot({
      df_tbrange <- uploadedData() %>% 
        mutate(ranges = cut(trsTotalBehavior, c(-1, 24, 37, Inf))) %>%
        group_by(ranges) %>% 
        tally() %>% 
        as.data.frame()
      
      studenttotalplot <- ggplot(df_tbrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill = ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        theme(legend.position = "none") +
        ggtitle("TRS Total Score Distribution") +
        xlab("Risk Levels") + ylab("Number of Students") +
        scale_x_discrete(labels = c("High Risk", "Some Risk", "Low Risk"))
      return(studenttotalplot)
    })
    
    output$trssocialBar <- renderPlot({
      df_sbrange <-
        uploadedData() %>% 
        mutate(ranges = cut(trsSocialBehavior, c(-1, 9, 12, Inf))) %>%
        group_by(ranges) %>% 
        tally() %>% 
        as.data.frame()
      
      studenttotalplot <-
        ggplot(df_sbrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill = ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        theme(legend.position = "none") +
        ggtitle("TRS Social Score Distribution") +
        xlab("Risk Levels") + ylab("Number of Students") +
        scale_x_discrete(labels = c("High Risk", "Some Risk", "Low Risk"))
      return(studenttotalplot)
    })
    output$trsacademicBar <- renderPlot({
      df_abrange <-
        uploadedData() %>% 
        mutate(ranges = cut(trsAcademicBehavior, c(-1, 6, 9, Inf))) %>%
        group_by(ranges) %>%
        tally() %>% 
        as.data.frame()
      
      studenttotalplot <-
        ggplot(df_abrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill = ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        theme(legend.position = "none") +
        ggtitle("TRS Academic Score Distribution") +
        xlab("Risk Levels") + ylab("Number of Students") +
        scale_x_discrete(labels = c("High Risk", "Some Risk", "Low Risk"))
      return(studenttotalplot)
    })
    
    output$trsemotionalBar <- renderPlot({
      df_ebrange <-
        uploadedData() %>% 
        mutate(ranges = cut(trsEmotionalBehavior, c(-1, 7, 10, Inf))) %>%
        group_by(ranges) %>% 
        tally() %>% 
        as.data.frame()
      
      studenttotalplot <-
        ggplot(df_ebrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill = ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        theme(legend.position = "none") +
        ggtitle("TRS Emotional Score Distribution") +
        xlab("Risk Levels") + ylab("Number of Students") +
        scale_x_discrete(labels = c("High Risk", "Some Risk", "Low Risk"))
      return(studenttotalplot)
    })
    
    #School-wide MySAEBRS bargraph outputs
    output$totalBar <- renderPlot({
      df_tbrange <-
        uploadedData() %>% 
        mutate(ranges = cut(totalBehavior, c(-1, 24, 37, Inf))) %>%
        group_by(ranges) %>% 
        tally() %>% 
        as.data.frame()
      
      studenttotalplot <-
        ggplot(df_tbrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill = ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        theme(legend.position = "none") +
        ggtitle("MySAEBRS Total Score Distribution") +
        xlab("Risk Levels") + ylab("Number of Students") +
        scale_x_discrete(labels = c("High Risk", "Some Risk", "Low Risk"))
      return(studenttotalplot)
    })
    
    output$socialBar <- renderPlot({
      df_sbrange <-
        uploadedData() %>%
        mutate(ranges = cut(socialBehavior, c(-1, 9, 12, Inf))) %>%
        group_by(ranges) %>% 
        tally() %>% 
        as.data.frame()
      
      studenttotalplot <-
        ggplot(df_sbrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill = ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        theme(legend.position = "none") +
        ggtitle("MySAEBRS Social Score Distribution") +
        xlab("Risk Levels") + ylab("Number of Students") +
        scale_x_discrete(labels = c("High Risk", "Some Risk", "Low Risk"))
      return(studenttotalplot)
    })
    
    output$academicBar <- renderPlot({
      df_abrange <-
        uploadedData() %>% 
        mutate(ranges = cut(academicBehavior, c(-1, 6, 9, Inf))) %>%
        group_by(ranges) %>% 
        tally() %>% 
        as.data.frame()
      
      df_abrange
      
      studenttotalplot <-
        ggplot(df_abrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill = ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        theme(legend.position = "none") +
        ggtitle("MySAEBRS Academic Score Distribution") +
        xlab("Risk Levels") + ylab("Number of Students") +
        scale_x_discrete(labels = c("High Risk", "Some Risk", "Low Risk"))
      return(studenttotalplot)
    })
    
    output$emotionalBar <- renderPlot({
      df_ebrange <-
        uploadedData() %>% 
        mutate(ranges = cut(emotionalBehavior, c(-1, 7, 10, Inf))) %>%
        group_by(ranges) %>% 
        tally() %>% 
        as.data.frame()
      
      df_ebrange
      
      studenttotalplot <-
        ggplot(df_ebrange, aes(x = ranges, y = n)) +
        geom_bar(stat = 'identity', aes(fill = ranges)) +
        geom_text(aes(label = n),
                  position = position_dodge(width = 0.9),
                  vjust = -0.25) +
        theme(legend.position = "none") +
        ggtitle("MySAEBRS Emotional Score Distribution") +
        xlab("Risk Levels") + ylab("Number of Students") +
        scale_x_discrete(labels = c("High Risk", "Some Risk", "Low Risk"))
      return(studenttotalplot)
    })

    lowRiskMin <- 37
    someRiskMin <- 24
        
    #School-wide MySAEBRS Scores as reactive functions.
    low_total_MS <- reactive({
      tmp <- uploadedData()
      tmp[tmp$totalBehavior > lowRiskMin, ]
    })
    some_total_MS <- reactive({
      tmp <- uploadedData()
      tmp[tmp$totalBehavior > someRiskMin &
            tmp$totalBehavior < lowRiskMin, ]
    })
    high_total_MS <- reactive({
      tmp <- uploadedData()
      tmp[tmp$totalBehavior < someRiskMin, ]
    })
    all_total_MS <- reactive({
      tmp <- uploadedData()
    })
    
    #School-wide TRS Scores as reactive functions.
    low_total_TRS <- reactive({
      tmp <- uploadedData()
      tmp[tmp$trsTotalBehavior > lowRiskMin, ]
    })
    some_total_TRS <- reactive({
      tmp <- uploadedData()
      tmp[tmp$trsTotalBehavior > someRiskMin &
            tmp$trsTotalBehavior < lowRiskMin, ]
    })
    high_total_TRS <- reactive({
      tmp <- uploadedData()
      tmp[tmp$trsTotalBehavior < someRiskMin, ]
    })
    all_total_TRS <- reactive({
      tmp <- uploadedData()
    })
    
    #Selectable dataframe display of CSV contents
    output$contentsTable <- renderTable({
      options = list(scrollX = TRUE)
      # if a specific grade is selected, subset the list with that grade as a conditional.
      if (input$grade != "allgrades") {
        switch(
          input$level,
          # Depending on radiobutton selected, the returned table reflects one risk group.
          "lowtotalms" = return(low_total_MS()[low_total_MS()$grade == input$grade, ]),
          "sometotalms" = return(some_total_MS()[some_total_MS()$grade == input$grade, ]),
          "hightotalms" = return(high_total_MS()[high_total_MS()$grade == input$grade, ]),
          "alltotalms" = return(all_total_MS()[all_total_MS()$grade == input$grade, ])
        )
      }
      # if all grades are selected, simply return MS without conditional
      else{
        switch(
          input$level,
          # Depending on radiobutton selected, the returned table reflects one risk group.
          "lowtotalms" = return(low_total_MS()),
          "sometotalms" = return(some_total_MS()),
          "hightotalms" = return(high_total_MS()),
          "alltotalms" = return(all_total_MS())
        )
      }
      
    })
  })
}