# ---- UI ----
schoolTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "dashboardTab",
    
    fluidRow(
      # Average MIDAS Risk for whole school and SAEBRS scores for whole school
      # ---- School-wide Averages ----
      column(
        4,
        
        box(
          # TODO: Change to high risk, med risk, low-risk proportions
          title = "Score Summary",
          # -- Average Risk --
          fluidRow(
            column(
              12,
              valueBoxOutput(
                NS(id, "avgRisk"),
                width = 12
              ),
              align = "center"
            )
          ),
        
          
          # -- SAEBRS TRS --
          fluidRow(
            valueBoxOutput(
              NS(id, "avgTrs"),
              width = 12
            )
          ),
          fluidRow(
            valueBoxOutput(
              NS(id, "avgTrsSocial"),
              width = 4
            ),
            valueBoxOutput(
              NS(id, "avgTrsAcademic"),
              width = 4
            ),
            valueBoxOutput(
              NS(id, "avgTrsEmotional"),
              width = 4
            )
          ),
          
          hr(),
          
          # -- MySAEBRS --
          fluidRow(
            valueBoxOutput(
              NS(id, "avgMySaebrs"),
              width = 12
            )
          ),
          fluidRow(
            valueBoxOutput(
              NS(id, "avgMySocial"),
              width = 4
            ),
            
            
            valueBoxOutput(
              NS(id, "avgMyAcademic"),
              width = 4
            ),
            valueBoxOutput(
              NS(id, "avgMyEmotional"),
              width = 4
            )
          ),
          width = 12
        )
      ),
      
      # Grade data - Average risk histogram, grade summary below (three columns)
      # ---- Grade-wide Data ----
      column(
        8,
        # -- Average Risk Grade Histogram --
        fluidRow(
          box(
            title = "Average MIDAS Risk by Grade",
            plotOutput(NS(id, "gradeRiskHist")),
            width = 12
          )
        ),
        
        # -- Statistics summary by grade - 3 columns
        fluidRow(
          box(
            id = NS(id, "gradeSummaryBox"),
            column(
              4,
              id = NS(id, "sixthColumn"),
              verticalLayout(
                valueBoxOutput(NS(id, "sixthStudentCount"), width = 12),
                valueBoxOutput(NS(id, "sixthHighRiskPerc"), width = 12),
                # valueBoxOutput(NS(id, "sixthMedRiskPerc"), width = 12),
                # valueBoxOutput(NS(id, "sixthLowRiskPerc"), width = 12),
                div(
                  id = NS(id, "sixthGradeButtonDiv"),
                  appButton(inputId = NS(id, "sixthGradeButton"), label = "View 6th Grade Classrooms")
                )
              )
            ),
            column(
              4,
              verticalLayout(
                valueBoxOutput(NS(id, "seventhStudentCount"), width = 12),
                valueBoxOutput(NS(id, "seventhHighRiskPerc"), width = 12),
                # valueBoxOutput(NS(id, "seventhMedRiskPerc"), width = 12),s
                # valueBoxOutput(NS(id, "seventhLowRiskPerc"), width = 12),
                div(
                  id = NS(id, "seventhGradeButtonDiv"),
                  appButton(inputId = NS(id, "seventhGradeButton"), label = "View 7th Grade Classrooms")
                )
              )
            ),
            column(
              4,
              verticalLayout(
                valueBoxOutput(NS(id, "eighthStudentCount"), width = 12),
                valueBoxOutput(NS(id, "eighthHighRiskPerc"), width = 12),
                # valueBoxOutput(NS(id, "eighthMedRiskPerc"), width = 12),
                # valueBoxOutput(NS(id, "eighthLowRiskPerc"), width = 12),
                div(
                  id = NS(id, "eighthGradeButtonDiv"),
                  appButton(inputId = NS(id, "eighthGradeButton"), label = "View 8th Grade Classrooms")
                )
              )
            ),
            width = 12
          )
        )
      )
      
    ),
    
    # TO ACCESS ELEMENTS FROM MODULE BY ID, DO {NS}-{id} .{child}
    
    tags$head(tags$style(HTML('
                        #dashboardTab-avgRisk .small-box {
                        width: 225px;
                        height: 175px;
                        }
                        
                        hr {border-top: 0px solid #000000;}
                        
                        /* Div that contains a single button that allows us to 
                        center it horizontally in the column */
                        #dashboardTab-sixthGradeButtonDiv {
                        text-align: center;
                        }
                        
                        #dashboardTab-sixthGradeButton {
                        width: 90%;
                        margin: auto;
                        }
                        
                        #dashboardTab-seventhGradeButtonDiv {
                        text-align: center;
                        }
                        
                        #dashboardTab-seventhGradeButton {
                        width: 90%;
                        margin: auto;
                        }
                        
                        #dashboardTab-eighthGradeButtonDiv {
                        text-align: center;
                        }
                        
                        #dashboardTab-eighthGradeButton {
                        width: 90%;
                        margin: auto;
                        }
                        
                        #dashboardTab-gradeSummaryBox .box-header {
                        display: none;
                        }
                        
                        #dashboardTab-avgRisk .inner {
                        align-text: left;
                        padding: 50px;
                        }
                        
                        #dashboardTab-avgRisk h3 {
                        font-size: 50px;
                        }
                        
                        #dashboardTab-avgRisk p {
                        font-size: 20px;
                        }
                        
                        .small-box.bg-green {
                        background-color: #106849 !important
                        };
                        
                        .small-box h3 {
                          color: #f5f5f5;
                        }
                        
                        .small-box p {
                        color: #f5f5f5;
                        }
                        
                        .small-box.bg-yellow {
                        background-color: #9bcb3b !important
                        };
                        ')))
  )
  
  # #94bc5c
}

# ---- SERVER ----
schoolTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
    
    # ---- RENDER VALUEBOXOUTPUTS ----
    output$avgRisk <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$midasRisk)),
        subtitle = "Average Risk",
        color = "green"
      )
    })
    
    # ---- VB TRS Scores ----
    
    output$avgTrs <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$trsTotalBehavior)),
        subtitle = "TRS-SAEBRS Total",
        color = "green"
      )
    })
    
    output$avgTrsSocial <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$trsSocialBehavior)),
        subtitle = "TRS-SAEBRS Social",
        color = "green"
      )
    })
    
    output$avgTrsAcademic <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$trsAcademicBehavior)),
        subtitle = "TRS-SAEBRS Academic",
        color = "green"
      )
    })
    
    output$avgTrsEmotional <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$trsEmotionalBehavior)),
        subtitle = "TRS-SAEBRS Emotional",
        color = "green"
      )
    })

    # ---- VB MySAEBRS Scores ----
        
    output$avgMySaebrs <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$totalBehavior)),
        subtitle = "MySAEBRS Total",
        color = "green"
      )
    })
    
    output$avgMySocial <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$socialBehavior)),
        subtitle = "MySAEBRS Social",
        color = "green"
      )
    })
    
    output$avgMyAcademic <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$academicBehavior)),
        subtitle = "MySAEBRS Academic",
        color = "green"
      )
    })
    
    output$avgMyEmotional <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$emotionalBehavior)),
        subtitle = "MySAEBRS Emotional",
        color = "green"
      )
    })
    
    # ---- VB SIXTH GRADE ----
    sixthGraders <- reactive(subset(uploadedData(), grade == 6))
    seventhGraders <- reactive(subset(uploadedData(), grade == 7))
    eighthGraders <- reactive(subset(uploadedData(), grade == 8))
    
    # THIS WILL PROBABLY CHANGE - WE DONT KNOW HOW THE RISK SCORE WORKS AND WHAT CLASSIFIES AS HIGH RISK
    lowRiskThresh <- 17
    medRiskThresh <- 34
    highRiskThresh <- 51
    
    output$sixthStudentCount <- renderValueBox({
      valueBox(
        nrow(sixthGraders()),
        "# of 6th Graders",
        color = "green"
      )
    })

    output$sixthHighRiskPerc <- renderValueBox({
      valueBox(
        scales::percent(nrow(subset(sixthGraders(), midasRisk < highRiskThresh & midasRisk > medRiskThresh)) / nrow(sixthGraders())),
        "High Risk %",
        color = "green"
      )
    })
    
    # ---- VB SEVENTH GRADE ----
    
    output$seventhStudentCount <- renderValueBox({
      valueBox(
        nrow(seventhGraders()),
        "# of 7th Graders",
        color = "green"
      )
    })
    
    output$seventhHighRiskPerc <- renderValueBox({
      valueBox(
        scales::percent(nrow(subset(seventhGraders(), midasRisk < highRiskThresh & midasRisk > medRiskThresh)) / nrow(seventhGraders())),
        "High Risk %",
        color = "green"
      )
    })
    
    # ---- VB EIGHTH GRADE ----
    
    output$eighthStudentCount <- renderValueBox({
      valueBox(
        nrow(eighthGraders()),
        "# of 8th Graders",
        color = "green"
      )
    })
    
    output$eighthHighRiskPerc <- renderValueBox({
      valueBox(
        scales::percent(nrow(subset(eighthGraders(), midasRisk < highRiskThresh & midasRisk > medRiskThresh)) / nrow(eighthGraders())),
        "High Risk %",
        color = "green"
      )
    })
    
    # ---- RENDER PLOTS ----
    
    gradeData <- reactive(
      uploadedData() %>%
      group_by(grade) %>%
      summarize(mean_risk = mean(midasRisk))
    )
    
    print(head(gradeData))
    
    output$gradeRiskHist <- renderPlot({
      ggplot(data = gradeData(), aes(x = as.factor(grade), y = mean_risk)) +
        geom_bar(stat = "identity", width = 0.3, fill = "#106849") +
        geom_text(aes(label = round(mean_risk)), hjust = -1) +
        
        theme_bw() +
        labs(
          x = "Grade",
          y = "Mean MIDAS Risk Score"
        ) + 
        ylim(0, 40) +
        theme(
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 13)
        ) +
        coord_flip()
    })
  })
  
  
  # ---- BUTTON EVENTS ----
  
  
}

# MIDAS COLOR SCHEMES

# Best 
# blue grey to white to light green to dark green
# #183239,#617b84,#b1ced7,#f5f5f5,#73a944,#498848,#106849

# USF MIDAS PALETTE
# Dark green to light green to blue grey
# #106849,#498848,#73a944,#9bcb3b,#96b2bb,#546f77,#183239

# Another USF Palette
# #00391e,#287252,#5faf8c,#a4e0c4,#f5f5f5,#86b147,#719652,#5c7c5e,#476169
# Dark green to white to light green to blue grey

# DARK GREEN BLUE TO GREY GRADIENT PALETTE 
#00876c
#6aaa96
#aecdc2
#f1f1f1
#b5bec1
#7d8e94
#476169

# DARK GREEN TO GREEN-YELLOW PALETTE
#106849
#29814d
#499b4c
#6fb346
#9bcb3b