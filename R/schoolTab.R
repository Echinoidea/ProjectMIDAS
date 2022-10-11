# ---- UI ----
schoolTabUI <- function(id) {
  ns <- NS(id)
  # $("#schoolTabUI_avgRisk").height("500px");
  
  
  tabItem(
    tabName = "dashboardTab",
    
    fluidRow(
      # Average MIDAS Risk for whole school and SAEBRS scores for whole school
      column(
        4,
        
        box(
          # ---- Average Risk ----
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
          
          hr(),
          
          # ---- SAEBRS TRS ----
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
          
          # ---- MySAEBRS ----
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
      column(
        8,
        
      )
      
    ),
    # FINALLY!!!!!!
    # TO ACCESS ELEMENTS FROM MODULE BY ID, DO {NS}-{id} .{child}
    tags$head(tags$style(HTML('
                        #dashboardTab-avgRisk .small-box {
                        width: 225px;
                        height: 175px;
                        }
                        
                        hr {border-top: 0px solid #000000;}"
                        ')))
  )
  
  
}

# ---- SERVER ----
schoolTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
    
    # ---- RENDER VALUEBOXOUTPUTS ----
    output$avgRisk <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$midasRisk)),
        subtitle = "Average Risk"
      )
    })
    
    # -- TRS Scores --
    
    output$avgTrs <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$trsTotalBehavior)),
        subtitle = "TRS-SAEBRS Total"
      )
    })
    
    output$avgTrsSocial <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$trsSocialBehavior)),
        subtitle = "TRS-SAEBRS Social           "
      )
    })
    
    output$avgTrsAcademic <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$trsAcademicBehavior)),
        subtitle = "TRS-SAEBRS Academic"
      )
    })
    
    output$avgTrsEmotional <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$trsEmotionalBehavior)),
        subtitle = "TRS-SAEBRS Emotional"
      )
    })

    # -- MySAEBRS Scores --
        
    output$avgMySaebrs <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$totalBehavior)),
        subtitle = "MySAEBRS Total"
      )
    })
    
    output$avgMySocial <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$socialBehavior)),
        subtitle = "MySAEBRS Social             "
      )
    })
    
    output$avgMyAcademic <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$academicBehavior)),
        subtitle = "MySAEBRS Academic"
      )
    })
    
    output$avgMyEmotional <- renderValueBox({
      valueBox(
        value = round(mean(uploadedData()$emotionalBehavior)),
        subtitle = "MySAEBRS Emotional"
      )
    })
  })
}