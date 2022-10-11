# submenuitem for the school tab


popDemographicsTabUI <- function(id) {
  ns <- NS(id)
  
  tags$head(
    tags$style(
      HTML(
        "
        #bottommap-genderRisk {
          height: 200px;
        }
        "
      )
    )
  )
  
  tabItem(
    tabName = "popDemographicsTab",
    
    fluidPage(
      fluidRow(
        box(
          title = "Gender",
          plotOutput(NS(id, "genderHist")),
          width = 6
        ),
        box(
          title = "Ethnicity",
          plotOutput(NS(id, "ethnicityHist")),
          width = 6
        )
      ),
      fluidRow(
        box(
          title = "Special Education Status",
          plotOutput(NS(id, "specialEdHist")),
          width = 6
        ),
        box(
          title = "Grade",
          plotOutput(NS(id, "gradeHist")),
          width = 6
        )
      ),
      
      #bsModal(id = (NS(id, "bsModalTest")), title = "Modal Test", "genderHist", size = "large", p("this is a modal"))
      
      # ---- MODALS ----
      bsModal(id = NS(id, "genderModal"),
              title = "Gender Statistics",
              trigger = "genderHist",
              size = "large",
              
              # Counts for each factor
              # 
              # Box plot for each factor
              # 
              
              # Display average midas risk for group, and tab panel to view trs and mysaebers scores
              
              tabsetPanel(
                tabPanel(
                  "Female",
                  
                  fluidRow(
                    column(
                      4,
                      style = "margin-top:50px",
                      flexdashboard::valueBoxOutput(NS(id, "genderRisk"), width = 12, height = "250px")
                    ),
                    
                    column(
                      8,
                      style = "margin-top:5px",
                      # box(
                      tabsetPanel(
                        tabPanel(
                          "TRS/SRS",
                          fluidRow(
                            valueBoxOutput(NS(id, "genderTrsTotal"), width = 12)
                          ),
                          
                          fluidRow(
                            valueBoxOutput(NS(id, "genderTrsSocial"), width = 4),
                            valueBoxOutput(NS(id, "genderTrsAcademic"), width = 4),
                            valueBoxOutput(NS(id, "genderTrsEmotional"), width = 4)
                          )
                        )
                      )
                      # )
                      #tags$head(tags$style('#foo .box-header{ display: none}'))
                    )
                  )
                )
              ),
              
              
              box(
                title = "MIDAS Risk Distribution",
                plotOutput(NS(id, "genderRiskDensity")),
                width = 12
              )
      )
    )
  )
}

# ---- SERVER ----
popDemographicsTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
    # ---- DEMOGRAPHICS PLOTS ----
    
    output$genderHist <- renderPlot({
      ggplot(data = uploadedData(), aes(gender, fill = gender)) +
        geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)),
                width = 0.5, 
                stat= "count") +
        geom_text(aes(label = scales::percent(round((..count..)/sum(..count..), 2)),
                  y= ((..count..)/sum(..count..))), 
                  stat="count",
                  color = "#333333",
                  vjust = -0.3) +
        scale_x_discrete(labels = c("Female", "Male")) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
        
        # Visuals
        theme_bw() +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = 15),
          legend.position = "none"
        ) +
        scale_fill_brewer(palette = "YlGnBu")
    })
    # TODO: Adding percentages above bins, make bins look less ugly, find better color scheme
    
    output$ethnicityHist <- renderPlot({
      ggplot(data = uploadedData(), aes(x = ethnicity)) +
        geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)),
                 width = 0.5, 
                 stat= "count") +
        geom_text(aes(label = scales::percent(round((..count..)/sum(..count..), 2)),
                      y= ((..count..)/sum(..count..))), 
                  stat="count",
                  color = "#333333",
                  vjust = -0.3) +
        scale_x_discrete(labels = c("American Indian or\nNative Alaskan", 
                                    "Asian", 
                                    "Black or\nAfrican American", 
                                    "Native Hawaiian or\n Other Pacific Islander", 
                                    "White")) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
        
        # Visuals
        theme_bw() +
        theme(
          axis.title = element_blank(),
          axis.text.y = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          legend.position = "none"
        ) +
        scale_fill_brewer(palette = "YlGnBu")
    })
    
    output$specialEdHist <- renderPlot({
      ggplot(data = uploadedData(), aes(x = specialEducation)) +
        geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)),
                 width = 0.5, 
                 stat= "count") +
        geom_text(aes(label = scales::percent(round((..count..)/sum(..count..), 2)),
                      y= ((..count..)/sum(..count..))), 
                  stat="count",
                  color = "#333333",
                  vjust = -0.3) +
        scale_x_discrete(labels = c("Special Education", "Not Special Education")) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
        
        # Visuals
        theme_bw() +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = 15),
          legend.position = "none"
        ) +
        scale_fill_brewer(palette = "YlGnBu")
    })
    
    output$gradeHist <- renderPlot({
      ggplot(data = uploadedData(), aes(x = as.factor(grade))) +
        geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)),
                 width = 0.5, 
                 stat= "count") +
        geom_text(aes(label = scales::percent(round((..count..)/sum(..count..), 2)),
                      y= ((..count..)/sum(..count..))), 
                  stat="count",
                  color = "#333333",
                  vjust = -0.3) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
        
        # Visuals
        theme_bw() +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(size = 15),
          legend.position = "none"
        ) +
        scale_fill_brewer(palette = "YlGnBu")
    })
    
    
    # ---- GENDER PLOTS ----
    
    # sAEBERS-TRS/SRS Total behavior boxplot by gender
    output$genderMidasBoxplot <- renderPlot({
      #ggplot(data = subset(uploadedData(), gender == "")) +
      ggplot() +
        # Geom layer - Boxplot, point, and label
        geom_boxplot(data = subset(uploadedData(), gender == "female"),
                     aes(x = midasRisk, y = 0.5),
                     width = 0.3) +
        # Stat summary to label min, Q1, Q2, Q3, and max
        stat_summary(
          data = subset(uploadedData(), gender == "female"),
          geom = "text",
          fun = quantile, #function(x) boxplot.stats(x)$stats,
          aes(x = midasRisk, y = 0.5, label = ..x..),
          position = position_nudge(y = -0.25),
          size = 4,
          color = "#FB8072",
          orientation = "y"
        ) +
        
        geom_boxplot(data = subset(uploadedData(), gender == "male"),
                     aes(x = midasRisk, y = -0.5),
                     width = 0.3) +
        # Stat summary to label min, Q1, Q2, Q3, and max
        stat_summary(
          data = subset(uploadedData(), gender == "male"),
          geom = "text",
          fun = quantile, #function(x) boxplot.stats(x)$stats,
          aes(x = midasRisk, y = -0.5, label = ..x..),
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
    
    
    output$genderRiskDensity <- renderPlot({
      ggplot(uploadedData(), aes(x = midasRisk)) +
        # Geom layer - Density, vertical line, label
        geom_density() + #adjust = 0.5
          
          #scale_y_continuous(labels = scales::percent) +
          xlim(0, 50) +
          labs(title = "",
               x = "MIDAS Risk Score",
               y = "Number of Students") +
          theme_bw() +
          theme(
            legend.position = "none",
            title = element_blank(),
          )
    })
    
    
    # ---- GENDER VALUEBOX RENDERING ----
    
    output$genderRisk <- renderValueBox({
      valueBox(
        value = round(mean(subset(uploadedData(), gender == "female")$midasRisk)),
        subtitle = "MIDAS Risk"
      )
    })
    
    output$genderTrsTotal <- renderValueBox({
      valueBox(
        value = round(mean(subset(uploadedData(), gender == "female")$trsTotalBehavior)),
        subtitle = "Average Total"
      )
    })
    
    output$genderTrsSocial <- renderValueBox({
      valueBox(
        value = round(mean(subset(uploadedData(), gender == "female")$trsSocialBehavior)),
        subtitle = "Average Social"
      )
    })
    
    output$genderTrsAcademic <- renderValueBox({
      valueBox(
        value = round(mean(subset(uploadedData(), gender == "female")$trsAcademicBehavior)),
        subtitle = "Average Academic"
      )
    })
    
    output$genderTrsEmotional <- renderValueBox({
      valueBox(
        value = round(mean(subset(uploadedData(), gender == "female")$trsEmotionalBehavior)),
        subtitle = "Average Emotional"
      )
    })
    
    # ---- PLOT ONCLICK EVENT OBSERVERS ----
    onclick("genderHist", toggleModal(session, "genderModal"))
    
  })
}

# #2C6975 #6882A0 #CDEDC9 #E0ECDE #FFFFF 

# #333333, #643173, #7d5ba6, #86a59c, #89ce94

# Using ggtips from https://github.com/Roche/ggtips for ggplot tooltips