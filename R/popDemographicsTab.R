# submenuitem for the school tab


popDemographicsTabUI <- function(id) {
  ns <- NS(id)
  
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
      
      #bsModal(id = (NS(id, "bsModalTest")), title = "Modal Test", p("this is a modal"))
    )
  )
}

# ---- SERVER ----
popDemographicsTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
    # openModal <- "
    # function(el, x) {
    #   el.on('plotly_click', function(d) {
    #     var point = d.points[0];
    #     
    #   })
    # }
    # "
    
    # ---- PLOTS ----
    # output$genderHist <- renderPlotly({
    #   
    #   
    #   
    #   p <- ggplot(data = uploadedData(), aes(x = gender)) +
    #     geom_bar() +
    #     theme_bw()
    #     
    #   ggplotly(p, tooltip = c("textF", "textM")) %>%
    #     config(displayModeBar = FALSE)
    # })
    
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
  })
}

# #2C6975 #6882A0 #CDEDC9 #E0ECDE #FFFFF 

# #333333, #643173, #7d5ba6, #86a59c, #89ce94

# Using ggtips from https://github.com/Roche/ggtips for ggplot tooltips