library(shiny) #library for running Shiny Webapp
library(shinyjs) #library for js functions
library(shinydashboard) #dashboarding library
library(readxl) #library for taking in XLS/XLSX
library(ggplot2) #library for basic plots
library(tidyverse) #megapackage for analysis/operations
library(readr) #library for taking in CSV

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Welcome to Project MIDAS Shiny Stub!"
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem(text = "Upload Data", tabName = "Upload", icon = icon("tachometer-alt")),
                menuItem(text = "Dashboard", tabName = "Dashboard", icon = icon("tachometer-alt"))
                )
  ),
  
  body = dashboardBody(
    shinyjs::useShinyjs(),
    fluidPage(
      tabItems(
        #Upload - Uploading Data to be sent to Dashboard
        tabItem(tabName = "Upload",
                fluidRow(
                  box(width = 12,
                    fileInput("file1", "Please Upload File",
                              multiple = FALSE,
                              accept = c("text/csv", ".xlsx",
                                         "text/comma-separated-values,text/plain",
                                         ".csv",
                                         '.xlsx')
                              ),
                    radioButtons("level", "What Total Risk Levels should the Table display?",
                                 c("All" = "all",
                                   "Low" = "low",
                                   "Some" = "some",
                                   "High" = "high"))
                      )
                  #end of box
                  )
                #end of fluidRow
                ),
        #Dashboard - Displaying Data from Upload
        tabItem(tabName = "Dashboard",
                plotOutput("totalBar"),
                plotOutput("socialBar"),
                plotOutput("academicBar"),
                plotOutput("emotionalBar"),
                div(style='height:100vh; width:100vh; overflow: scroll',
                    tableOutput("contentsTable"))
        )
        #next tab
      )
      #end of tabs
    )
  )
)
#end of UI

server <- function(input, output) {
  #Risk minimums reflect total score minimums from fastbridge site Thomas sent.
  lowRiskMin <- 37
  someRiskMin <- 24
  output$totalBar <- renderPlot({
    req(input$file1)
    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
    df_tbrange <- df %>% mutate(ranges = cut(totalBehavior, c(0, 24, 37, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_tbrange
    totalbar <- barplot(df_tbrange$n, main = "Total Behavior Distribution", 
            xlab="Percentage of School",
            col="darkred",
            names.arg=c("High Risk", "Some Risk", "Low Risk")
    )
  })
  
  output$socialBar <- renderPlot({
    req(input$file1)
    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
    df_sbrange <- df %>% mutate(ranges = cut(socialBehavior, c(0, 9, 12, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_sbrange
    totalbar <- barplot(df_sbrange$n, main = "Social Behavior Distribution", 
                        xlab="Percentage of School",
                        col="darkred",
                        names.arg=c("High Risk", "Some Risk", "Low Risk")
    )
  })
  
  output$academicBar <- renderPlot({
    req(input$file1)
    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
    df_abrange <- df %>% mutate(ranges = cut(academicBehavior, c(0, 6, 9, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_abrange
    totalbar <- barplot(df_abrange$n, main = "Academic Behavior Distribution", 
                        xlab="Percentage of School",
                        col="darkred",
                        names.arg=c("High Risk", "Some Risk", "Low Risk")
    )
  })
  
  output$emotionalBar <- renderPlot({
    req(input$file1)
    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
    df_ebrange <- df %>% mutate(ranges = cut(emotionalBehavior, c(0, 7, 10, Inf))) %>%
      group_by(ranges) %>% tally() %>% as.data.frame()
    df_ebrange
    totalbar <- barplot(df_ebrange$n, main = "Emotional Behavior Distribution", 
                        xlab="Percentage of School",
                        col="darkred",
                        names.arg=c("High Risk", "Some Risk", "Low Risk")
    )
  })
  
  output$contentsTable <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, all rows that reflect the selected risk level will be shown.
    
    req(input$file1)

    inFile <- input$file1
    extension <- tools::file_ext(inFile$name)
    filepath <- inFile$datapath
    df <- switch(extension,
                 csv = readr::read_csv(filepath),
                 xls = readxl::read_xls(filepath),
                 xlsx = readxl::read_xlsx(filepath)
    )
    options = list(scrollX = TRUE)
    # This switch is used for demo purposes, needs to be changed for our dashboarding needs
    switch(input$level,
           # Depending on radiobutton selected, the returned table reflects one risk group.
           "low" = return(df[df$totalBehavior > lowRiskMin,]),
           "some" = return(df[df$totalBehavior > someRiskMin & df$totalBehavior < lowRiskMin,]),
           "high" = return(df[df$totalBehavior < someRiskMin,]),
           "all" = return(df))
  })
}
# necessary shiny app line. 
shinyApp(ui = ui, server = server)
