library(shiny) #library for running Shiny Webapp
library(shinyjs) #library for js functions
library(shinydashboard) #dashboarding library
library(shinydashboardPlus) # For additional shiny dashboard skins and other visuals
library(readxl) #library for taking in XLS/XLSX
library(ggplot2) #library for basic plots
library(dplyr)
#library(tidyverse) #megapackage for analysis/operations
library(readr) #library for taking in
library(ggthemes)

# Modular Shiny workflow:
#   - Each R file (module) is essentially a standalone Shiny app, containing a ui
#       function and a server function.
#   - Use the NS() function to create namespaced IDs for each UI element so that
#       the server functions may access them.
#   - Add a source() call in the main midasApp function for each module.

midasApp <- function() {
  source("./R/archiveTab.R")
  # source("./R/classTab.R")
  source("./R/schoolTab.R")
  source("./R/studentViewTab.R")
  source("./R/uploadTab.R")
  
  ui <- dashboardPage(
    dashboardHeader(title = "Project MIDAS"),
    # Sidebar =====
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Upload Data", tabName = "uploadTab"),
        # menuItem("Dashboard", tabName = "Dashboard"),
        menuItem("Student", tabName = "studentTab"),
        # menuItem("Class", tabName = "classTab"),
        menuItem("School", tabName = "schoolTab"),
        menuItem("Archive", tabName = "archiveTab")
      )
    ),
    
    dashboardBody(shinyjs::useShinyjs(),
                  #skin = "green",
        # tags$head(
        #   includeCSS("www/css.css")
        # ),
        # 
        # tags$style(HTML('body {font-family:"Poppins"}')),
        # #color the body
        # tags$head(tags$style(HTML('
        # /* logo */
        # .skin-blue .main-header .logo {
        #                       background-color: #CCEBC5;
        #                       }
        # 
        # /* logo when hovered */
        # .skin-blue .main-header .logo:hover {
        #                       background-color: #bddbb6;
        #                       }
        # 
        # /* navbar (rest of the header) */
        # .skin-blue .main-header .navbar {
        #                       background-color: #CCEBC5;
        #                       }
        # /* main sidebar */
        # .skin-blue .main-sidebar {
        #                       background-color: #CCEBC5;
        #                       }
        # 
        # /* active selected tab in the sidebarmenu */
        # .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        #                       background-color: #bddbb6;
        #                       }
        # 
        # /* other links in the sidebarmenu */
        # .skin-blue .main-sidebar .sidebar .sidebar-menu a{
        #                       background-color: #CCEBC5;
        #                       color: black;
        #                       }
        # 
        # /* other links in the sidebarmenu when hovered */
        #  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        #                       background-color: #A8DDB5;
        #                       }
        # /* toggle button when hovered  */
        #  .skin-blue .main-header .navbar .sidebar-toggle:hover{
        #                       background-color: #A8DDB5;
        #  }
        # /* body */
        # .content-wrapper, .right-side {
        #                         background-color: #FFFFFF;
        #                         }
        #                       '))),
      
        
        
      fluidPage(
        tabItems(
          # Upload - Uploading Data to be sent to Dashboard
          uploadTabUI("uploadTab"),
          # # School - Displaying School Data from Upload
          # schoolTabUI("schoolTab"),
          # # # Student - Displaying student data
          studentViewTabUI("studentViewTab"),
          schoolTabUI("schoolTab"),
          # # # Class - Displaying class data
          # classTabUI("classTab"),
          # # # Archive - under construction?
          archiveTabUI("archiveTab")
        )
        #end of tabs))
      ))
  )
  
  # Server -----
  server <- function(input, output) {
    uploadedData <- uploadTabServer("uploadTab")
    # schoolTabServer("schoolTab")
    studentViewTabServer("studentViewTab", uploadedData)
    schoolTabServer("schoolTab", uploadedData)
    # classTabServer("classTab")
    archiveTabServer("archiveTab", uploadedData)
  }
  
  shinyApp(ui, server)
}

midasApp()
# Color scheme for UI
# > brewer.pal(9, "GnBu")
# [1] "#F7FCF0" "#E0F3DB" "#CCEBC5" "#A8DDB5" "#7BCCC4" "#4EB3D3" "#2B8CBE" "#0868AC" "#084081"

# Color scheme for graphs
# > RColorBrewer::brewer.pal(12, "Set3")
# [1] "#8DD3C7" "#FFFFB3" "#BEBADA" "#FB8072" "#80B1D3" "#FDB462" "#B3DE69" "#FCCDE5" "#D9D9D9" "#BC80BD" "#CCEBC5" "#FFED6F"

# Color palette for accents
# > RColorBrewer::brewer.pal(9, "Pastel1")
# [1] "#FBB4AE" "#B3CDE3" "#CCEBC5" "#DECBE4" "#FED9A6" "#FFFFCC" "#E5D8BD" "#FDDAEC" "#F2F2F2"