library(shiny) #library for running Shiny Webapp
library(shinyjs) #library for js functions

library(readxl) #library for taking in XLS/XLSX
library(ggplot2) #library for basic plots
library(dplyr)
#library(tidyverse) #megapackage for analysis/operations
library(readr) #library for taking in
library(ggthemes)
library(shinyBS)
library(plotly) # for ggplot tooltips
library(flexdashboard)
library(shinydashboard) #dashboarding library
library(shinydashboardPlus) # For additional shiny dashboard skins and other visuals

# Modular Shiny workflow:
#   - Each R file (module) is essentially a standalone Shiny app, containing a ui
#       function and a server function.
#   - Use the NS() function to create namespaced IDs for each UI element so that
#       the server functions may access them.
#   - Add a source() call in the main midasApp function for each module.



midasApp <- function() {
  source("./R/uploadTab.R")
  source("./R/studentViewTab.R")
  source("./R/schoolTab.R")
  source("./R/classroomTab.r")
  source("./R/popDemographicsTab.R")
  source("./R/popSaebersTab.R")
  source("./R/popDataTab.R")
  
  ui <- dashboardPage(
    dashboardHeader(title = "Project MIDAS"),
    # Sidebar =====
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem(text = "Upload Data", tabName = "uploadTab"),
        menuItem(text = "Student", tabName = "studentTab"),
        menuItem(
          text = "School",
          tabName = "schoolTab",
          menuSubItem(
            text = "School Dashboard",
            tabName = "dashboardTab"
          ),
          menuSubItem(
            text = "Classroom",
            tabName = "classroomTab"
          ),
          menuSubItem(
            text = "Demographics",
            tabName = "popDemographicsTab"
          ),
          menuSubItem(
            text = "SAEBERS",
            tabName = "popSaebersTab"
          ),
          menuSubItem(
            text = "Data",
            tabName = "popDataTab"
          )
        )
      )
    ),
    
    dashboardBody(shinyjs::useShinyjs(),
        # Edit CSS elements here
        tags$head(tags$style(HTML(
          '/* CSS for the dropdown buttons on studentViewTab plot boxes */
          .dropdown-toggle {
            visibility: hidden;
          }
          
          .dropdown-toggle:after {
            content: "Select SAEBERS Score";
            border: 2px solid Gainsboro;
            border-radius: 4px;
            text-align: center;
            padding: 0.75em 1em;
            visibility: visible;
          }
          '
        ))),
        
      fluidPage(
        tabItems(
          
          # Upload - Uploading Data to be sent to Dashboard
          uploadTabUI("uploadTab"),
          
          # View individual students data
          studentViewTabUI("studentViewTab"),
          
          # School population tabs
          schoolTabUI("dashboardTab"),
          classroomTabUI("classroomTab"),
          popDemographicsTabUI("popDemographicsTab"),
          popSaebersTabUI("popSaebersTab"),
          popDataTabUI("popDataTab")
        )
        #end of tabs))
      ))
  )
  
  # Server -----
  server <- function(input, output, session) {
    uploadedData <- uploadTabServer("uploadTab")
    studentViewTabServer("studentViewTab", uploadedData)
    
    schoolTabServer("dashboardTab", uploadedData)
    classroomTabServer("classroomTab", uploadedData, parent_session = session)
    popDemographicsTabServer("popDemographicsTab", uploadedData)
    popSaebersTabServer("popSaebersTab", uploadedData)
    popDataTabServer("popDataTab", uploadedData)
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

# > RColorBrewer::brewer.pal(9, "Greens")
# [1] "#F7FCF5" "#E5F5E0" "#C7E9C0" "#A1D99B" "#74C476" "#41AB5D" "#238B45" "#006D2C" "#00441B"


# New color scheme ?
# 86E3CE • D0E6A5 • FFDD94 • FA897B • CCABD8
# https://ro.pinterest.com/pin/345510602668902064/