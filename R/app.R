library(shiny) #library for running Shiny Webapp
library(shinyjs) #library for js functions
library(shinydashboard) #dashboarding library
library(readxl) #library for taking in XLS/XLSX
library(ggplot2) #library for basic plots
#library(tidyverse) #megapackage for analysis/operations
library(readr) #library for taking in

# Modular Shiny workflow:
#   - Each R file (module) is essentially a standalone Shiny app, containing a ui 
#       function and a server function. 
#   - Use the NS() function to create namespaced IDs for each UI element so that 
#       the server functions may access them.
#   - Add a source() call in the main midasApp function for each module.


midasApp <- function(...) {
  source("./R/archiveTab.R")
  source("./R/classTab.R")
  source("./R/schoolTab.R")
  source("./R/studentViewTab.R")
  source("./R/uploadTab.R")
  
  # UI -----
  ui <- dashboardPage(
    dashboardHeader(title = "Project MIDAS"),
    # Sidebar =====
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem(
          text = "Upload Data",
          tabName = "Upload",
          icon = icon("tachometer-alt")
        ),
        menuItem(
          text = "Dashboard",
          tabName = "Dashboard",
          icon = icon("tachometer-alt")
        ),
        menuItem("Student", tabName = "studentTab"),
        menuItem("Class", tabName = "classTab"),
        menuItem("Archive", tabName = "archiveTab")
      )
    ),
    
    # Body =====
    body = dashboardBody(shinyjs::useShinyjs(),
                         fluidPage(
                           tabItems(
                             #Upload - Uploading Data to be sent to Dashboard
                             uploadTabUI("uploadTab"),
                             #School - Displaying School Data from Upload
                             schoolTabUI("schoolTab"),
                             #Student - Displaying student data
                             studentViewTabUI("studentViewTab"),
                             #Class - Displaying class data
                             classTabUI("classTab"),
                             #Archive - under construction?
                             archiveTabUI("archiveTab")
                           )
                           #end of tabs
                         ))
  )
  
  # Server -----
  server <- function(input, output) {
    uploadTabServer("uploadTab")
    schoolTabServer("schoolTab")
    studentViewTabServer("studentViewTab")
    classTabServer("classTab")
    archiveTabServer("archiveTab")
  }
  
  # *Shiny Initialization* ----
  shinyApp(ui = ui, server = server)
}

midasApp()
