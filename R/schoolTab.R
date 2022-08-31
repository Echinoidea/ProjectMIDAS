# ---- UI ----

# Page with multiple tabs: SAEBERS, Demographics, Data (summary and dataset)
# The SAEBERS page will contain similar graphs to the studentView graphs. I need
# to think of how to make it unique and useful.

# The demographics tab will be a 2x2 grid of plots. The layout will be:
# [Gender Pie chart]    [Ethnicity Histogram]
# [Special Ed pie]      [Grade Histogram]
# Consider adding interactivity to show some data about each category of student.
# Like, click the male segment of the gender pie chart to bring a pop-up of 
# some data summary of all the male students. Probably focus on showing more
# demographic info instead of SAEBERS because that will be on the SAEBERS tab.
# Use shinyBS package for modal popups and tooltips for displaying extra demo info

# The Data tab will be a more raw view of the dataset. Make a summary section
# and a data view section. Idk how to make data view more attractive. Also, 
# maybe add a download to csv, xlsx, etc. button.
# Use renderDataTable

schoolTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "schoolTab",
    
    # IF I STICK WITH SPLITTING UP SCHOOL INTO THREE TABS, THIS FILE WILL BE 
    # DELETED BECAUSE IT IS NEVER ACCESSED
    
    p("bye")
  )
}

# ---- SERVER ----
schoolTabServer <- function(id, uploadedData) {
  moduleServer(id, function(input, output, session) {
    
  })
}