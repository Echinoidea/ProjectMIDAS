guideTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "guideTab",
          fluidRow(
            column(
              12, 
              box(tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/jyRhcV5l6aE", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA))
            ),
            column(
              12,
              
              box(
                p("FAQ:"),
                p("how to interpret data")
                )
            ),
          )
  )
}

guideTabServer <- function(id, uploadedData) {
  moduleServer( id,function(input, output, session) {
    
      output$faqtext <- renderUI({
        filename <- normalizePath(file.path('data', paste('faq', '.txt', sep='')))
        fileText <- readLines(filename)
        splitText <- stringi::stri_split(str = fileText, regex = '\\n')
        replacedText <- lapply(splitText, p)
        replacedText
      })
      
    }
  )
}

