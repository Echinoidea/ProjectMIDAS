faqTabUI <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "faqTab",
          column(
            12,
            div(style = "background-color: #d0df92; padding: 5px; border-radius: 25px; height: 100%;",
                p("FAQ:"),
                uiOutput(NS(id, "faqtext")))
          ))
}

faqTabServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      output$faqtext <- renderUI({
        filename <- normalizePath(file.path('text', paste('faq', '.txt', sep='')))
        fileText <- readLines(filename)
        splitText <- stringi::stri_split(str = fileText, regex = '\\n')
        replacedText <- lapply(splitText, p)
        return(replacedText)
      })
    }
  )
}
