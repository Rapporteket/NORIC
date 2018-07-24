#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # will data be passed along to knit?
  output$reshID <- renderPrint({
    rapbase::getShinyUserReshId(session, testCase = TRUE)
  })
  
  output$distPlot1 <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$prosedyrer <- renderUI({
    shiny::HTML(
      markdown::markdownToHTML(
        knitr::knit(
          system.file("NORIC_local_monthly.Rmd", package="noric")
        ),
        options = c("fragment_only")
      )
    )
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML5 = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      #src <- normalizePath('testNoric.Rmd')
      src <- normalizePath(system.file("NORIC_local_monthly.Rmd",
                                       package="noric"))
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'testNoric.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('testNoric.Rmd', switch(
        input$format,
        #PDF = pdf_document(), HTML = html_document(), Word = word_document()
        PDF = pdf_document(), HTML5 = revealjs::revealjs_presentation(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
})
