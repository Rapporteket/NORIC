#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # will data be passed along to knit?
  output$reshID <- renderPrint({
    rapbase::getShinyUserReshId(session, testCase = TRUE)
  })
  
  output$stentbruk <- renderUI({
    # set param needed for knitting
    params <- list(tableFormat="html")
    shiny::HTML(
      markdown::markdownToHTML(
        knitr::knit(
          system.file("NORIC_local_monthly_stent.Rmd", package="noric")
        ), options = c('fragment_only', 'base64_images')
      )
    )
  })
  
  output$prosedyrer <- renderUI({
    # set param needed for knitting
    params <- list(tableFormat="html")
    shiny::HTML(
      markdown::markdownToHTML(
        knitr::knit(
          system.file("NORIC_local_monthly.Rmd", package="noric")
        ), options = c('fragment_only', 'base64_images')
      )
    )
  })
  
  output$downloadReportStentbruk <- downloadHandler(
    filename = function() {
      # add secs since epoch to provide a timestamp and pseudo-unique filename
      paste(paste0('NORIC_local_monthly_stent',
                   as.character(as.numeric(as.POSIXct(Sys.time())))),
            sep = '.', switch(
              input$format, PDF = 'pdf', HTML = 'html', REVEAL = 'html',
              BEAMER = 'pdf')
      )
    },
    
    content = function(file) {
      src <- normalizePath(system.file("NORIC_local_monthly_stent.Rmd",
                                       package="noric"))
      hospitalName <- rapbase::getShinyUserReshId(session, TRUE) %>% 
        noric::getHospitalName()
      
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'tmpNoricStent.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('tmpNoricStent.Rmd', output_format = switch(
        input$format,
        #PDF = pdf_document(), HTML = html_document(), Word = word_document()
        PDF = pdf_document(),
        HTML = html_document(),
        BEAMER = beamer_presentation(theme = "Hannover"),
        REVEAL = revealjs::revealjs_presentation(theme = "sky",
                                                 css = "bootsrap.css")
      ), params = list(tableFormat=switch(
        input$format,
        PDF = "latex",
        HTML = "html",
        BEAMER = "latex",
        REVEAL = "html"), hospitalName=hospitalName
      ), output_dir = tempdir())
      file.rename(out, file)
    }
  )
  
  output$downloadReportProsedyrer <- downloadHandler(
    filename = function() {
      # add secs since epoch to provide a timestamp and pseudo-unique filename
      paste(paste0('NORIC_local_monthly',
                   as.character(as.numeric(as.POSIXct(Sys.time())))),
            sep = '.', switch(
              input$format, PDF = 'pdf', HTML = 'html', REVEAL = 'html',
              BEAMER = 'pdf')
            )
    },
    
    content = function(file) {
      src <- normalizePath(system.file("NORIC_local_monthly.Rmd",
                                       package="noric"))
      hospitalName <- rapbase::getShinyUserReshId(session, TRUE) %>% 
        noric::getHospitalName()
      
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'tmpNoric.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('tmpNoric.Rmd', output_format = switch(
        input$format,
        #PDF = pdf_document(), HTML = html_document(), Word = word_document()
        PDF = pdf_document(),
        HTML = html_document(),
        BEAMER = beamer_presentation(theme = "Hannover"),
        REVEAL = revealjs::revealjs_presentation(theme = "sky")
      ), params = list(tableFormat=switch(
        input$format,
        PDF = "latex",
        HTML = "html",
        BEAMER = "latex",
        REVEAL = "html"), hospitalName=hospitalName
        ), output_dir = tempdir())
      file.rename(out, file)
    }
  )
})
