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
  
  # testing css for revealjs, we need a proxy
  # conf <- rapbase::getConfig(fileName = "rapbaseConfig.yml",
  #                            packageName = "rapbase")
  # Sys.setenv(https_proxy=conf$network$proxy$http)
  
  # html rendering function for re-use
  htmlRenderRmd <- function(srcFile) {
    # set param needed for report meta processing
    params <- list(tableFormat="html")
    system.file(srcFile, package="noric") %>% 
      knitr::knit() %>% 
      markdown::markdownToHTML(.,
                               options = c('fragment_only',
                                           'base64_images')) %>% 
      shiny::HTML()
  }
  
  output$stentbruk <- renderUI({
    htmlRenderRmd("NORIC_local_monthly_stent.Rmd")
  })
  
  output$prosedyrer <- renderUI({
    htmlRenderRmd("NORIC_local_monthly.Rmd")
  })
  
  # filename function for re-use
  downloadFilename <- function(fileBaseName, type) {
    paste(paste0(fileBaseName,
                 as.character(as.integer(as.POSIXct(Sys.time())))),
          sep = '.', switch(
            type, 
            PDF = 'pdf', HTML = 'html', REVEAL = 'html', BEAMER = 'pdf')
    )
  }
  
  # render file function for re-use
  contentFile <- function(file, srcFile, tmpFile, type) {
    src <- normalizePath(system.file(srcFile, package="noric"))
    hospitalName <- rapbase::getShinyUserReshId(session, TRUE) %>% 
      noric::getHospitalName()
    
    # temporarily switch to the temp dir, in case we do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFile, overwrite = TRUE)
    
    library(rmarkdown)
    out <- render(tmpFile, output_format = switch(
      type,
      PDF = pdf_document(),
      HTML = html_document(),
      BEAMER = beamer_presentation(theme = "Hannover"),
      REVEAL = revealjs::revealjs_presentation(theme = "sky")
        #css = normalizePath(system.file("bootstrap.css", package = "noric")))
    ), params = list(tableFormat=switch(
      type,
      PDF = "latex",
      HTML = "html",
      BEAMER = "latex",
      REVEAL = "html"), hospitalName=hospitalName
    ), output_dir = tempdir())
    # active garbage collection to prevent memory hogging?
    gc()
    file.rename(out, file)
  }
  
  
  output$downloadReportStentbruk <- downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly_stent",
                                input$formatStentbruk)
    },

    content = function(file) {
      contentFile(file, "NORIC_local_monthly_stent.Rmd", "tmpNoricStent.Rmd",
                  input$formatStentbruk)
    }
  )
  
  output$downloadReportProsedyrer <- downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly", input$formatProsedyrer)
    },
    
    content = function(file) {
      contentFile(file, "NORIC_local_monthly.Rmd", "tmpNoric.Rmd",
                  input$formatProsedyrer)
    }
  )
})
