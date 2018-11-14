#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

regTitle <- "NORIC"

shinyUI(
  navbarPage(
    title = div(img(src="Logo-B.svg", alt="Rapporteket", height="26px"),
                regTitle),
    windowTitle = regTitle,
    theme = "bootstrap.css",
    
    # Application title
    #titlePanel("NORIC"),
    
    tabPanel(
      "Stentbruk",
      sidebarLayout(
        sidebarPanel(
          radioButtons('formatStentbruk',
                       'Format for nedlasting',
                       c('PDF', 'HTML', 'BEAMER', 'REVEAL'),
                       inline = FALSE),
          downloadButton('downloadReportStentbruk', 'Last ned'),
          width = 2
        ),
        mainPanel(
          htmlOutput("stentbruk", inline = TRUE)
        )
      )
    ),
    tabPanel(
      "Prosedyrer",
      sidebarLayout(
        sidebarPanel(
          radioButtons('formatProsedyrer',
                       'Format for nedlasting',
                       c('PDF', 'HTML', 'BEAMER', 'REVEAL'),
                       inline = FALSE),
          downloadButton('downloadReportProsedyrer', 'Last ned'),
          width = 2
        ),
        mainPanel(
          htmlOutput("prosedyrer", inline = TRUE)
        )
      )
    )
  )
)
