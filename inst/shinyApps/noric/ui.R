#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    title="NORIC",
    theme = "bootstrap.css",
    
    # Application title
    #titlePanel("NORIC"),
    
    tabPanel(
      "Stentbruk",
      sidebarLayout(
        sidebarPanel(
          radioButtons('format', 'Format for nedlasting', c('PDF', 'HTML', 'BEAMER', 'REVEAL'),
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
          radioButtons('format', 'Format for nedlasting', c('PDF', 'HTML', 'BEAMER', 'REVEAL'),
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
