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
addResourcePath('rap', system.file('www', package='rapbase'))

shinyUI(
  navbarPage(
    title = div(img(class="icon", src="Logo-B.svg", alt="Rapporteket", height="26px"),
                regTitle),
    #title = div(class="logo navbar-right", regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
    
    # Application title
    #titlePanel("NORIC"),
    
    tabPanel("Testpanel",
      mainPanel(
        # return from rapbase functions
        h4("Test 'rapbase' functions using the session object:"),
        textOutput("callUser"),
        textOutput("callGroups"),
        textOutput("callReshId"),
        textOutput("callRole"),
        textOutput("callEmail"),
        h4("Environment var R_RAP_INSTANCE:"),
        textOutput("envInstance"),
        h4("Environmental var R_RAP_CONFIG_PATH:"),
        textOutput("envConfigPath"),
        h4("Locale settings:"),
        textOutput("locale")
      )
    ),
    
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
