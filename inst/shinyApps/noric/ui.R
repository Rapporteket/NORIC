#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(rapbase)
library(rpivotTable)
library(shiny)
library(shinyalert)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "NORIC"
logo <- includeHTML(system.file('www/logo.svg', package='rapbase'))
logoCode <- paste0("var header = $('.navbar> .container-fluid');\n",
                   "header.append('<div class=\"navbar-brand\" style=\"float:left;font-size:75%\">",
                   logo,
                   "</div>');\n",
                   "console.log(header)")
logoWidget <- tags$script(shiny::HTML(logoCode))

ui <- tagList(
  navbarPage(
    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
    
    tabPanel("Testpanel",
      mainPanel(
        # return from rapbase functions
        h4("Test 'rapbase' functions using the session object:"),
        textOutput("callUser"),
        textOutput("callGroups"),
        textOutput("callReshId"),
        textOutput("callRole"),
        textOutput("callEmail"),
        textOutput("callFullName"),
        textOutput("callPhone"),
        h4("Environment var R_RAP_INSTANCE:"),
        textOutput("envInstance"),
        h4("Environmental var R_RAP_CONFIG_PATH:"),
        textOutput("envConfigPath"),
        h4("Locale settings:"),
        textOutput("locale")
      )
    ),
    
    tabPanel("TabAnP",
    #   sidebarLayout(
    #     sidebarPanel(
    #       
    #     ),
        mainPanel(
          rpivotTableOutput("tabAnP")
        )),
    #   )
    # ),
    
    tabPanel("TabAP",
    #   sidebarLayout(
    #     sidebarPanel(
    #       
    #     ),
        mainPanel(
          rpivotTableOutput("tabAP")
        )),
    #   )
    # ),
    
    tabPanel("TabSO",
    #          sidebarLayout(
    #            sidebarPanel(
    #              
    #            ),
               mainPanel(
                 rpivotTableOutput("tabSO")
               )),
    #          )
    # ),
    
    tabPanel(
      "Stentbruk",
      sidebarLayout(
        sidebarPanel(
          radioButtons('formatStentbruk',
                       'Format for nedlasting',
                       c('PDF', 'HTML', 'BEAMER', 'REVEAL'),
                       inline = FALSE),
          downloadButton('downloadReportStentbruk', 'Hent!'),
          width = 2
        ),
        mainPanel(
          useShinyalert(),
          htmlOutput("stentbruk", inline = TRUE),
          appNavbarUserWidget(user = uiOutput("appUserName"),
                              organization = uiOutput("appOrgName"),
                              addUserInfo = TRUE)
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
          downloadButton('downloadReportProsedyrer', 'Hent!'),
          width = 2
        ),
        mainPanel(
          htmlOutput("prosedyrer", inline = TRUE)
        )
      )
    ),
    tabPanel("Abonnement",
      sidebarLayout(
        sidebarPanel(width = 3,
                     selectInput("subscriptionRep", "Rapport:",
                                 c("Stentbruk, månedlig", "Prosedyrer, månedlig",
                                   "Samlerapport1", "Samlerapport2")),
                     selectInput("subscriptionFreq", "Frekvens:",
                                 list(Årlig="Årlig-year",
                                       Kvartalsvis="Kvartalsvis-quarter",
                                       Månedlig="Månedlig-month",
                                       Ukentlig="Ukentlig-week",
                                       Daglig="Daglig-DSTday"),
                                 selected = "Månedlig-month"),
                     actionButton("subscribe", "Bestill!")
        ),
        mainPanel(
          uiOutput("subscriptionContent")
        )
      )
    )
  )
)
