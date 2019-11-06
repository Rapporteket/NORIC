#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(magrittr)
library(rapbase)
library(rpivotTable)
library(shiny)
library(shinyalert)
library(shinycssloaders)



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
    
    tabPanel("Krysstabell",
      fluidRow(
        column(6, uiOutput("pivotAction")),
        column(6, uiOutput("pivotControl"))
      ),
      uiOutput("dataSetInfo"),
      fluidRow(
        column(12, rpivotTableOutput("pivotData"))
      )
    ),
    
    tabPanel("Utforsker",
             fluidRow(
               column(6, uiOutput("selectDataSet")),
               column(6, uiOutput("selectVars"))
             ),
             fluidRow(
               column(12, uiOutput("togglePivotSurvey"))
             ),
             fluidRow(
               column(12, rpivotTableOutput("pivotSurvey") %>%
                        withSpinner(color = "#18bc9c",
                                    color.background = "#ffffff",
                                    type = 2))
             )
    ),
    
    ## suggesting replacement by the above
    # tabPanel("TabAnP",
    #          mainPanel(
    #            rpivotTableOutput("tabAnP")
    #          )),
    # 
    # tabPanel("TabAP",
    #          mainPanel(
    #            rpivotTableOutput("tabAP")
    #          )),
    # 
    # tabPanel("TabSO",
    #          mainPanel(
    #            rpivotTableOutput("tabSO")
    #          )),
    
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
          htmlOutput("stentbruk", inline = TRUE) %>%
            withSpinner(color = "#18bc9c",color.background = "#ffffff",
                        type = 2),
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
          htmlOutput("prosedyrer", inline = TRUE) %>%
            withSpinner(color = "#18bc9c",color.background = "#ffffff",
                        type = 2)
        )
      )
    ),
    tabPanel("Abonnement",
      sidebarLayout(
        sidebarPanel(width = 3,
                     selectInput("subscriptionRep", "Rapport:",
                                 c("Stentbruk, månedlig", "Prosedyrer, månedlig")),
                     selectInput("subscriptionFreq", "Frekvens:",
                                 list(Årlig="Årlig-year",
                                       Kvartalsvis="Kvartalsvis-quarter",
                                       Månedlig="Månedlig-month",
                                       Ukentlig="Ukentlig-week",
                                       Daglig="Daglig-DSTday"),
                                 selected = "Månedlig-month"),
                     selectInput("subscriptionFileFormat", "Format:",
                                 c("html", "pdf")),
                     actionButton("subscribe", "Bestill!")
        ),
        mainPanel(
          uiOutput("subscriptionContent")
        )
      )
    )
  )
)
