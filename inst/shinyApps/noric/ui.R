#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(lubridate)
library(magrittr)
library(rapbase)
library(rpivotTable)
library(shiny)
library(shinyalert)
library(shinycssloaders)



addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "NORIC"

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
                              addUserInfo = TRUE),
          tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))
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
    
    tabPanel("Datadump",
      sidebarLayout(
        sidebarPanel(width = 4,
                     selectInput("dumpDataSet", "Velg datasett:",
                                 c("AndreProsedyrerVar",
                                   "AngioPCIVar",
                                   "AnnenDiagnostikkVar",
                                   "AortaklaffOppfVar",
                                   "CTAngioVar",
                                   "ForlopsOversikt",
                                   "MitralklaffOppfVar",
                                   "PasienterStudier",
                                   "SegmentStent",
                                   "SkjemaOversikt")),
                     dateRangeInput("dumpDateRange", "Velg periode:",
                                    start = ymd(Sys.Date())- years(1),
                                    end = Sys.Date(), separator = "-"),
                     radioButtons("dumpFormat", "Velg filformat:",
                                  choices = c("csv", "xlsx")),
                     downloadButton("dumpDownload", "Hent!")
                     ),
        mainPanel(
          htmlOutput("dataDumpInfo") %>% 
            withSpinner(color = "#18bc9c",color.background = "#ffffff",
                        type = 2)
        )
      )),
    
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
