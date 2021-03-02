library(lubridate)
library(magrittr)
library(rapbase)
library(rpivotTable)
library(shiny)
library(shinyalert)
library(shinycssloaders)


addResourcePath("rap", system.file("www", package = "rapbase"))
regTitle <- "NORIC"

ui <- tagList(
  navbarPage(
    title = div(a(includeHTML(system.file("www/logo.svg",
                                          package = "rapbase"))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
    id = "tabs",

    tabPanel("Start",
      useShinyalert(),
      mainPanel(width = 12,
        htmlOutput("veiledning", inline = TRUE),
        appNavbarUserWidget(user = uiOutput("appUserName"),
                           organization = uiOutput("appOrgName"),
                           addUserInfo = TRUE),
        tags$head(tags$link(rel = "shortcut icon", href = "rap/favicon.ico"))
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
               column(12,
                      rpivotTableOutput("pivotSurvey") #%>%
                        # shinycssloaders::withSpinner(
                        #   color = "#18bc9c",
                        #   color.background = "#ffffff",
                        #   type = 2)
                      )
             )
    ),

    tabPanel(
      "Stentbruk",
      sidebarLayout(
        sidebarPanel(
          radioButtons("formatStentbruk",
                       "Format for nedlasting",
                       c("PDF", "HTML"),
                       inline = FALSE),
          downloadButton("downloadReportStentbruk", "Hent!"),
          width = 2
        ),
        mainPanel(
          htmlOutput("stentbruk", inline = TRUE) %>%
            shinycssloaders::withSpinner(color = "#18bc9c",
                                         color.background = "#ffffff",
                                         type = 2,
                                         size = 1,
                                         proxy.height = "1px")
        )
      )
    ),
    tabPanel(
      "Prosedyrer",
      sidebarLayout(
        sidebarPanel(
          radioButtons("formatProsedyrer",
                       "Format for nedlasting",
                       c("PDF", "HTML"),
                       inline = FALSE),
          downloadButton("downloadReportProsedyrer", "Hent!"),
          width = 2
        ),
        mainPanel(
          htmlOutput("prosedyrer", inline = TRUE) %>%
          shinycssloaders::withSpinner(color = "#18bc9c",
                                       color.background = "#ffffff",
                                       type = 2,
                                       size = 1,
                                       proxy.height = "1px")

        )
      )
    ),
    tabPanel(
      "Prosedyrer2",
      sidebarLayout(
        sidebarPanel(
          radioButtons("formatProsedyrer2",
                       "Format for nedlasting",
                       c("PDF", "HTML"),
                       inline = FALSE),
          downloadButton("downloadReportProsedyrer2", "Hent!"),
          width = 2
        ),
        mainPanel(
          htmlOutput("prosedyrer2", inline = TRUE) #%>%
            # shinycssloaders::withSpinner(color = "#18bc9c",
            #                              color.background = "#ffffff",
            #                              type = 2)
        )
      )
    ),


    tabPanel("Datadump",
      sidebarLayout(
        sidebarPanel(width = 4,
                     selectInput("dumpDataSet", "Velg datasett:",
                                 c("AndreProsedyrerVar",
                                   "AnnenDiagnostikkVar",
                                   "AngioPCIVar",
                                   "AortaklaffVar",
                                   "AortaklaffOppfVar",
                                   "CTAngioVar",
                                   "ForlopsOversikt",
                                   "MitralklaffVar",
                                   "PasienterStudier",
                                   "SegmentStent",
                                   "SkjemaOversikt")),
                     dateRangeInput("dumpDateRange", "Velg periode:",
                                    start = ymd(Sys.Date()) - years(1),
                                    end = Sys.Date(), separator = "-",
                                    weekstart = 1),
                     radioButtons("dumpFormat", "Velg filformat:",
                                  choices = c("csv", "xlsx-csv")),
                     downloadButton("dumpDownload", "Hent!")
                     ),
        mainPanel(
          htmlOutput("dataDumpInfo") #%>%
            # shinycssloaders::withSpinner(color = "#18bc9c",
            #                              color.background = "#ffffff",
            #                              type = 2)
        )
      )
    ),

    tabPanel("Metadata",
      sidebarLayout(
        sidebarPanel(uiOutput("metaControl")),
        mainPanel(htmlOutput("metaData"))
      )
    ),

    tabPanel("Abonnement",
      sidebarLayout(
        sidebarPanel(width = 3,
                     uiOutput("subscriptionRepList"),
                     selectInput("subscriptionFreq", "Frekvens:",
                                 list(Årlig = "Årlig-year",
                                       Kvartalsvis = "Kvartalsvis-quarter",
                                       Månedlig = "Månedlig-month",
                                       Ukentlig = "Ukentlig-week",
                                       Daglig = "Daglig-DSTday"),
                                 selected = "Månedlig-month"),
                     selectInput("subscriptionFileFormat", "Format:",
                                 c("html", "pdf")),
                     actionButton("subscribe", "Bestill!")
        ),
        mainPanel(
          uiOutput("subscriptionContent")
        )
      )
    ),
    
    shiny::tabPanel("Utsending",
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput("report"),
          uiOutput("freq"),
          textInput("email", "Epostmottakere:"),
          uiOutput("editEmail"),
          htmlOutput("recipients"),
          tags$hr(),
          uiOutput("makeDispatchment")
        ),
        mainPanel(
          uiOutput("dispatchmentContent")
        )
      )
    )
  )
)
