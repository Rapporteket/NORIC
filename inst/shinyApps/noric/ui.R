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
               column(6, uiOutput("utforskerDateRange"))),
             fluidRow(
               column(12, 
                      uiOutput("selectVars"))),
             fluidRow(
               column(12, uiOutput("togglePivotSurvey"))
             ),
             fluidRow(
               column(12,
                      rpivotTableOutput("pivotSurvey")
               )
             )
    ),
    
    shiny::tabPanel("Kodebok",
                    sidebarLayout(
                      sidebarPanel(uiOutput("kbControl")),
                      mainPanel(htmlOutput("kbdData"))
                    )),
    
    
    
    
    shiny::navbarMenu(
      "Månedsrapporter",
      
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
            htmlOutput("stentbruk", inline = TRUE)
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
            htmlOutput("prosedyrer", inline = TRUE)
          )
        )
      ),
      tabPanel(
        "Aktivitet",
        sidebarLayout(
          sidebarPanel(
            radioButtons("formatAktivitet",
                         "Format for nedlasting",
                         c("PDF", "HTML"),
                         inline = FALSE),
            downloadButton("downloadReportAktivitet", "Hent!"),
            width = 2),
          mainPanel(
            htmlOutput("aktivitet", inline = TRUE)
          )
        )
      )) ,
    
    
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
    
    shiny::tabPanel(
      "Abonnement",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          rapbase::autoReportInput("noricSubscription")
        ),
        shiny::mainPanel(
          rapbase::autoReportUI("noricSubscription")
        )
      )
    ),
    
    shiny::navbarMenu(
      "Verktøy",
      
      tabPanel("Metadata",
               sidebarLayout(
                 sidebarPanel(uiOutput("metaControl")),
                 mainPanel(htmlOutput("metaData"))
               )
      ),
      
      shiny::tabPanel(
        "Utsending",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportOrgInput("noricDispatch"),
            rapbase::autoReportInput("noricDispatch")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("noricDispatch")
          )
        )
      ),
      
      
      tabPanel("Nedlasting rapporter",
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("dwnldControlRap"), 
                   uiOutput("dwnldControl")),
                 mainPanel(
                   htmlOutput("dwldInfo"),
                   downloadButton("dwnldReport", "Hent rapport!"))
               )
      ),
      
      shiny::tabPanel(
        "Bruksstatistikk",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::statsInput("noricStats"),
            rapbase::statsGuideUI("noricStatsGuide")
          ),
          shiny::mainPanel(rapbase::statsUI("noricStats"))
        )
      ),
      
      shiny::tabPanel(
        "Eksport",
        shiny::sidebarLayout(
          shiny::sidebarPanel(rapbase::exportUCInput("noricExport")),
          shiny::mainPanel(rapbase::exportGuideUI("noricExportGuide"))
        )
      ), 
      
      # shiny::tabPanel(
      #   "Forvaltning stagingData", 
      #   shiny::sidebarLayout(
      #     shiny::sidebarPanel(htmlOutput("stagingControl")),
      #     shiny::mainPanel(shiny::htmlOutput("stagingData"))
      #   )
      # )
      
      shiny::tabPanel(
        "Forvaltning stagingData", 
        shiny::sidebarLayout(
          shiny::sidebarPanel(htmlOutput("stagingControl")),
          shiny::mainPanel(
            DT::dataTableOutput("stagingDataTable")
          )
        ))
      
      
    )
  )
)

