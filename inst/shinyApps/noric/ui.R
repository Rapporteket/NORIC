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
                      sidebarPanel(uiOutput("kbControl"), width = 2),
                      mainPanel(htmlOutput("kbdData"))
                    )),
    
    
    
    
    shiny::navbarMenu(
      "Månedsrapporter",
      tabPanel(
        "Invasive prosedyrer",
        sidebarLayout(
          sidebarPanel(
            style = "position:fixed;width:130px;",
            h5("Last ned rapporten (pdf)"),
            downloadButton("downloadReportProsedyrer", "Hent!"),
            width = 2
          ),
          mainPanel(
            htmlOutput("prosedyrer", inline = TRUE)
          )
        )
      ),
      tabPanel(
        "Angiografør/Operatør",
        sidebarLayout(
          sidebarPanel(
            style = "position:fixed;width:130px;",
            h5("Last ned rapporten (pdf)"),
            downloadButton("downloadReportAktivitet", "Hent!"),
            width = 2),
          mainPanel(
            htmlOutput("aktivitet", inline = TRUE)
          )
        )
      ), 
      
      
      tabPanel(
        "Aortaklaff",
        sidebarLayout(
          sidebarPanel(
            style = "position:fixed;width:130px;",
            h5("Last ned rapporten (pdf)"),
            downloadButton("downloadReportProsedyrer", "Hent!"),
            width = 2
          ),
          mainPanel(
            htmlOutput("tavi", inline = TRUE)
          )
        )
      )
      
      ) ,

    
    
    shiny::tabPanel("Datadump",
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        width = 4,
                        shiny::uiOutput(outputId = "selectDumpSet"),
                        shiny::dateRangeInput(
                          inputId = "dumpDateRange", 
                          label = "Velg periode:",
                          start = ymd(Sys.Date()) - years(1),
                          end = Sys.Date(), separator = "-",
                          weekstart = 1),
                        shiny::radioButtons(inputId = "dumpFormat",
                                            label = "Velg filformat:",
                                            choices = c("csv", "xlsx-csv")),
                        shiny::downloadButton(outputId = "dumpDownload",
                                              label =  "Hent!")
                      ),
                      mainPanel(
                        htmlOutput("dataDumpInfo") 
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
      
      
      shiny::tabPanel(
        title = "Staging data", 
        
        shiny::titlePanel("Liste alle staging data"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(htmlOutput("stagingControl")),
          
          shiny::mainPanel(DT::dataTableOutput("stagingDataTable"))), 
        
        
        br(),
        shiny::titlePanel("Regelmessing etablering av staging data"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportOrgInput("noricBulletin"),
            rapbase::autoReportInput("noricBulletin")),
          
          shiny::mainPanel(
            rapbase::autoReportUI("noricBulletin")
          )
        )
      )
    )
  )
)

