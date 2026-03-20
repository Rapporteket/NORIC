library(lubridate)
library(magrittr)
library(rpivotTable)
library(shiny)
library(shinyalert)
library(shinycssloaders)


regTitle <- "NORIC"

ui <- shiny::tagList(
  shiny::navbarPage(
    title = rapbase::title(regTitle),
    windowTitle = regTitle,
    theme = rapbase::theme(),
    id = "tabs",
    
    shiny::tabPanel(
      title = "Start",
      rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
      shiny::mainPanel(width = 12,
                       shiny::htmlOutput("veiledning", inline = TRUE))
    ),
    
    shiny::tabPanel(
      title = "Utforsker",
      shiny::fluidRow(
        column(6, shiny::uiOutput("selectDataSet")),
        column(6, shiny::uiOutput("utforskerDateRange"))),
      shiny::fluidRow(
        column(12, shiny::uiOutput("selectVars"))),
      shiny::fluidRow(
        column(12, shiny::uiOutput("togglePivotSurvey"))
      ),
      shiny::fluidRow(
        column(12, rpivotTable::rpivotTableOutput("pivotSurvey")))
    ),
    
    
    shiny::tabPanel(
      title = "Kodebok",
      shiny::sidebarLayout(
        shiny::sidebarPanel(shiny::uiOutput("kbControl"), width = 2),
        shiny::mainPanel(shiny::htmlOutput("kbdData")))
    ),
    
    
    shiny::navbarMenu(
      title = "Månedsrapporter",
      
      shiny::tabPanel(
        title = "Invasive prosedyrer",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            style = "position:fixed;width:130px;",
            h5("Last ned rapporten (pdf)"),
            shiny::downloadButton("downloadReportProsedyrer", "Hent!"),
            width = 2
          ),
          shiny:: mainPanel(
            shiny:: htmlOutput("prosedyrer", inline = TRUE)))
      ),
      
      shiny::tabPanel(
        title = "Angiografør/Operatør",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            style = "position:fixed;width:130px;",
            h5("Last ned rapporten (pdf)"),
            shiny::downloadButton("downloadReportAktivitet", "Hent!"),
            width = 2),
          shiny::mainPanel(
            shiny::htmlOutput("aktivitet", inline = TRUE)))
      ), 
      
      shiny::tabPanel(
        title = "Aortaklaff",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            style = "position:fixed;width:130px;",
            h5("Last ned rapporten (pdf)"),
            shiny::downloadButton("downloadReportTavi", "Hent!"),
            width = 2
          ),
          shiny::mainPanel(
            shiny::htmlOutput("tavi", inline = TRUE))))
    ),
    
    
    shiny::tabPanel(
      title = "Datadump",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::uiOutput(outputId = "selectDumpSet"),
          shiny::dateRangeInput(
            inputId = "dumpDateRange", 
            label = "Velg periode:",
            start = as.Date(x = "01-01-2013", format = "%d-%m-%Y"),
            end = Sys.Date(), 
            min = as.Date("2013-01-01", format = "%Y-%m-%d"), 
            separator = "-",
            weekstart = 1),
          shiny::radioButtons(inputId = "dumpFormat",
                              label = "Velg filformat:",
                              choices = c("csv", "xlsx-csv")),
          shiny::downloadButton(outputId = "dumpDownload", label =  "Hent!")
        ),
        shiny::mainPanel(
          shiny::htmlOutput("dataDumpInfo")))
    ),
    
    
    shiny::tabPanel(
      title = "Abonnement",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          rapbase::autoReportInput("noricSubscription")
        ),
        shiny::mainPanel(
          rapbase::autoReportUI("noricSubscription")))
    ),
    
    shiny::navbarMenu(
      title = "Verktøy",
      
      shiny::tabPanel(
        title = "Metadata",
        shiny::sidebarLayout(
          shiny::sidebarPanel(shiny::uiOutput("metaControl")),
          shiny::mainPanel(shiny::htmlOutput("metaData")))
      ),
      
      shiny::tabPanel(
        title = "Utsending",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportOrgInput("noricDispatch"),
            rapbase::autoReportInput("noricDispatch")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("noricDispatch")))
      ),
      
      shiny::tabPanel(
        title = "Nedlasting rapporter",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::uiOutput("dwnldControlRap"), 
            shiny::uiOutput("dwnldControl")),
          shiny::mainPanel(
            shiny::htmlOutput("dwldInfo"),
            shiny::downloadButton("dwnldReport", "Hent rapport!")))
      ),
      
      shiny::tabPanel(
        title = "Bruksstatistikk",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::statsInput("noricStats"),
            rapbase::statsGuideUI("noricStatsGuide")
          ),
          shiny::mainPanel(rapbase::statsUI("noricStats")))
      ),
      
      shiny::tabPanel(
        title = "Eksport",
        shiny::sidebarLayout(
          shiny::sidebarPanel(rapbase::exportUCInput("noricExport")),
          shiny::mainPanel(rapbase::exportGuideUI("noricExportGuide")))
      ), 
      
      
      shiny::tabPanel(
        title = "Staging data",
        shiny::titlePanel("Liste alle staging data"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(htmlOutput("stagingControl")),
          shiny::mainPanel(DT::dataTableOutput("stagingDataTable"))
        ),
        br(),
        shiny::titlePanel("Regelmessing etablering av staging data"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportOrgInput("noricBulletin"),
            rapbase::autoReportInput("noricBulletin")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("noricBulletin"))
        ))
    )
  )
)

