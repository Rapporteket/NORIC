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

