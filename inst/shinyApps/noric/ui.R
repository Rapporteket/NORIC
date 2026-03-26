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
      shiny::uiOutput("startTab")
    ),
    shiny::tabPanel(
      title = "Utforsker",
      shiny::uiOutput("utforskerTab")
    ),


    shiny::tabPanel(
      title = "Kodebok",
      shiny::uiOutput("kodebokTab")
    ),


    shiny::navbarMenu(
      title = "Månedsrapporter",

      shiny::tabPanel(
        title = "Invasive prosedyrer",
        shiny::uiOutput("prosedyrerReport")
      ),

      shiny::tabPanel(
        title = "Angiografør/Operatør",
        shiny::uiOutput("angioReport")
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
      shiny::uiOutput("datadumpTab")
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
        shiny::uiOutput("metadataTab")
      ),
      shiny::tabPanel(
        title = "Utsending",
        shiny::uiOutput("dispatchTab")
      ),
      shiny::tabPanel(
        title = "Nedlasting rapporter",
        shiny::uiOutput("dwnldReportTab")
      ),
      shiny::tabPanel(
        title = "Bruksstatistikk",
        shiny::uiOutput("statsTab")
      ),
      shiny::tabPanel(
        title = "Eksport",
        shiny::uiOutput("exportTab")
      ),
      shiny::tabPanel(
        title = "Staging data",
        shiny::uiOutput("stagingTab")
      )
    )
  )
)

