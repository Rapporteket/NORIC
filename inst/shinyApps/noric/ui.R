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
    )
  )
)

