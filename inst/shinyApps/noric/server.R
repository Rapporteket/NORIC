#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(rpivotTable)
library(shiny)
library(magrittr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Various calls for session data from rapbase and systemn settings
  output$callUser <- renderText({
    paste("rapbase::getUserName(session):",
          rapbase::getUserName(session))
  })
  output$callGroups <- renderText({
    paste("rapbase::getUserGroups(session):",
          rapbase::getUserGroups(session))
  })
  output$callReshId <- renderText({
    paste("rapbase::getUserReshId(session):",
          rapbase::getUserReshId(session))
  })
  output$callRole <- renderText({
    paste("rapbase::getUserRole(session):",
          rapbase::getUserRole(session))
  })

  output$callEmail <- renderText({
    paste("rapbase::getUserEmail(session):",
          rapbase::getUserEmail(session))
  })
  
  output$callFullName <- renderText({
    paste("rapbase::getUserFullName(session):",
          rapbase::getUserFullName(session))
  })
  
  output$callPhone <- renderText({
    paste("rapbase::getUserPhone(session):",
          rapbase::getUserPhone(session))
  })
  
  output$envInstance <- renderText({
    Sys.getenv("R_RAP_INSTANCE")
  })
  
  output$envConfigPath <- renderText({
    Sys.getenv("R_RAP_CONFIG_PATH")
  })
  
  output$locale <- renderText({
    Sys.getlocale()
  })
  
  # Parameters that will remain throughout the session
  ## values that do depend on a Rapporteket context
  if (rapbase::isRapContext()) {
    reshId <- rapbase::getUserReshId(session)
    hospitalName <- noric::getHospitalName(reshId)
    registryName <- noric::NORICmakeRegistryName("noricStaging", reshId)
  } else {
    ### if need be, define your (local) values here
  }
  
    
  ## other values
  
  # html rendering function for re-use
  htmlRenderRmd <- function(srcFile) {
    # set param needed for report meta processing
    params <- list(tableFormat="html")
    system.file(srcFile, package="noric") %>% 
      knitr::knit() %>% 
      markdown::markdownToHTML(.,
                               options = c('fragment_only',
                                           'base64_images')) %>% 
      shiny::HTML()
  }
  
  
  # filename function for re-use
  downloadFilename <- function(fileBaseName, type) {
    paste(paste0(fileBaseName,
                 as.character(as.integer(as.POSIXct(Sys.time())))),
          sep = '.', switch(
            type, 
            PDF = 'pdf', HTML = 'html', REVEAL = 'html', BEAMER = 'pdf')
    )
  }
  
  # render file function for re-use
  contentFile <- function(file, srcFile, tmpFile, type) {
    src <- normalizePath(system.file(srcFile, package="noric"))
    hospitalName <- rapbase::getUserReshId(session) %>% 
      noric::getHospitalName()
    
    # temporarily switch to the temp dir, in case we do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFile, overwrite = TRUE)
    
    library(rmarkdown)
    out <- render(tmpFile, output_format = switch(
      type,
      PDF = pdf_document(),
      HTML = html_document(),
      BEAMER = beamer_presentation(theme = "Hannover"),
      REVEAL = revealjs::revealjs_presentation(theme = "sky")
        #css = normalizePath(system.file("bootstrap.css", package = "noric")))
    ), params = list(tableFormat=switch(
      type,
      PDF = "latex",
      HTML = "html",
      BEAMER = "latex",
      REVEAL = "html"), hospitalName=hospitalName
    ), output_dir = tempdir())
    file.rename(out, file)
  }
  
  # widget
  output$appUserName <- renderText(getUserFullName(session))
  output$appOrgName <- renderText(getUserReshId(session))
  
  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = "Den er grei!")
  })
  
  output$tabAnP <- renderRpivotTable({
    AnP <- noric::getLocalAnPData(registryName)
    rpivotTable(AnP, rows = c("Year", "Month"), cols = c("AnnenProsType"),
                rendererName = c("Heatmap"), width="100%", height="400px")
  })
  
  output$tabAP <- renderRpivotTable({
    AP <- noric::getLocalAPData(registryName)
    rpivotTable(AP, rows = c("Year", "Month"), cols = c("ProsedyreType"),
                rendererName = c("Heatmap"), width = "100%", height = "400px")
  })
  
  output$tabSO <- renderRpivotTable({
    SO <- noric::getLocalSOData(registryName)
    rpivotTable(SO, rows = c("Year", "Skjemanavn"), cols = c("OpprettetAv"),
                rendererName = c("Heatmap"), width = "100%", height = "400px")
  })
  
  output$stentbruk <- renderUI({
    htmlRenderRmd("NORIC_local_monthly_stent.Rmd")
  })
  
  output$prosedyrer <- renderUI({
    htmlRenderRmd("NORIC_local_monthly.Rmd")
  })
  
  output$downloadReportStentbruk <- downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly_stent",
                                input$formatStentbruk)
    },

    content = function(file) {
      contentFile(file, "NORIC_local_monthly_stent.Rmd", "tmpNoricStent.Rmd",
                  input$formatStentbruk)
    }
  )
  
  output$downloadReportProsedyrer <- downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly", input$formatProsedyrer)
    },
    
    content = function(file) {
      contentFile(file, "NORIC_local_monthly.Rmd", "tmpNoric.Rmd",
                  input$formatProsedyrer)
    }
  )
  
  # Abonnement
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(session))
  
  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    rownames = FALSE, options = list(dom = 't')
  )
  
  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    fullName <- rapbase::getUserFullName(session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", fullName))
    } else {
      tagList(
        p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
                rapbase::getUserEmail(session), ":")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })
  
  ## nye abonnement
  observeEvent (input$subscribe, {
    package <- "noric"
    owner <- rapbase::getUserName(session)
    organization <- rapbase::getUserReshId(session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = input$subscriptionFreq
    )
    email <- rapbase::getUserEmail(session)
    if (input$subscriptionRep == "Samlerapport1") {
      synopsis <- "Automatisk samlerapport1"
      fun <- "samlerapport1Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("Alder", 1)
      
    }
    if (input$subscriptionRep == "Samlerapport2") {
      synopsis <- "Automatisk samlerapport2"
      fun <- "samlerapport2Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("BMI", 2)
    }
    rapbase::createAutoReport(synopsis = synopsis, package = package,
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })
  
  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })
})
