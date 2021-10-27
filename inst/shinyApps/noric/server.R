library(magrittr)
library(noric)
library(readr)
library(rpivotTable)
library(shiny)

shinyServer(function(input, output, session) {

  rapbase::appLogger(session = session, msg = "Starting NORIC application")

  # Parameters that will remain throughout the session
  ## setting values that do depend on a Rapporteket context
  if (rapbase::isRapContext()) {
    reshId <- rapbase::getUserReshId(session)
    hospitalName <- noric::getHospitalName(reshId)
    userFullName <- rapbase::getUserFullName(session)
    userRole <- rapbase::getUserRole(session)
    registryName <- noric::makeRegistryName("noricStaging", reshId)
    mapOrgId <- mapOrgReshId(registryName)
    author <- paste0(userFullName, "/", "Rapporteket")
  } else {
    ### if need be, define your (local) values here
  }

  # Hide tabs
  ## when role is 'LU' or some tabs for role 'LC'
  if (userRole == "LU") {
    shiny::hideTab(inputId = "tabs", target = "Utforsker")
    shiny::hideTab(inputId = "tabs", target = "Datadump")
    shiny::hideTab(inputId = "tabs", target = "Verktøy")
  } else if (userRole == "LC") {
    shiny::hideTab(inputId = "tabs", target = "Datadump")
    shiny::hideTab(inputId = "tabs", target = "Verktøy")
  }

  ## 'Prosedyrer2', regardless
  shiny::hideTab(inputId = "tabs", target = "Prosedyrer2")

  ## local reports/tabs for national registry
  if (isNationalReg(reshId)) {
    shiny::hideTab(inputId = "tabs", target = "Stentbruk")
    shiny::hideTab(inputId = "tabs", target = "Prosedyrer")
  }

  ## dispatchment when not national registry
  if (!isNationalReg(reshId)) {
    shiny::hideTab(inputId = "tabs", target = "Eksport")
  }

  # html rendering function for re-use
  htmlRenderRmd <- function(srcFile) {
    # set param needed for report meta processing
    params <- list(author = author,
                   hospitalName = hospitalName,
                   tableFormat = "html",
                   reshId = reshId,
                   registryName = registryName)
    # do all kniting and rendering from temporary directory/file
    sourceFile <- tempfile(fileext = ".Rmd")
    file.copy(system.file(srcFile, package = "noric"), sourceFile,
              overwrite = TRUE)
    owd <- setwd(dirname(sourceFile))
    on.exit(setwd(owd))
    sourceFile %>%
      knitr::knit() %>%
      rmarkdown::render(., output_format = "html_fragment") %>%
      readLines() %>%
      shiny::HTML()
  }


  # filename function for re-use
  downloadFilename <- function(fileBaseName, type) {
    paste(paste0(fileBaseName,
                 as.character(as.integer(as.POSIXct(Sys.time())))),
          sep = ".", switch(
            type,
            PDF = "pdf", HTML = "html", REVEAL = "html", BEAMER = "pdf")
    )
  }

  # render file function for re-use
  contentFile <- function(file, srcFile, tmpFile, type) {
    src <- normalizePath(system.file(srcFile, package = "noric"))
    # temporarily switch to the temp dir, in case we do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFile, overwrite = TRUE)

    out <- rmarkdown::render(tmpFile, output_format = switch(
      type,
      PDF = rmarkdown::pdf_document(),
      HTML = rmarkdown::html_document(),
      BEAMER = rmarkdown::beamer_presentation(theme = "Hannover"),
      REVEAL = revealjs::revealjs_presentation(theme = "sky")
        #css = normalizePath(system.file("bootstrap.css", package = "noric")))
    ), params = list(tableFormat = switch(
      type,
      PDF = "latex",
      HTML = "html",
      BEAMER = "latex",
      REVEAL = "html"),
      hospitalName = hospitalName,
      author = author,
      reshId = reshId,
      registryName = registryName
    ), output_dir = tempdir())
    file.rename(out, file)
  }

  contentDump <- function(file, type) {
    d <- noric::getDataDump(registryName, input$dumpDataSet,
                            fromDate = input$dumpDateRange[1],
                            toDate = input$dumpDateRange[2],
                            session = session)
    if (type == "xlsx-csv") {
      readr::write_excel_csv2(d, file)
    } else {
      readr::write_csv2(d, file)
    }
  }

  # widget
  output$appUserName <- renderText(userFullName)
  output$appOrgName <- renderText(paste(hospitalName, userRole, sep = ", "))

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session, callerPkg = "noric")
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })

  # Start
  output$veiledning <- renderUI({
    htmlRenderRmd("veiledning.Rmd")
  })

  # Utforsker
  ## Data sets available
  if (userRole == "SC") {
    dataSets <- list(`Bruk og valg av data...` = "info",
                     `Angio PCI med utledete variabler, 3 siste år` = "ApLight",
                     `Andre prosedyrer` = "AnP",
                     `Annen diagnostikk` = "AnD",
                     `Angio PCI` = "AP",
                     `Aortaklaff` = "AK",
                     `Aortaklaff oppfølging` = "AKOppf",
                     `CT Angio` = "CT",
                     `Forløpsoversikt` = "FO",
                     `Mitralklaff` = "MK",
                     `PasientStudier` = "PS",
                     `Skjemaoversikt` = "SO",
                     `Segment stent` = "SS"
                     )
  } else {
    dataSets <- list(`Bruk og valg av data...` = "info",
                     `Angio PCI med utledete variabler, 3 siste år` = "ApLight",
                     `Andre prosedyrer` = "AnP",
                     `Annen diagnostikk` = "AnD",
                     `Angio PCI` = "AP",
                     `Aortaklaff` = "AK",
                     `CT Angio` = "CT",
                     `Forløpsoversikt` = "FO",
                     `Skjemaoversikt` = "SO",
                     `Segment stent` = "SS"
    )
  }


  ## reactive vals
  rvals <- reactiveValues()
  rvals$showPivotTable <- FALSE
  rvals$togglePivotingText <- "Last valgte data!"
  rvals$selectedDataSet <- "info"
  rvals$selectedVars <- ""

  ## observers
  observeEvent(input$togglePivoting, {
    if (rvals$showPivotTable) {
      rvals$showPivotTable <- FALSE
      rvals$togglePivotingText <- "Last valgte data!"
      # persist last choice
      rvals$selectedDataSet <- input$selectedDataSet
      rvals$selectedVars <- input$selectedVars
    } else {
      rvals$showPivotTable <- TRUE
      rvals$togglePivotingText <- "Endre valg av data!"
    }
  })

  observeEvent(input$selectedDataSet, {
    rvals$selectedVars <- ""
  })

  dat <- reactive({
    noric::getPivotDataSet(setId = input$selectedDataSet,
                           registryName = registryName,
                           session = session, userRole = userRole)
  })

  metaDat <- reactive({
    noric::getPivotDataSet(setId = input$selectedDataSet,
                           registryName = registryName,
                           singleRow = TRUE,
                           session = session, userRole = userRole)
  })

  ## outputs
  output$selectDataSet <- renderUI({
    if (rvals$showPivotTable) {
      NULL
    } else {
      tagList(
        selectInput(inputId = "selectedDataSet", label = "Velg datasett:",
                    choices = dataSets, selected = rvals$selectedDataSet),
        checkboxInput("isSelectAllVars", "Velg alle variabler")
      )
    }
  })

  output$selectVars <- renderUI({
    req(input$selectedDataSet)
    if (length(rvals$showPivotTable) == 0 | rvals$showPivotTable) {
      h4(paste("Valgt datasett:",
               names(dataSets)[dataSets == input$selectedDataSet]))
    } else {
      if (input$isSelectAllVars) {
        vars <- names(metaDat())
      } else {
        vars <- rvals$selectedVars
      }
      selectInput(inputId = "selectedVars", label = "Velg variabler:",
                  choices = names(metaDat()), multiple = TRUE,
                  selected = vars)
    }
  })

  output$togglePivotSurvey <- renderUI({
    if (length(input$selectedVars) == 0) {
      NULL
    } else {
      actionButton(inputId = "togglePivoting",
                   label = rvals$togglePivotingText)
    }
  })

  output$pivotSurvey <- renderRpivotTable({
    if (rvals$showPivotTable) {
      rpivotTable(dat()[input$selectedVars])
    } else {
      rpivotTable(data.frame())
    }
  })


  # Samlerapporter
  output$stentbruk <- renderUI({
    htmlRenderRmd("NORIC_local_monthly_stent.Rmd")
  })

  output$prosedyrer <- renderUI({
    htmlRenderRmd("NORIC_local_monthly.Rmd")
  })

  # render of report-to-be
  output$prosedyrer2 <- renderUI({
    htmlRenderRmd("NORIC_local_monthly.Rmd")
  })

  output$downloadReportStentbruk <- downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly_stent",
                                input$formatStentbruk)
    },

    content = function(file) {
      contentFile(file, "NORIC_local_monthly_stent.Rmd",
                  basename(tempfile(fileext = ".Rmd")),
                  input$formatStentbruk)
    }
  )

  output$downloadReportProsedyrer <- downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly", input$formatProsedyrer)
    },

    content = function(file) {
      contentFile(file, "NORIC_local_monthly.Rmd",
                  basename(tempfile(fileext = ".Rmd")),
                  input$formatProsedyrer)
    }
  )

  # download of report-to-be
  output$downloadReportProsedyrer2 <- downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly", input$formatProsedyrer2)
    },

    content = function(file) {
      contentFile(file, "NORIC_local_monthly.Rmd",
                  basename(tempfile(fileext = ".Rmd")),
                  input$formatProsedyrer2)
    }
  )


  # Datadump
  output$dataDumpInfo <- renderUI({
    p(paste("Valgt for nedlasting:", input$dumpDataSet))
  })

  output$dumpDownload <- downloadHandler(
    filename = function() {
      basename(tempfile(pattern = input$dumpDataSet,
                        fileext = ".csv"))
    },
    content = function(file) {
      contentDump(file, input$dumpFormat)
    }
  )


  # Metadata
  meta <- reactive({
    noric::describeRegistryDb(registryName)
  })

  output$metaControl <- renderUI({
    tabs <- names(meta())
    selectInput("metaTab", "Velg tabell:", tabs)
  })

  output$metaDataTable <- DT::renderDataTable(
    meta()[[input$metaTab]], rownames = FALSE,
    options = list(
      lengthMenu = c(25, 50, 100, 200, 400),
      language = list(
        lengthMenu = "Vis _MENU_ rader per side",
        search = "S\u00f8k:",
        info = "Rad _START_ til _END_ av totalt _TOTAL_",
        paginate = list(previous = "Forrige", `next` = "Neste")
      ))
  )

  output$metaData <- renderUI({
    DT::dataTableOutput("metaDataTable")
  })

  # Abonnement
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  subscription <- reactiveValues(
    tab = rapbase::makeAutoReportTab(session, mapOrgId = mapOrgId))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    subscription$tab, server = FALSE, escape = FALSE, selection = "none",
    rownames = FALSE,
    options = list(
      dom = "t",
      ordering = FALSE,
      language = list(
        lengthMenu = "Vis _MENU_ rader per side",
        search = "S\u00f8k:",
        info = "Rad _START_ til _END_ av totalt _TOTAL_",
        paginate = list(previous = "Forrige", `next` = "Neste")
      )
    )
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    fullName <- rapbase::getUserFullName(session)
    if (length(subscription$tab) == 0) {
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

  ### lag liste over mulige valg styrt av lokal eller nasjonal sesjon
  output$subscriptionRepList <- renderUI({
    if (isNationalReg(reshId)) {
      selectInput("subscriptionRep", "Rapport:",
                  c(""))
    } else {
      selectInput("subscriptionRep", "Rapport:",
                  c("Stentbruk, månedlig", "Prosedyrer, månedlig"))
    }
  })

  ### aktiver abonnement, men kun når et aktuelt valg er gjort
  observeEvent(input$subscribe, {
    package <- "noric"
    owner <- rapbase::getUserName(session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    organization <- rapbase::getUserReshId(session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval
    )
    email <- rapbase::getUserEmail(session)
    if (input$subscriptionRep == "Prosedyrer, månedlig") {
      synopsis <- "NORIC/Rapporteket: prosedyrer, månedlig"
      baseName <- "NORIC_local_monthly"
    }
    if (input$subscriptionRep == "Stentbruk, månedlig") {
      synopsis <- "NORIC/Rapporteket: stentbruk, månedlig"
      baseName <- "NORIC_local_monthly_stent"
    }

    if (nchar(input$subscriptionRep) > 0) {
      fun <- "subscriptionLocalMonthlyReps"
      paramNames <- c("baseName", "reshId", "registryName", "author",
                      "hospitalName", "type")
      paramValues <- c(baseName, reshId, registryName, author, hospitalName,
                       input$subscriptionFileFormat)
      rapbase::createAutoReport(synopsis = synopsis, package = package,
                                fun = fun, paramNames = paramNames,
                                paramValues = paramValues, owner = owner,
                                email = email, organization = organization,
                                runDayOfYear = runDayOfYear,
                                interval = interval,
                                intervalName = intervalName)
    }
    subscription$tab <-
      rapbase::makeAutoReportTab(session, mapOrgId = mapOrgId)
  })
  
  # List of org name(s) and number(s) for both subscription and dispatchments
  orgs <- noric::mapOrgReshId(registryName, asNamedList = TRUE)
  
  # Ny abonnement kode (med moduler fra rapbase)
  ## currently, function parameters are the same for all reports
  pn <- c("baseName", "reshId", "registryName", "author", "hospitalName",
          "type")
  pv <- c(reshId, registryName, author, hospitalName, "pdf")
  
  subReports <- list(
    Prosedyrer = list(
      synopsis = "M\u00E5nedlig oppsummering av prosedyrer siste \u00E5r",
      fun = "subscriptionLocalMonthlyReps",
      paramNames = pn,
      paramValues = c("NORIC_local_monthly", pv)
    ),
    Stentbruk = list(
      synopsis = "M\u00E5nedlig oppsummering av stentbruk siste \u00E5r",
      fun = "subscriptionLocalMonthlyReps",
      paramNames = pn,
      paramValues = c("NORIC_local_monthly_stent", pv)
    )
  )
  
  ## serve subscriptions
  rapbase::autoReportServer(
    "noricSubscription", registryName = "noric", type = "subscription",
    reports = subReports, orgs = orgs
  )

  
  # Ny Utsending (ved rapbase)
  dispatch <- list(
    `KI: sykehus mot resten av landet` = list(
      synopsis = paste("NORIC kvalitetsindikatorer: eget sykehus",
                        "sammenlignet med resten av landet"),
      fun = "dispatchMonthlyKi",
      paramNames = c("baseName", "hospitalName", "reshID", "author",
                     "userRole", "type", "registryName"),
      paramValues = c(
        "NORIC_kvalitetsindikator", "udefName", 999999, userFullName,
        userRole, "pdf", registryName
      )
    )
  )
  
  org <- rapbase::autoReportOrgServer("noricDispatch", orgs)
  
  dispatchParamNames <- shiny::reactive(
    c("hospitalName", "reshID")
  )
  dispatchParamValues <- shiny::reactive(
    c(org$name(), org$value())
  )
  
  rapbase::autoReportServer(
    "noricDispatch", registryName = "noric", type = "dispatchment",
    org = org$value, paramNames = dispatchParamNames,
    paramValues = dispatchParamValues, reports = dispatch, orgs = orgs,
    eligible = all(c(userRole == "SC", isNationalReg(reshId)))
  )
  
  

  # Rediger eksisterende auto rapport (alle typer)
  shiny::observeEvent(input$edit_button, {
    repId <- strsplit(input$edit_button, "_")[[1]][2]
    rep <- rapbase::readAutoReportData()[[repId]]
    if (rep$type == "subscription") {

    }
    if (rep$type == "dispatchment") {
      dispatchment$freq <- paste0(rep$intervalName, "-", rep$interval)
      dispatchment$email <- rep$email
      rapbase::deleteAutoReport(repId)
      dispatchment$tab <-
        rapbase::makeAutoReportTab(session, type = "dispatchment",
                                   mapOrgId = mapOrgId)
      dispatchment$report <- rep$synopsis
    }
    if (rep$type == "bulletin") {

    }
  })

  # Slett eksisterende auto rapport (alle typer)
  shiny::observeEvent(input$del_button, {
    repId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(repId)
    subscription$tab <-
      rapbase::makeAutoReportTab(session, type = "subscription",
                                 mapOrgId = mapOrgId)
    dispatchment$tab <-
      rapbase::makeAutoReportTab(session, type = "dispatchment",
                                 mapOrgId = mapOrgId)
  })
  
  # Use stats
  rapbase::statsServer("noricStats", registryName = "noric",
                       eligible = all(userRole == "SC"))
  rapbase::statsGuideServer("noricStatsGuide", registryName = registryName)
  
  # Export
  rapbase::exportUCServer("noricExport", registryName = registryName,
                          repoName = "noric", eligible = (userRole == "SC"))
  rapbase::exportGuideServer("noricExportGuide", registryName = registryName)
})
