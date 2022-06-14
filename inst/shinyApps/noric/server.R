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
    shiny::hideTab(inputId = "tabs", target = "Aktivitet")
    shiny::hideTab(inputId = "tabs", target = "Kodebok")
    shiny::hideTab(inputId = "tabs", target = "Nedlasting rapporter")
  } else if (userRole == "LC") {
    shiny::hideTab(inputId = "tabs", target = "Datadump")
    shiny::hideTab(inputId = "tabs", target = "Verktøy")
    shiny::hideTab(inputId = "tabs", target = "Nedlasting rapporter")
    shiny::hideTab(inputId = "tabs", target = "Aktivitet")
  }

  ## local reports/tabs for national registry
  if (isNationalReg(reshId)) {

    shiny::hideTab(inputId = "tabs", target = "Stentbruk")
    shiny::hideTab(inputId = "tabs", target = "Prosedyrer")
    shiny::hideTab(inputId = "tabs", target = "Aktivitet")
    shiny::hideTab(inputId = "tabs", target = "Månedsrapporter")
    shiny::hideTab(inputId = "tabs", target = "Abonnement")
  }

  ## dispatchment and use stats when not national registry
  if (!isNationalReg(reshId)) {
    shiny::hideTab(inputId = "tabs", target = "Utsending")
    shiny::hideTab(inputId = "tabs", target = "Bruksstatistikk")
    shiny::hideTab(inputId = "tabs", target = "Nedlasting rapporter")
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
  contentFile <- function(file, srcFile, tmpFile, type, useReportProcessor = FALSE, orgId = reshId, orgName = hospitalName) {
    src <- normalizePath(system.file(srcFile, package = "noric"))
    # temporarily switch to the temp dir, in case we do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, tmpFile, overwrite = TRUE)    
    
    if(!useReportProcessor){
      out <- rmarkdown::render(
        tmpFile, 
        output_format = switch(
          type,
          PDF = rmarkdown::pdf_document(),
          HTML = rmarkdown::html_document(),
          BEAMER = rmarkdown::beamer_presentation(theme = "Hannover"),
          REVEAL = revealjs::revealjs_presentation(theme = "sky")),
        
        params = list(
          tableFormat = switch(
            type,
            PDF = "latex",
            HTML = "html",
            BEAMER = "latex",
            REVEAL = "html"),
          hospitalName = hospitalName,
          author = author,
          reshId = reshId,
          registryName = registryName), 
        output_dir = tempdir())  
    } 
    
    
    if(useReportProcessor){
      withProgress(message = 'Rendering, please wait!', {
      out <- noric::reportProcessor(
        report = sub(pattern  = ".Rmd", 
                     replacement =  "",
                     x = srcFile),
        outputType = "pdf", 
        title = "unknown title", 
        author = "unknown author", 
        orgName = orgName, 
        orgId = orgId,
        registryName = registryName,
        userFullName = userFullName, 
        userRole = userRole, 
        userOperator = "unknown operator", 
        rendered_by_shiny = TRUE) })
    }
    
    file.rename(out, file)
  }

  contentDump <- function(file, type) {
    d <- noric::getDataDump(registryName = registryName,
                            tableName = input$dumpDataSet,
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
                           singleRow = FALSE,
                           session = session,
                           userRole = userRole)
  })

  metaDat <- reactive({
    noric::getPivotDataSet(setId = input$selectedDataSet,
                           registryName = registryName,
                           singleRow = TRUE,
                           session = session,
                           userRole = userRole)
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


  # KODEBOK
  kodebok <- noric::getKodebokMedUtledetedVar()
  metaDatKb <- shiny::reactive({
    noric::getPivotDataSet(setId = input$kbdTab,
                           registryName = registryName,
                           session = session,
                           userRole = userRole,
                           singleRow = TRUE)
  })

  ## innhold kontrollpanel:
  output$kbControl <- renderUI({
    selectInput(inputId = "kbdTab",
                label = "Vis kodebok for tabellen:",
                choices =  dataSets)
  })

  # vektor med alle variabelnavn i valgt tabell
  selectedkbTabVars <- reactive({
    if (input$kbdTab %in% c("ApLight", "AnP", "AnD",
                            "AP", "AK", "AKOppf", "CT", "FO",
                            "MK", "PS", "SO", "SS")) {
      metaDatKb() %>% names()
    }
    else {
      data.frame()
    }
  })

  output$kbdTable <- DT::renderDataTable(
    # kodebok noric, Kun variabelnavn som finnes den valgte tabellen
    kodebok[kodebok$fysisk_feltnavn %in% selectedkbTabVars(), ],
    options = list(
      lengthMenu = c(25, 50, 100, 200, 400),
      language = list(
        lengthMenu = "Vis _MENU_ rader per side",
        search = "S\u00f8k:",
        info = "Rad _START_ til _END_ av totalt _TOTAL_",
        paginate = list(previous = "Forrige", `next` = "Neste")
      )
    )
  )

  output$kbdData <- renderUI({
    DT::dataTableOutput("kbdTable")
  })



  # Samlerapporter
  output$stentbruk <- renderUI({
    htmlRenderRmd("NORIC_local_monthly_stent.Rmd")
  })

  output$prosedyrer <- renderUI({
    htmlRenderRmd("NORIC_local_monthly.Rmd")
  })


  output$aktivitet <- renderUI({
    htmlRenderRmd("NORIC_local_monthly_activity.Rmd")

  })

  output$downloadReportStentbruk <- downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly_stent",
                       input$formatStentbruk)
    },

    content = function(file) {
      contentFile(file, "NORIC_local_monthly_stent.Rmd",
                  basename(tempfile(fileext = ".Rmd")),
                  input$formatStentbruk, 
                  useReportProcessor = FALSE)
    }
  )

  output$downloadReportProsedyrer <- downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly", input$formatProsedyrer)
    },

    content = function(file) {
      contentFile(file, "NORIC_local_monthly.Rmd",
                  basename(tempfile(fileext = ".Rmd")),
                  input$formatProsedyrer, 
                  useReportProcessor = FALSE)
    }
  )

  output$downloadReportAktivitet <- downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly_activity", input$formatAktivitet)
    },

    content = function(file) {
      contentFile(file, "NORIC_local_monthly_activity.Rmd",
                  basename(tempfile(fileext = ".Rmd")),
                  useReportProcessor = FALSE, 
                  type = input$formatAktivitet)
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

  if(!isNationalReg(reshId) & userRole == "SC"){
    liste_aktivitet <- list(
      Aktivitet = list(
        synopsis = "M\u00E5nedlig oppsummering av aktiviteter siste \u00E5r",
        fun = "subscriptionLocalMonthlyReps",
        paramNames = pn,
        paramValues = c("NORIC_local_monthly_activity", pv)
      )
    )

    subReports <- c(subReports, liste_aktivitet)
  }


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
      fun = "reportProcessor",
      paramNames = c("report",
                     "outputType",
                     "title",
                     "author",
                     "orgName",
                     "orgId",
                     "registryName",
                     "userFullName",
                     "userRole",
                     "userOperator"),
      paramValues = c("NORIC_kvalitetsindikator",
                      "pdf",
                      "Månedsresultater",
                      "unknown author",
                      "unknown organization",
                      999999,
                      registryName,
                      userFullName,
                      userRole,
                      "unknown operator")
    )
  )

  org <- rapbase::autoReportOrgServer("noricDispatch", orgs)

  dispatchParamNames <- shiny::reactive(
    c("orgName", "orgId")
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

  # Download reports
  # Tabell med sykehusnavn - orgID
  orgs_df <- noric::mapOrgReshId(registryName = registryName,
                                 asNamedList = FALSE)
  
  ## innhold kontrollpanel:
  output$dwnldControl <- renderUI({
    selectInput(inputId = "dwldSykehus",
                label = "Velg sykehus for ki-rapporten:",
                choices = orgs)
  })
  
  output$dwldInfo <- renderUI({
    p(paste("Valgt for nedlasting:",
            orgs_df[orgs_df$id == input$dwldSykehus, "name"]))
  })
  
  output$dwnldReport <- shiny::downloadHandler(
    filename = function() {
      downloadFilename(fileBaseName = "NORIC_kvalitetsindikator",
                       type = "PDF")
    },
    
    
    content = function(file) {
      contentFile(file, 
                  srcFile = "NORIC_kvalitetsindikator.Rmd",
                  tmpFile = basename(tempfile(fileext = ".Rmd")),
                  type = "PDF", 
                  orgId = input$dwldSykehus, 
                  orgName = orgs_df[orgs_df$id == input$dwldSykehus, "name"],
                  useReportProcessor = TRUE)
    }
  )

  # Use stats
  rapbase::statsServer(
    "noricStats",
    registryName = "noric",
    eligible = all(c(userRole == "SC", isNationalReg(reshId)))
  )
  rapbase::statsGuideServer("noricStatsGuide", registryName = registryName)

  # Export
  rapbase::exportUCServer("noricExport", registryName = registryName,
                          repoName = "noric", eligible = (userRole == "SC"))
  rapbase::exportGuideServer("noricExportGuide", registryName = registryName)
})
