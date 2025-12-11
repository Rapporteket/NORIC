library(magrittr)
library(noric)
library(readr)
library(rpivotTable)
library(shiny)

shinyServer(function(input, output, session) {
  
  rapbase::appLogger(session = session, msg = "Starting NORIC application")
  registryName <- "noric_bergen"

  map_orgname <- noric::mapOrgReshId(registryName = registryName, 
                                     asNamedList = FALSE) %>% 
    dplyr::transmute(AvdRESH = id) %>% 
    noric::fikse_sykehusnavn(.) %>% 
    rbind(data.frame(Sykehusnavn = "Nasjonal", AvdRESH = 0)) %>% 
    dplyr::rename(UnitId = AvdRESH,
                  orgname = Sykehusnavn)
  
  user <- rapbase::navbarWidgetServer2(
    id = "navbar-widget",
    orgName = "noric",
    caller = "noric",
    map_orgname = shiny::req(map_orgname)
  )
  
  # Parameters that may change depending on the role and org of user
  userFullName <- Sys.getenv("FALK_USER_FULLNAME")
  hospitalName <- shiny::reactive(
    map_orgname$orgname[map_orgname$UnitId == user$org()]
    )
  
  # Hide tabs
  ## when role is 'LU' or some tabs for role 'LC'
  shiny::observeEvent(list(user$role(), user$org()), {
    shiny::showTab(inputId = "tabs", target = "Utforsker")
    shiny::showTab(inputId = "tabs", target = "Datadump")
    shiny::showTab(inputId = "tabs", target = "Verktøy")
    shiny::showTab(inputId = "tabs", target = "Angiografør/Operatør")
    shiny::showTab(inputId = "tabs", target = "Kodebok")
    shiny::showTab(inputId = "tabs", target = "Nedlasting rapporter")
    shiny::showTab(inputId = "tabs", target = "Prosedyrer")
    shiny::showTab(inputId = "tabs", target = "Månedsrapporter")
    shiny::showTab(inputId = "tabs", target = "Abonnement")
    shiny::showTab(inputId = "tabs", target = "Utsending")
    shiny::showTab(inputId = "tabs", target = "Bruksstatistikk")
    shiny::showTab(inputId = "tabs", target = "Aortaklaff")
    
    if (shiny::req(user$role()) == "LU") {
      shiny::hideTab(inputId = "tabs", target = "Utforsker")
      shiny::hideTab(inputId = "tabs", target = "Datadump")
      shiny::hideTab(inputId = "tabs", target = "Verktøy")
      shiny::hideTab(inputId = "tabs", target = "Angiografør/Operatør")
      shiny::hideTab(inputId = "tabs", target = "Kodebok")
      shiny::hideTab(inputId = "tabs", target = "Nedlasting rapporter")
    } else if (shiny::req(user$role()) == "LC") {
      shiny::hideTab(inputId = "tabs", target = "Datadump")
      shiny::hideTab(inputId = "tabs", target = "Verktøy")
      shiny::hideTab(inputId = "tabs", target = "Nedlasting rapporter")
      shiny::hideTab(inputId = "tabs", target = "Angiografør/Operatør")
    }
    
    if (shiny::req(user$org()) == 0) {
      shiny::hideTab(inputId = "tabs", target = "Prosedyrer")
      shiny::hideTab(inputId = "tabs", target = "Angiografør/Operatør")
      shiny::hideTab(inputId = "tabs", target = "Månedsrapporter")
      shiny::hideTab(inputId = "tabs", target = "Abonnement")
    }
    
    ## dispatchment and use stats hidden when not national registry
    if (shiny::req(user$org()) != 0) {
      shiny::hideTab(inputId = "tabs", target = "Utsending")
      shiny::hideTab(inputId = "tabs", target = "Bruksstatistikk")
      shiny::hideTab(inputId = "tabs", target = "Nedlasting rapporter")
      shiny::hideTab(inputId = "tabs", target = "Eksport")
      shiny::hideTab(inputId = "tabs", target = "Staging data")
    }
    
    if(shiny::req(user$org()) %in% c(108141, 4210141, 114150, 105502, 106944)){
      shiny::hideTab(inputId = "tabs", target = "Aortaklaff")
    }
  })
  
  
  
  # filename function for re-use
  downloadFilename <- function(fileBaseName) {
    paste0(fileBaseName,
           as.character(as.integer(as.POSIXct(Sys.time()))), 
           ".pdf")
  }
  
  
  # render file function for re-use
  contentFile <- function(file, srcFile, tmpFile, type, tableFormat,
                          useReportProcessor = FALSE, orgId, orgName,
                          registryName, userFullName, userRole) {
    
    src <- normalizePath(system.file(srcFile, package = "noric"))
    file.copy(from = src, to = tmpFile, overwrite = TRUE)
    # temporarily switch to the temp dir, in case we do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    shiny::withProgress(message = 'Lager pdf, vent litt...', {
      out <- noric::reportProcessor(
        report = sub(pattern = ".Rmd", replacement = "", x = srcFile),
        outputType = type,
        title = "unknown title",
        author = "unknown author",
        orgName = orgName,
        orgId = orgId,
        registryName = registryName,
        userFullName = userFullName,
        userRole = userRole,
        rendered_by_shiny = TRUE,
        tableFormat = tableFormat)
    })
    file.rename(from = out, to = file)
  }
  
  # datadump function for re-use
  contentDump <- function(file, type) {
    d <- noric::getDataDump(registryName = registryName,
                            tableName = input$dumpDataSet,
                            fromDate = input$dumpDateRange[1],
                            toDate = input$dumpDateRange[2],
                            session = session, 
                            singleHospital = user$org())
    if (type == "xlsx-csv") {
      readr::write_excel_csv2(d, file)
    } else {
      readr::write_csv2(d, file)
    }
  }
  
  # WIDGET
  output$appUserName <- shiny::renderText(userFullName)
  output$appOrgName <- shiny::renderText(paste(hospitalName(),
                                               user$role(),
                                               sep = ", "))
  userInfo <- rapbase::howWeDealWithPersonalData(session, callerPkg = "noric")
  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert(title = "Dette vet Rapporteket om deg:", 
                           text = userInfo,
                           type = "", 
                           imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, 
                           closeOnClickOutside = TRUE,
                           html = TRUE, 
                           confirmButtonText = rapbase::noOptOutOk())
    })
  
  # START
  output$veiledning <- shiny::renderUI({
    rapbase::renderRmd(
      sourceFile = system.file("veiledning.Rmd", package = "noric"),
      outputType = "html_fragment",
      params = list(
        author = user$fullName(),
        hospitalName = hospitalName(),
        tableFormat = "html",
        reshId = user$org()
      ))
  })
  
  # UTFORSKER
  dataSets <- shiny::reactive({
    if (user$role() == "SC") {
      dataSets <- list(
        `Bruk og valg av data...` = "info",
        `Angio PCI med utledete variabler` = "ApLight",
        `Angio PCI` = "AP",
        `Andre prosedyrer` = "AnP",
        `Annen diagnostikk` = "AnD",
        `Aortaklaff` = "AK",
        `Aortaklaff eprom` = "TP",
        `Aortaklaff oppfølging` = "AKOppf",
        `Mitralklaff` = "MK",
        `CT Angio` = "CT",
        `Forløpsoversikt (ignorer kalender)` = "FO",
        `PasientStudier (ignorer kalender)` = "PS",
        `Skjemaoversikt` = "SO",
        `Segment stent` = "SS"
      )
      if (user$org() != 0) {
        dataSets <- within(dataSets, rm("Aortaklaff eprom"))
      }
      
    } else {
      dataSets <- list(
        `Bruk og valg av data...` = "info",
        `Angio PCI med utledete variabler` = "ApLight",
        `Angio PCI` = "AP",
        `Andre prosedyrer` = "AnP",
        `Annen diagnostikk` = "AnD",
        `Aortaklaff` = "AK",
        `Mitralklaff` = "MK",
        `CT Angio` = "CT",
        `Forløpsoversikt (ignorer kalender)` = "FO",
        `Skjemaoversikt` = "SO",
        `Segment stent` = "SS"
      )
    }
    return(dataSets)
  })
  
  ## reactive values for utforsker
  rvals <- reactiveValues()
  rvals$showPivotTable <- FALSE
  rvals$togglePivotingText <- "Last valgte data!"
  rvals$selectedDataSet <- "info"
  rvals$selectedVars <- ""
  rvals$utfDateStart <- as.Date(
    x = paste0("01-01-", 
               as.integer(format(
                 x = lubridate::ymd(Sys.Date()) - lubridate::years(3), 
                 format ="%Y"))), 
    format = "%d-%m-%Y")
  rvals$utfDateEnd <- Sys.Date()
  
  ## observers 
  shiny::observeEvent(input$togglePivoting, {
    if (rvals$showPivotTable) {
      rvals$showPivotTable <- FALSE
      rvals$togglePivotingText <- "Last valgte data!"
      # persist last choice
      rvals$selectedDataSet <- input$selectedDataSet
      rvals$selectedVars <- input$selectedVars
      rvals$utfDateStart <- input$utforskerDateRange[1]
      rvals$utfDateEnd <- input$utforskerDateRange[2]
    } else {
      rvals$showPivotTable <- TRUE
      rvals$togglePivotingText <- "Endre valg av data!"
    }
  })
  
  shiny::observeEvent(input$selectedDataSet, {
    rvals$selectedVars <- ""
  })
  
  dat <- shiny::reactive({
    noric::getPivotDataSet(setId = input$selectedDataSet,
                           registryName = registryName,
                           singleRow = FALSE,
                           session = session,
                           userRole = user$role(),
                           fromDate = input$utforskerDateRange[1],
                           toDate = input$utforskerDateRange[2], 
                           singleHospital = user$org())
  })
  
  metaDat <- shiny::reactive({
    noric::getPivotDataSet(setId = input$selectedDataSet,
                           registryName = registryName,
                           singleRow = TRUE,
                           session = session,
                           userRole = user$role(),
                           fromDate = NULL,
                           toDate = NULL, 
                           singleHospital = user$org())
  })
  
  ## outputs
  output$selectDataSet <- shiny::renderUI({
    if (rvals$showPivotTable) {
      NULL
    } else {
      htmltools::tagList(
        shiny::selectInput(inputId = "selectedDataSet",
                           label = "Velg datasett:",
                           choices = dataSets(),
                           selected = rvals$selectedDataSet),
        shiny::checkboxInput(inputId = "isSelectAllVars",
                             label = "Velg alle variabler")
      )
    }
  })
  
  output$utforskerDateRange <- shiny::renderUI({
    if (rvals$showPivotTable) {
      NULL
    } else {
      htmltools::tagList(
        shiny::dateRangeInput(
          inputId = "utforskerDateRange", 
          label = "Velg periode:",
          start = rvals$utfDateStart,
          end = rvals$utfDateEnd,
          min = as.Date("2013-01-01", format = "%Y-%m-%d"), 
          max = Sys.Date(),
          separator = "-",
          weekstart = 1)
      )}
  })
  
  
  output$selectVars <- shiny::renderUI({
    shiny::req(input$selectedDataSet, dataSets)
    if (length(rvals$showPivotTable) == 0 | rvals$showPivotTable) {
      h4(paste0("Valgt datasett: ",
                names(dataSets())[dataSets() == input$selectedDataSet], 
                " i perioden " , 
                input$utforskerDateRange[1],
                " : ", 
                input$utforskerDateRange[2]))
    } else {
      if (input$isSelectAllVars) {
        vars <- names(metaDat())
      } else {
        vars <- rvals$selectedVars
      }
      shiny::selectInput(inputId = "selectedVars", 
                         label = "Velg variabler:",
                         choices = names(metaDat()), 
                         multiple = TRUE,
                         selected = vars)
    }
  })
  
  output$togglePivotSurvey <- shiny::renderUI({
    if (length(input$selectedVars) == 0) {
      NULL
    } else {
      shiny::actionButton(inputId = "togglePivoting",
                          label = rvals$togglePivotingText)
    }
  })
  
  output$pivotSurvey <- rpivotTable::renderRpivotTable({
    if (rvals$showPivotTable) {
      rpivotTable::rpivotTable(dat()[input$selectedVars])
    } else {
      rpivotTable::rpivotTable(data.frame())
    }
  })
  
  
  # KODEBOK
  kodebok <- noric::getKodebokMedUtledetedVar()
  metaDatKb <- shiny::reactive({
    noric::getPivotDataSet(setId = input$kbdTab,
                           registryName = registryName,
                           session = session,
                           userRole = user$role(),
                           singleRow = TRUE, 
                           fromDate = NULL,
                           toDate = NULL, 
                           singleHospital = user$org())
  })
  
  ## innhold kontrollpanel:
  output$kbControl <- shiny::renderUI({
    shiny::selectInput(inputId = "kbdTab",
                       label = "Vis kodebok for tabellen:",
                       choices =  dataSets())
  })
  
  # vektor med alle variabelnavn i valgt tabell
  selectedkbTabVars <- shiny::reactive({
    if (input$kbdTab %in% c("ApLight", "AnP", "AnD",
                            "AP", "AK", "AKOppf", "CT", "FO",
                            "MK", "PS", "SO", "SS", "TP")) {
      metaDatKb() %>% names()
    }
    else {
      data.frame()
    }
  })
  
  output$kbdTable <- DT::renderDataTable(
    expr = kodebok[kodebok$fysisk_feltnavn %in% selectedkbTabVars(), ],
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
  
  output$kbdData <- shiny::renderUI({
    DT::dataTableOutput("kbdTable")
  })
  
  
  
  # SAMLERAPPORT
  output$prosedyrer <- shiny::renderUI({
    shiny::withProgress(message = 'Laster, dette kan ta litt tid...', {
        rapbase::renderRmd(
          sourceFile = system.file("NORIC_local_monthly.Rmd", 
                                   package = "noric"),
          outputType = "html_fragment",
          params = list(
            author = user$fullName(),
            hospitalName = hospitalName(),
            tableFormat = "html",
            reshId = user$org(),
            registryName = registryName,
            userFullName = user$fullName(),
            userRole = user$role(),
            rendered_by_shiny = TRUE
          ))
      })
  })
  
  output$aktivitet <- shiny::renderUI({
    shiny::withProgress(message = 'Laster, dette kan ta litt tid...', {
        rapbase::renderRmd(
          sourceFile = system.file("NORIC_local_monthly_activity.Rmd",
                                   package = "noric"),
          outputType = "html_fragment",
          params = list(
            author = user$fullName(),
            hospitalName = hospitalName(),
            tableFormat = "html",
            reshId = user$org(),
            registryName = registryName,
            userFullName = user$fullName(),
            userRole = user$role(),
            rendered_by_shiny = TRUE
          ))
      }) 
  })
  
  output$tavi <- shiny::renderUI({
    shiny::withProgress(message = 'Laster, dette kan ta litt tid...', {
        rapbase::renderRmd(
          sourceFile = system.file("NORIC_tavi_report.Rmd", package = "noric"),
          outputType = "html_fragment",
          params = list(
            author = user$fullName(),
            hospitalName = hospitalName(),
            tableFormat = "html",
            reshId = user$org(),
            registryName = registryName,
            userFullName = user$fullName(),
            userRole = user$role(),
            rendered_by_shiny = TRUE
          ))
      }) 
  })
  
  output$downloadReportProsedyrer <- shiny::downloadHandler(
    filename = function() {
      downloadFilename(fileBaseName = "NORIC_local_monthly")
    },
    content = function(file) {
      contentFile(file, 
                  srcFile = "NORIC_local_monthly.Rmd", 
                  tmpFile = basename(tempfile(fileext = ".Rmd")),
                  type = "pdf",
                  orgId = user$org(),
                  orgName = hospitalName(),
                  userFullName = user$fullName(),
                  userRole = user$role(),
                  registryName = registryName,
                  tableFormat = "latex")
    })
  
  output$downloadReportAktivitet <- shiny::downloadHandler(
    filename = function() {
      downloadFilename(fileBaseName = "NORIC_local_monthly_activity")
    },
    content = function(file) {
      contentFile(file,
                  srcFile = "NORIC_local_monthly_activity.Rmd",
                  tmpFile = basename(tempfile(fileext = ".Rmd")),
                  type = "pdf",
                  orgId = user$org(),
                  orgName = hospitalName(),
                  userFullName = user$fullName(),
                  userRole = user$role(),
                  registryName = registryName,
                  tableFormat = "latex")
    })
  
  output$downloadReportTavi <- shiny::downloadHandler(
    filename = function() {
      downloadFilename(fileBaseName = "NORIC_tavi_report")
    },
    content = function(file) {
      contentFile(file,
                  srcFile = "NORIC_tavi_report.Rmd",
                  tmpFile = basename(tempfile(fileext = ".Rmd")),
                  type = "pdf",
                  orgId = user$org(),
                  orgName = hospitalName(),
                  userFullName = user$fullName(),
                  userRole = user$role(),
                  registryName = registryName,
                  tableFormat = "latex")
    })
  
  
  # DATADUMP
  
  ## Data sets available for datadump
  dataSetsDump <- shiny::reactiveVal(
    c("AndreProsedyrerVar",
      "AnnenDiagnostikkVar",
      "AngioPCIVar",
      "AortaklaffVar",
      "AortaklaffOppfVar",
      "AortaklaffProm",
      "CTAngioVar",
      "ForlopsOversikt_ignorererKaldender",
      "MitralklaffVar",
      "PasienterStudier_ignorerKalender",
      "SegmentStent",
      "segment_history",
      "SkjemaOversikt", 
      "UtskrDiagnoser", 
      "MergeReportFID", 
      "MergeReportPID", 
      "MergeReportSegmentId")
  )
  
  shiny::observeEvent(list(user$role(), user$org()), {
    if (!(user$role() == "SC" & user$org() == 0)) {
      # Remove if not national SC-role
      dataSetsDump(dataSetsDump()[!dataSetsDump() %in% "AortaklaffProm"])
      dataSetsDump(dataSetsDump()[!dataSetsDump() %in% "MergeReportFID"])
      dataSetsDump(dataSetsDump()[!dataSetsDump() %in% "MergeReportPID"])
      dataSetsDump(dataSetsDump()[!dataSetsDump() %in% "MergeReportSegmentId"])
    }
  })
  
  output$selectDumpSet <- shiny::renderUI({
    htmltools::tagList(
      shiny::selectInput(inputId = "dumpDataSet",
                         label = "Velg datasett:",
                         choices = dataSetsDump()))
  })
  
  output$dataDumpInfo <- shiny::renderUI({
    p(paste("Valgt for nedlasting:", input$dumpDataSet))
  })
  
  output$dumpDownload <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = input$dumpDataSet, fileext = ".csv"))
    },
    content = function(file) {
      contentDump(file = file, type = input$dumpFormat)
    }
  )
  
  
  # Verktøy - Metadata
  meta <- shiny::reactive({
    noric::describeRegistryDb(registryName = registryName)
  })
  
  output$metaControl <- shiny::renderUI({
    tabs <- names(meta())
    shiny::selectInput(inputId = "metaTab", 
                       label = "Velg tabell:", 
                       choices =  tabs)
  })
  
  output$metaDataTable <- DT::renderDataTable(
    expr = meta()[[input$metaTab]], 
    rownames = FALSE,
    options = list(
      lengthMenu = c(25, 50, 100, 200, 400),
      language = list(
        lengthMenu = "Vis _MENU_ rader per side",
        search = "S\u00f8k:",
        info = "Rad _START_ til _END_ av totalt _TOTAL_",
        paginate = list(previous = "Forrige", `next` = "Neste")
      ))
  )
  
  output$metaData <- shiny::renderUI({
    DT::dataTableOutput("metaDataTable")
  })
  
  # Abonnement og verktøy-utsending
  orgs <- noric::mapOrgReshId(
    registryName =  registryName,
    asNamedList = TRUE)
  
  ## currently, function parameters are the same for all reports
  pn <- c("outputType",
          "title",
          "author",
          "orgName",
          "orgId",
          "registryName",
          "userFullName",
          "userRole", 
          "tableFormat")
  
  pv <- c("pdf",
          "Månedsresultater",
          "unknown author",
          "user$orgName()",
          "user$org()",
          "registryName",
          "userFullName",
          "user$role()", 
          "latex")
  
  subReports <- shiny::reactiveVal(
    list()
  )
  
  shiny::observeEvent(list(user$org(), user$role()), {
    subReports(list(
      `Invasive prosedyrer` = list(
        synopsis = paste0("M\u00E5nedlig oppsummering av invasive prosedyrer ",
                          "siste \u00E5r"),
        fun = "reportProcessor",
        paramNames = c("report", pn),
        paramValues = c("NORIC_local_monthly", pv)
      )
    ))
    
    if (user$role() == "SC" & shiny::req(user$org()) != 0) {
      subReportsOperator <- list(
        `Angiografør/Operatør` = list(
          synopsis = "Angiografør/Operatør siste \u00E5r",
          fun = "reportProcessor",
          paramNames = c("report", pn),
          paramValues = c("NORIC_local_monthly_activity", pv)
        )
      )
      subReports(c(subReports(), subReportsOperator))
    }
    
    if (user$org() %in% c(102966, 700422, 109880, 104284, 101619)) {
      subReportsAortaklaff <- list(
        `Aortaklaff` = list(
          synopsis = "NORIC aortaklaff",
          fun = "reportProcessor",
          paramNames = c("report", pn),
          paramValues = c("NORIC_tavi_report", pv)
        )
      )
      subReports(c(subReports(), subReportsAortaklaff))
    }
  })
  
  
  subParamNames <- shiny::reactive(c(
    "orgId",
    "orgName",
    "userFullName",
    "userRole",
    "registryName", 
    "tableFormat"
  ))
  subParamValues <- shiny::reactive(c(
    user$org(),
    user$orgName(),
    user$fullName(),
    user$role(),
    registryName, 
    "latex"
  ))
  
  ## serve subscriptions (Abonnement)
  rapbase::autoReportServer(
    id = "noricSubscription",
    registryName = "noric",
    type = "subscription",
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = subReports,
    orgs = orgs,
    user = user
  )
  
  # Ny Utsending 
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
                     "tableFormat"),
      paramValues = c("NORIC_kvalitetsindikator",
                      "pdf",
                      "Månedsresultater",
                      "unknown author",
                      "unknown organization",
                      999999,
                      "registryName",
                      "userFullName",
                      "user$role()", 
                      "latex")
    ), 
    
    
    `Invasive prosedyrer` = list(
      synopsis = paste("NORIC ",
                       "invasive prosedyrer"),
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
                     "tableFormat"),
      paramValues = c("NORIC_local_monthly",
                      "pdf",
                      "Månedsresultater",
                      "unknown author",
                      "unknown organization",
                      999999,
                      "registryName",
                      "userFullName",
                      "user$role()", 
                      "latex")
    ), 
    
    `Aortaklaff` = list(
      synopsis = paste("NORIC ",
                       "aortaklaff"),
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
                     "tableFormat"),
      paramValues = c("NORIC_tavi_report",
                      "pdf",
                      "Månedsresultater",
                      "unknown author",
                      "unknown organization",
                      999999,
                      "registryName",
                      "userFullName",
                      "user$role()", 
                      "latex")
    )
  )
  
  orgDispatch <- rapbase::autoReportOrgServer("noricDispatch", orgs)
  
  dispatchParamNames <- shiny::reactive(
    c("orgName", "orgId", "registryName", "userFullName", 
      "userRole", "tableFormat")
  )
  dispatchParamValues <- shiny::reactive(
    c(orgDispatch$name(), orgDispatch$value(), 
      registryName, user$fullName(), user$role(), "latex")
  )
  
  eligible <- reactiveVal(FALSE)
  observeEvent(list(user$org(), user$role()), {
    eligible(all(c(user$role() == "SC", user$org() == 0)))
  })
  ## serve dispatchments (Utsending)
  rapbase::autoReportServer(
    id = "noricDispatch",
    registryName = "noric", 
    type = "dispatchment",
    org = orgDispatch$value, 
    paramNames = dispatchParamNames,
    paramValues = dispatchParamValues, 
    reports = dispatch, 
    orgs = orgs,
    eligible = eligible,
    user = user,
    runAutoReportButton = (Sys.getenv("R_RAP_INSTANCE") %in% c("QAC", "DEV", "TEST"))
  )
  
  
  
  #Verktøy - nedlasting rapporter
  orgs_df <- shiny::reactiveVal(orgs)
  shiny::observeEvent(registryName, {
    # Update orgs_df when registryName changes
    orgs_df(noric::mapOrgReshId(registryName = registryName,
                                asNamedList = FALSE))
  })
  
  ## innhold kontrollpanel:
  output$dwnldControlRap <- shiny::renderUI({
    shiny::selectInput(inputId = "dwldRapport",
                       label = "Velg rapport:",
                       choices = list(
                         "Kvalitetsindikatorer" = "NORIC_kvalitetsindikator", 
                         "Filvask avdød" = "NORIC_filvask_avdod", 
                         "Invasive prosedyrer" = "NORIC_local_monthly",
                         "Angiografør/Operatør" = "NORIC_local_monthly_activity",
                         "Aortaklaff" = "NORIC_tavi_report"))
  })
  
  output$dwnldControl <- shiny::renderUI({
    shiny::selectInput(inputId = "dwldSykehus",
                       label = "Velg sykehus:",
                       choices = orgs)
  })
  
  output$dwldInfo <- shiny::renderUI({
    p(paste("Valgt for nedlasting:\n",
            input$dwldRapport,
            "fra", 
            orgs_df()[orgs_df()$id == input$dwldSykehus, "name"]))
  })
  
  output$dwnldReport <- shiny::downloadHandler(
    
    filename = function() {
      downloadFilename(fileBaseName = input$dwldRapport)
    },
    
    content = function(file) {
      contentFile(file = file, 
                  srcFile = paste0(input$dwldRapport, ".Rmd"), 
                  tmpFile = basename(tempfile(fileext = ".Rmd")),
                  type = "pdf",
                  orgId = input$dwldSykehus,
                  orgName = orgs_df()[orgs_df()$id == input$dwldSykehus, "name"],
                  userFullName = user$fullName(),
                  userRole = user$role(),
                  registryName = registryName, 
                  tableFormat = "latex")
    })
  
  # Verktøy - brukerstatistikk
  rapbase::statsServer2(
    id = "noricStats",
    registryName = "noric",
    app_id = Sys.getenv("FALK_APP_ID"),
    eligible = shiny::req(eligible)
  )
  rapbase::statsGuideServer("noricStatsGuide",
                            registryName = "noric")

  # Verktøy - Eksport
  rapbase::exportUCServer("noricExport",
                          registryName,
                          "noric")

  rapbase::exportGuideServer("noricExportGuide",
                             registryName)
  
  
  # Verktøy - Staging data
  output$stagingControl <- shiny::renderUI({
    shiny::actionButton(inputId = "lagNyStaging",
                        label = "Lag ny staging data nå")
  })
  
  # reactive values staging data
  rv <- shiny::reactiveValues(
    staged = NULL
  )
  
  # observers staging data
  shiny::observeEvent(input$lagNyStaging, {
    message("Lager ny staging data...")
    shiny::withProgress(message = "Lager ny staging data, vent!", value = 0, {
      noric::makeStagingDataKi(registryName = registryName,
                               rendered_by_shiny = TRUE)
      rv$staged <- noric::makeStagingDataFrame(registryName = registryName)
    })
    message("Ny staging data laget!")
  })
  
  #' A column of delete buttons for each row in the data frame
  #' for the first column
  #'
  #' @param df data frame
  #' @param id id prefix to add to each actionButton.
  #' The buttons will be id'd as id_INDEX.
  #' @return A DT::datatable that has the delete 
  #' buttons in the last column and \code{df} in the others
  deleteButtonColumn <- function(df, id, ...) {
    # function to create one action button as string
    f <- function(i) {
      as.character(shiny::actionButton(
        # The id prefix with index
        inputId = paste(id, i, sep = "_"),
        label = NULL,
        icon = icon("trash"),
        onclick = "Shiny.setInputValue(\"deletePressed\", this.id, {priority: 'event'})"
      ))
    }
    if(is.null(nrow(df))) {
      return(NULL)
    }
    deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
    
    # Return a data table
    DT::datatable(cbind(df, Slett = deleteCol),
                  escape = FALSE,
                  rownames = FALSE, 
                  options = list(
                    lengthMenu = c(25, 50, 100, 200, 400),
                    language = list(
                      lengthMenu = "Vis _MENU_ rader per side",
                      search = "S\u00f8k:",
                      info = "Rad _START_ til _END_ av totalt _TOTAL_",
                      paginate = list(previous = "Forrige", `next` = "Neste")
                    )
                  ))
  }
  
  #' Extracts the row id number from the id string
  #' @param idstr the id string formated as id_INDEX
  #' @return INDEX from the id string id_INDEX
  parseDeleteEvent <- function(idstr) {
    res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
    if (! is.na(res)) res
  }
  
  observeEvent(input$deletePressed, {
    rowNum <- parseDeleteEvent(input$deletePressed)
    
    # Slette valgt datasett
    rowName <- rapbase::listStagingData(registryName = registryName)[rowNum]
    rapbase::deleteStagingData(registryName = registryName,
                               dataName = rowName)
    rv$staged <- noric::makeStagingDataFrame(registryName = registryName)
  })
  
  output$stagingDataTable <- DT::renderDataTable(
    expr = deleteButtonColumn(df = rv$staged, id = "delete_button")
  )
  
  # serve bulletins
  orgDataStaging <- rapbase::autoReportOrgServer("noricBulletin", orgs)
  
  bulletinParamNames <- shiny::reactive(
    c(
      "orgName",
      "orgId",
      "registryName",
      "userFullName",
      "userRole"
    )
  )
  bulletinParamValues <- shiny::reactive(
    c(orgDataStaging$name(),
      orgDataStaging$value(),
      registryName,
      user$fullName(),
      user$role()
    )
  )
  
  bulletins <- list(
    `KI nasjonal staged data` = list(
      synopsis = paste("NORIC staged data KI"),
      fun = "bulletinProcessorStaging",
      paramNames = shiny::reactive(c(
        "dataset",
        "author",
        "orgName",
        "orgId",
        "registryName",
        "userFullName",
        "userRole"
      )),
      paramValues = shiny::reactive(c(
        "ki",
        "unknown author",
        "unknown organization",
        999999,
        "registryName",
        "userFullName()",
        "user$role()"
      ))
    )
  )
  
  ## serve bulletin ()
  rapbase::autoReportServer(
    id = "noricBulletin",
    registryName = "noric",
    type = "bulletin",
    org = orgDataStaging$value,
    paramNames = bulletinParamNames,
    paramValues = bulletinParamValues,
    reports = bulletins,
    orgs = orgs,
    eligible = eligible,
    user = user
  )
  
  
})
