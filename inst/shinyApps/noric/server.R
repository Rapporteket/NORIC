library(magrittr)
library(noric)
library(readr)
library(rpivotTable)
library(shiny)

shinyServer(function(input, output, session) {
  
  rapbase::appLogger(session = session, msg = "Starting NORIC application")
  
  map_db_resh <-
    rapbase::getConfig("rapbaseConfig.yml")$reg$noric$databases |>
    unlist() |>
    matrix(nrow=2) |>
    t() |>
    as.data.frame() |>
    dplyr::rename(dbname = V1, AvdRESH = V2)
  
  map_orgname <- map_db_resh |>
    dplyr::rename(Sykehusnavn = dbname) |>
    fikse_sykehusnavn() |>
    dplyr::rename(UnitId = AvdRESH,
                  orgname = Sykehusnavn)
  
  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "noric",
    caller = "noric",
    map_orgname = shiny::req(map_orgname)
  )
  
  # Parameters that may change depending on the role and org of user
  ## setting values that do depend on a Rapporteket context
  registryName <- reactive(
    map_db_resh$dbname[map_db_resh$AvdRESH == user$org()]
  )
  userFullName <- Sys.getenv("FALK_USER_FULLNAME")
  hospitalName <- reactive(
      map_orgname$orgname[map_orgname$UnitId ==  user$org()]
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
    
    if (isNationalReg(shiny::req(user$org()))) {
      shiny::hideTab(inputId = "tabs", target = "Prosedyrer")
      shiny::hideTab(inputId = "tabs", target = "Angiografør/Operatør")
      shiny::hideTab(inputId = "tabs", target = "Månedsrapporter")
      shiny::hideTab(inputId = "tabs", target = "Abonnement")
    }
    
    ## dispatchment and use stats hidden when not national registry
    if (!isNationalReg(shiny::req(user$org()))) {
      shiny::hideTab(inputId = "tabs", target = "Utsending")
      shiny::hideTab(inputId = "tabs", target = "Bruksstatistikk")
      shiny::hideTab(inputId = "tabs", target = "Nedlasting rapporter")
      shiny::hideTab(inputId = "tabs", target = "Lag nasjonal database")
    }
    
    if(shiny::req(user$org()) %in% c(108141, 4210141, 114150, 105502, 106944)){
      shiny::hideTab(inputId = "tabs", target = "Aortaklaff")
    }
  })
  
  # html rendering function for re-use
  htmlRenderRmd <- function(srcFile, params) {
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
          sep = ".", 
          switch(type,
                 PDF = "pdf", HTML = "html", REVEAL = "html", BEAMER = "pdf")
    )
  }
  
  
  # render file function for re-use
  contentFile <- function(
    file,
    srcFile,
    tmpFile,
    type,
    useReportProcessor = FALSE,
    orgId,
    orgName,
    registryName = "noric",
    userFullName,
    userRole
  ) {
    
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
          outputType = type,
          title = "unknown title",
          author = "unknown author",
          orgName = orgName,
          orgId = orgId,
          registryName = registryName,
          userFullName = userFullName,
          userRole = userRole,
          userOperator = "unknown operator",
          rendered_by_shiny = TRUE,
          tableFormat = "latex"
        )})
    }
    
    file.rename(out, file)
  }
  
  contentDump <- function(file, type) {
    d <- noric::getDataDump(registryName = registryName(),
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
  output$appUserName <- shiny::renderText(userFullName)
  output$appOrgName <- shiny::renderText(paste(hospitalName(),
                                               user$role(),
                                               sep = ", "))
  
  # User info in widget
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
  
  # Start
  output$veiledning <- shiny::renderUI({
    htmlRenderRmd("veiledning.Rmd", 
                  params = list(
                    author = userFullName,
                    hospitalName = hospitalName(),
                    tableFormat = "html",
                    reshId = user$org(),
                    registryName = registryName()
                  ))
  })
  
  # Utforsker
  ## Data sets available
  dataSets <- shiny::reactive({
    if (user$role() == "SC") {
      dataSets <- list(
        `Bruk og valg av data...` = "info",
        `Angio PCI med utledete variabler` = "ApLight",
        `Angio PCI rådata` = "AP",
        `Andre prosedyrer` = "AnP",
        `Annen diagnostikk` = "AnD",
        `Aortaklaff` = "AK",
        `Aortaklaff eprom` = "TP",
        `Aortaklaff oppfølging` = "AKOppf",
        `CT Angio` = "CT",
        `Forløpsoversikt` = "FO",
        `Mitralklaff` = "MK",
        `PasientStudier` = "PS",
        `Skjemaoversikt` = "SO",
        `Segment stent` = "SS"
      )
      # EPROM is only for nasjoanl
      if (!isNationalReg(user$org())) {
        dataSets <- within(dataSets, rm("Aortaklaff eprom"))
      }
    } else {
      dataSets <- list(
        `Bruk og valg av data...` = "info",
        `Angio PCI med utledete variabler` = "ApLight",
        `Angio PCI rådata` = "AP",
        `Andre prosedyrer` = "AnP",
        `Annen diagnostikk` = "AnD",
        `Aortaklaff` = "AK",
        `CT Angio` = "CT",
        `Forløpsoversikt` = "FO",
        `Skjemaoversikt` = "SO",
        `Segment stent` = "SS"
      )
    }
    return(dataSets)
  })
  
  ## reactive vals for utforsker
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
                           registryName = registryName(),
                           singleRow = FALSE,
                           session = session,
                           userRole = user$role(),
                           fromDate = input$utforskerDateRange[1],
                           toDate = input$utforskerDateRange[2])
  })
  
  metaDat <- shiny::reactive({
    noric::getPivotDataSet(setId = input$selectedDataSet,
                           registryName = registryName(),
                           singleRow = TRUE,
                           session = session,
                           userRole = user$role(),
                           fromDate = NULL,
                           toDate = NULL)
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
      )
    }
  })
  
  
  output$selectVars <- shiny::renderUI({
    req(input$selectedDataSet, dataSets)
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
                           registryName = registryName(),
                           session = session,
                           userRole = user$role(),
                           singleRow = TRUE, 
                           fromDate = NULL,
                           toDate = NULL)
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
  
  
  
  # Samlerapporter
  output$prosedyrer <- renderUI({
    htmlRenderRmd("NORIC_local_monthly.Rmd", params = list(
      author = userFullName,
      hospitalName = hospitalName(),
      tableFormat = "html",
      reshId = user$org(),
      registryName = registryName()
    ))
  })
  
  output$aktivitet <- renderUI({
    htmlRenderRmd("NORIC_local_monthly_activity.Rmd", params = list(
      author = userFullName,
      hospitalName = hospitalName(),
      tableFormat = "html",
      reshId = user$org(),
      registryName = registryName()
    ))
  })
  
  output$tavi <- renderUI({
    htmlRenderRmd("NORIC_tavi_report.Rmd", params = list(
      author = userFullName,
      hospitalName = hospitalName(),
      tableFormat = "html",
      reshId = user$org(),
      registryName = registryName()
    ))
  })
  
  output$downloadReportProsedyrer <- downloadHandler(
    filename = function() {
      downloadFilename(fileBaseName = "NORIC_local_monthly",
                       type = "PDF")
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
                  registryName = registryName(),
                  useReportProcessor = TRUE)
    }
  )
  
  output$downloadReportAktivitet <- shiny::downloadHandler(
    filename = function() {
      downloadFilename(fileBaseName = "NORIC_local_monthly_activity",
                       type = "PDF")
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
                  registryName = registryName(),
                  useReportProcessor = TRUE)
    }
  )
  
  
  output$downloadReportTavi <- shiny::downloadHandler(
    filename = function() {
      downloadFilename(fileBaseName = "NORIC_tavi_report",
                       type = "PDF")
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
                  registryName = registryName(),
                  useReportProcessor = TRUE)
    }
  )
  
  
  # Datadump
  
  ## Data sets available for datadump
  dataSetsDump <- reactiveVal(
    c("AndreProsedyrerVar",
      "AnnenDiagnostikkVar",
      "AngioPCIVar",
      "AortaklaffVar",
      "AortaklaffOppfVar",
      "AortaklaffProm",
      "CTAngioVar",
      "ForlopsOversikt",
      "MitralklaffVar",
      "PasienterStudier",
      "SegmentStent",
      # "segment_history",
      "SkjemaOversikt"
    )
  )
  
  observeEvent(list(user$role(), user$org()), {
    if (!(user$role() == "SC" & noric::isNationalReg(reshId = user$org()))) {
      # Remove if not national SC-role
      dataSetsDump(dataSetsDump()[!dataSetsDump() %in% "AortaklaffProm"])
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
      basename(tempfile(pattern = input$dumpDataSet,
                        fileext = ".csv"))
    },
    content = function(file) {
      contentDump(file = file, 
                  type = input$dumpFormat)
    }
  )
  
  
  # Verktøy - Nasjonal database
  output$nationalControl <- shiny::renderUI({
    htmltools::tagList(
      shiny::actionButton(inputId = "createNational",
                          label = "Populer nasjonal database"),
      shiny::br(),
      shiny::p(paste0("Ved å trykke på knappen ",
                      "vil den nasjonale databasen oppdateres."))
    )
  })
  
  shiny::observeEvent(input$createNational, {
    shiny::updateActionButton(
      inputId = "createNational",
      label = "Nasjonal database oppdatert!",
      disabled = TRUE
    )
    createNational() # make it happen
  })
  
  
  # Verktøy - Metadata
  meta <- shiny::reactive({
    noric::describeRegistryDb(registryName = registryName())
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
  orgs <- noric::mapOrgReshId(registryName =  map_db_resh$dbname[map_db_resh$AvdRESH == 0],
                              asNamedList = TRUE,
                              newNames = TRUE)
  
  ## currently, function parameters are the same for all reports
  
  pn <- c("outputType",
          "title",
          "author",
          "orgName",
          "orgId",
          "registryName",
          "userFullName",
          "userRole",
          "userOperator")
  
  pv <- c("pdf",
          "Månedsresultater",
          "unknown author",
          "user$orgName()",
          "user$org()",
          "registryName",
          "userFullName",
          "user$role()",
          "unknown operator")

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

    if (!isNationalReg(user$org()) && user$role() == "SC") {
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
    "registryName"
  ))
  subParamValues <- shiny::reactive(c(
    user$org(),
    user$orgName(),
    user$fullName(),
    user$role(),
    registryName()
  ))

  ## serve subscriptions (Abonnement)
  shiny::observeEvent(subReports(), {
    rapbase::autoReportServer(
      id = "noricSubscription",
      registryName = "noric",
      type = "subscription",
      paramNames = subParamNames,
      paramValues = subParamValues,
      reports = subReports(),
      orgs = orgs,
      user = user
    )
  })

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
                     "userOperator"),
      paramValues = c("NORIC_kvalitetsindikator",
                      "pdf",
                      "Månedsresultater",
                      "unknown author",
                      "unknown organization",
                      999999,
                      "registryName()",
                      "userFullName",
                      "user$role()",
                      "unknown operator")
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
                     "userOperator"),
      paramValues = c("NORIC_local_monthly",
                      "pdf",
                      "Månedsresultater",
                      "unknown author",
                      "unknown organization",
                      999999,
                      "registryName()",
                      "userFullName",
                      "user$role()",
                      "unknown operator")
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
                     "userOperator"),
      paramValues = c("NORIC_tavi_report",
                      "pdf",
                      "Månedsresultater",
                      "unknown author",
                      "unknown organization",
                      999999,
                      "registryName()",
                      "userFullName",
                      "user$role()",
                      "unknown operator")
    )
    
    
  )
  
  orgDispatch <- rapbase::autoReportOrgServer("noricDispatch", orgs)
  
  dispatchParamNames <- shiny::reactive(
    c("orgName", "orgId", "registryName", "userFullName", "userRole")
  )
  dispatchParamValues <- shiny::reactive(
    c(orgDispatch$name(), orgDispatch$value(), 
      registryName(), user$fullName(), user$role())
  )
  
  eligible <- reactiveVal(FALSE)
  observeEvent(list(user$org(), user$role()), {
    eligible(all(c(user$role() == "SC", isNationalReg(user$org()))))
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
  shiny::observeEvent(registryName(), {
    # Update orgs_df when registryName changes
    orgs_df(noric::mapOrgReshId(registryName = registryName(),
                                asNamedList = FALSE,
                                newNames = TRUE))
  })
  
  ## innhold kontrollpanel:
  output$dwnldControlRap <- shiny::renderUI({
    shiny::selectInput(inputId = "dwldRapport",
                       label = "Velg rapport:",
                       choices = list(
                         "Kvalitetsindikatorer" = "NORIC_kvalitetsindikator", 
                         "Filvask avdød" = "NORIC_filvask_avdod", 
                         "Invasive prosedyrer" = "NORIC_local_monthly", 
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
      downloadFilename(fileBaseName = input$dwldRapport,
                       type = "PDF")
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
                  registryName = registryName(),
                  useReportProcessor = TRUE)
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
  
  tinyeligible <- reactiveVal(FALSE)
  observeEvent(user$role(), {
    tinyeligible(user$role() == "SC")
  })
  # Verktøy - Eksport
  rapbase::exportUCServer2(id = "noricExport", 
                           registryName = registryName,
                           repoName = "noric", 
                           eligible = shiny::req(tinyeligible))
  
  rapbase::exportGuideServer2(id = "noricExportGuide",
                              registryName = registryName)
  
})
