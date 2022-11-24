library(magrittr)
library(noric)
library(readr)
library(rpivotTable)
library(shiny)

shinyServer(function(input, output, session) {
  
  rapbase::appLogger(session = session, msg = "Starting NORIC application")
  
  # Parameters that will remain throughout the session
  ## setting values that do depend on a Rapporteket context
  reshId <- rapbase::getUserReshId(session)
  hospitalName <- noric::getHospitalName(reshId)
  userFullName <- rapbase::getUserFullName(session)
  userRole <- rapbase::getUserRole(session)
  registryName <- noric::makeRegistryName("noricStaging", reshId)
  mapOrgId <- mapOrgReshId(registryName)
  author <- paste0(userFullName, "/", "Rapporteket")
  
  
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
  
  ## dispatchment and use stats hidden when not national registry
  if (!isNationalReg(reshId)) {
    shiny::hideTab(inputId = "tabs", target = "Utsending")
    shiny::hideTab(inputId = "tabs", target = "Bruksstatistikk")
    shiny::hideTab(inputId = "tabs", target = "Nedlasting rapporter")
    shiny::hideTab(inputId = "tabs", target = "Staging data")
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
          sep = ".", 
          switch(type,
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
      shiny::withProgress(message = 'Rendering, please wait!', {
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
          rendered_by_shiny = TRUE) 
      })
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
  output$appUserName <- shiny::renderText(userFullName)
  output$appOrgName <- shiny::renderText(paste(hospitalName, 
                                               userRole, 
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
    htmlRenderRmd("veiledning.Rmd")
  })
  
  # Utforsker
  ## Data sets available
  if (userRole == "SC") {
    dataSets <- list(`Bruk og valg av data...` = "info",
                     `Angio PCI med utledete variabler` = "ApLight",
                     `Angio PCI rådata` = "AP",
                     `Andre prosedyrer` = "AnP",
                     `Annen diagnostikk` = "AnD",
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
                           registryName = registryName,
                           singleRow = FALSE,
                           session = session,
                           userRole = userRole, 
                           fromDate = input$utforskerDateRange[1],
                           toDate = input$utforskerDateRange[2])
  })
  
  metaDat <- shiny::reactive({
    noric::getPivotDataSet(setId = input$selectedDataSet,
                           registryName = registryName,
                           singleRow = TRUE,
                           session = session,
                           userRole = userRole, 
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
                           choices = dataSets, 
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
    req(input$selectedDataSet)
    if (length(rvals$showPivotTable) == 0 | rvals$showPivotTable) {
      h4(paste0("Valgt datasett: ",
                names(dataSets)[dataSets == input$selectedDataSet], 
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
                           userRole = userRole,
                           singleRow = TRUE, 
                           fromDate = NULL,
                           toDate = NULL)
  })
  
  ## innhold kontrollpanel:
  output$kbControl <- shiny::renderUI({
    shiny::selectInput(inputId = "kbdTab",
                       label = "Vis kodebok for tabellen:",
                       choices =  dataSets)
  })
  
  # vektor med alle variabelnavn i valgt tabell
  selectedkbTabVars <- shiny::reactive({
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
  output$stentbruk <- shiny::renderUI({
    htmlRenderRmd("NORIC_local_monthly_stent.Rmd")
  })
  
  output$prosedyrer <- shiny::renderUI({
    htmlRenderRmd("NORIC_local_monthly.Rmd")
  })
  
  output$aktivitet <- shiny::renderUI({
    htmlRenderRmd("NORIC_local_monthly_activity.Rmd")
  })
  
  output$downloadReportStentbruk <- shiny::downloadHandler(
    filename = function() {
      downloadFilename("NORIC_local_monthly_stent",
                       input$formatStentbruk) 
    },
    
    content = function(file) {
      contentFile(file = file,
                  srcFile =  "NORIC_local_monthly_stent.Rmd",
                  tmpFile = basename(tempfile(fileext = ".Rmd")),
                  type = input$formatStentbruk, 
                  useReportProcessor = FALSE)
    }
  )
  
  output$downloadReportProsedyrer <- shiny::downloadHandler(
    filename = function() {
      downloadFilename(fileBaseName = "NORIC_local_monthly",
                       type =  input$formatProsedyrer)
    },
    
    content = function(file) {
      contentFile(file = file, 
                  srcFile = "NORIC_local_monthly.Rmd", 
                  tmpFile = basename(tempfile(fileext = ".Rmd")),
                  type = input$formatProsedyrer, 
                  useReportProcessor = FALSE)
    }
  )
  
  output$downloadReportAktivitet <- shiny::downloadHandler(
    filename = function() {
      downloadFilename(fileBaseName = "NORIC_local_monthly_activity", 
                       type = input$formatAktivitet)
    },
    
    content = function(file) {
      contentFile(file = file, 
                  srcFile = "NORIC_local_monthly_activity.Rmd", 
                  tmpFile = basename(tempfile(fileext = ".Rmd")),
                  type = input$formatAktivitet, 
                  useReportProcessor = FALSE)
    }
  )
  
  
  # Datadump
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
  orgs <- noric::mapOrgReshId(registryName, asNamedList = TRUE)
  
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
  
  
  ## serve subscriptions (Abonnement)
  rapbase::autoReportServer(id = "noricSubscription",
                            registryName = "noric", 
                            type = "subscription",
                            reports = subReports, 
                            orgs = orgs)
  
  
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
                      registryName,
                      userFullName,
                      userRole,
                      "unknown operator")
    )
  )
  
  orgDispatch <- rapbase::autoReportOrgServer("noricDispatch", orgs)
  
  dispatchParamNames <- shiny::reactive(
    c("orgName", "orgId")
  )
  dispatchParamValues <- shiny::reactive(
    c(orgDispatch$name(), orgDispatch$value())
  )
  
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
    eligible = all(c(userRole == "SC", isNationalReg(reshId)))
  )
  
  
  
  #Verktøy - nedlasting rapporter
  orgs_df <- noric::mapOrgReshId(registryName = registryName,
                                 asNamedList = FALSE)
  
  ## innhold kontrollpanel:
  output$dwnldControlRap <- shiny::renderUI({
    shiny::selectInput(inputId = "dwldRapport",
                       label = "Velg rapport:",
                       choices = list(
                         "Kvalitetsindikatorer" = "NORIC_kvalitetsindikator", 
                         "Filvask avdød" = "NORIC_filvask_avdod"))
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
            orgs_df[orgs_df$id == input$dwldSykehus, "name"]))
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
                  type = "PDF", 
                  orgId = input$dwldSykehus, 
                  orgName = orgs_df[orgs_df$id == input$dwldSykehus, "name"],
                  useReportProcessor = TRUE)
    })
  
  # Verktøy - brukerstatistikk
  rapbase::statsServer(
    id = "noricStats",
    registryName = "noric",
    eligible = all(c(userRole == "SC", isNationalReg(reshId)))
  )
  rapbase::statsGuideServer("noricStatsGuide",
                            registryName = registryName)
  
  # Verktøy - Eksport
  rapbase::exportUCServer(id = "noricExport", 
                          registryName = registryName,
                          repoName = "noric", 
                          eligible = (userRole == "SC"))
  
  rapbase::exportGuideServer(id = "noricExportGuide",
                             registryName = registryName)
  
  
  # Verktøy - Staging data
  output$stagingControl <- shiny::renderUI({
    shiny::actionButton(inputId = "lagNyStaging",
                        label = "Lag ny staging data nå")
  })
  
  
  # reactive values staging data
  rv <- shiny::reactiveValues(
    staged = noric::makeStagingDataFrame(registryName = registryName)
  )
  
  # observers staging data
  shiny::observeEvent(input$lagNyStaging, {
    shiny::withProgress(message = 'Lager ny staging data, vent!', value = 0, {
      noric::makeStagingDataKi(registryName = registryName,
                               rendered_by_shiny = TRUE)
      
      rv$staged <- noric::makeStagingDataFrame(registryName = registryName)
    })
  })
  
  
  #' A column of delete buttons for each row in the data frame for the first column
  #'
  #' @param df data frame
  #' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
  #' @return A DT::datatable that has the delete 
  #' buttons in the last column and \code{df} in the others
  deleteButtonColumn <- function(df, id, ...) {
    # function to create one action button as string
    f <- function(i) {
      as.character(shiny::actionButton(
        # The id prefix with index
        inputId = paste(id, i, sep = "_"),
        label = NULL,
        icon = icon('trash'),
        onclick = 'Shiny.setInputValue(\"deletePressed\", this.id, {priority: "event"})'))
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
                    )))
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
    expr = deleteButtonColumn(df = rv$staged, id = 'delete_button')
  ) 
  
  
  
  # serve bulletins
  orgDataStaging <- rapbase::autoReportOrgServer("noricBulletin", orgs)

  bulletinParamNames <- shiny::reactive(
    c("orgName", "orgId")
  )
  bulletinParamValues <- shiny::reactive(
    c(orgDataStaging$name(), orgDataStaging$value())
  )

  
  bulletins <- list(
    `KI nasjonal staged data` = list(
      synopsis = paste("NORIC staged data KI"),
      fun = "bulletinProcessorStaging",
      paramNames = c("dataset",
                     "author",
                     "orgName",
                     "orgId",
                     "registryName",
                     "userFullName",
                     "userRole",
                     "userOperator"),
      paramValues = c("NORIC_makeStagingKI",
                      "unknown author",
                      "unknown organization",
                      999999,
                      registryName,
                      userFullName,
                      userRole,
                      "unknown operator")
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
    eligible = all(c(userRole == "SC", isNationalReg(reshId)))
  )



  
})
