#' Create national database based on regional databases
#'
#' @export
#'
createNational <- function() {
  nationalDb <- "noric_nasjonal"
  con <- rapbase::rapOpenDbConnection(nationalDb)$con
  listOfTables <- c(
    "andreprosedyrervarnum",
    "angiopcinum",
    "angiopcivardel1num",
    "angiopcivardel2num",
    "annendiagnostikkvarnum",
    "aortaklaffoppfvarnum",
    "aortaklaffvarnum",
    "ctangiovarnum",
    "forlopsoversikt",
    "mitralklaffvarnum",
    "pasienterstudier",
    "segmentstentnum",
    "skjemaoversikt",
    "taviprom"
  )
  listOfDb <- unique((
    rapbase::getConfig("rapbaseConfig.yml")$reg$noric$databases |>
    unlist() |>
    matrix(nrow=2) |>
    t() |>
    as.data.frame() |>
    dplyr::filter(.data$V1 != nationalDb)
  )$V1)

  # Check if db in listOfDb exists, and filter out non-existing db
  query <- "SHOW DATABASES;"
  res <- DBI::dbGetQuery(con, query)
  listOfDb <- listOfDb[listOfDb %in% res$Database]
  if (length(listOfDb) == 0) {
    stop("No regional databases found in the configuration file.")
  }

  message("Start creating national database!")
  for (aTable in listOfTables) {
    message(paste0("Check if table ", aTable, " exists in ", nationalDb))
    query <- paste0("SHOW TABLES LIKE '", aTable, "';")
    res <- DBI::dbGetQuery(con, query)
    if (nrow(res) > 0) {
      message(paste0("Delete table content in ", aTable))
      query <- paste0(
        "DELETE FROM ", nationalDb, ".", aTable, ";"
      )
      DBI::dbExecute(con, query)
      loopListofDb <- listOfDb
    } else {
      message(paste0("Create table ", aTable, " from ", listOfDb[[1]]))
      query <- paste0(
        "CREATE TABLE ", aTable,
        " AS SELECT * FROM ", listOfDb[[1]], ".", aTable, ";"
      )
      DBI::dbExecute(con, query)
      # Exclude the first db as it is already copied
      loopListofDb <- listOfDb[-1]
    }
    for (aDb in loopListofDb) {
      message(paste0("Copy table ", aTable, " from ", aDb))
      query <- paste0(
        "INSERT INTO ", nationalDb, ".", aTable,
        " SELECT * FROM ", aDb, ".", aTable, ";"
      )
      DBI::dbExecute(con, query)
    }
  }
  message("Finished creating national database!")
  rapbase::rapCloseDbConnection(con)
  con <- NULL
}
