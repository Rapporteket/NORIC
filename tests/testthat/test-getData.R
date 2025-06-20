# For these tests to work locally make sure an instance of mysql server is
# available and that the necessary user privileges are provided, e.g. as SQL:
#   \code{grant all privileges on [DATABASE].* to '[USER]'@'localhost';}
# When run at Github Actions build servers [USER] must be set to 'actions' and
# with an empty password (as also assumed in the above localhost example).
# See also .github/workflows/R-CMD-check.yml

# Database infrastructure is only guaranteed at Github Actions and our own
# dev env.
# Tests running on other environments should be skipped:
check_db <- function(is_test_that = TRUE) {
  if (Sys.getenv("R_RAP_INSTANCE") == "DEV") {
    NULL
  } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    if (is_test_that) {
      testthat::skip("Possible lack of database infrastructure")
    } else {
      1
    }
  }
}

# preserve initial state
config_path <- Sys.getenv("R_RAP_CONFIG_PATH")


test_that("env vars needed for testing is present", {
  check_db()
  expect_true("MYSQL_HOST" %in% names(Sys.getenv()))
  expect_true("MYSQL_USER" %in% names(Sys.getenv()))
  expect_true("MYSQL_PASSWORD" %in% names(Sys.getenv()))
})

# prep db for testing
if (is.null(check_db(is_test_that = FALSE))) {
  con <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                             host = Sys.getenv("MYSQL_HOST"),
                             user = Sys.getenv("MYSQL_USER"),
                             password = Sys.getenv("MYSQL_PASSWORD"),
                             bigint = "integer"
  )
  RMariaDB::dbExecute(con, "CREATE DATABASE testDb;")
  RMariaDB::dbDisconnect(con)
}

# make queries for creating tables
fc <- file(system.file("testDb.sql", package = "noric"), "r")
t <- readLines(fc)
close(fc)
sql <- paste0(t, collapse = "\n")
queries <- strsplit(sql, ";")[[1]]

test_that("relevant test database and tables can be made", {
  check_db()
  con <- rapbase::rapOpenDbConnection("testDb")$con
  for (i in seq_len(length(queries))) {
    expect_equal(class(RMariaDB::dbExecute(con, queries[i])), "integer")
  }
  rapbase::rapCloseDbConnection(con)
})

### ALL QUERY FUNS SHOULD BE TESTED HERE ###

test_that("mitralklaff data can be provided", {
  check_db()
  res <- getMk("testDb", fromDate = "1900-01-01", toDate = Sys.Date(),
               singleRow = TRUE)
  expect_equal(class(res), "list")
  expect_equal(class(res$mK), "data.frame")
})

test_that("pasientstudier data can be provided", {
  check_db()
  res <- getPs("testDb", fromDate = "1900-01-01", toDate = Sys.Date(),
               singleRow = TRUE)
  expect_equal(class(res), "list")
  expect_equal(class(res$pS), "data.frame")
})

# remove test db
if (is.null(check_db(is_test_that = FALSE))) {
  con <- rapbase::rapOpenDbConnection("testDb")$con
  RMariaDB::dbExecute(con, "DROP DATABASE testDb;")
  rapbase::rapCloseDbConnection(con)
}

# restore initial state
Sys.setenv(R_RAP_CONFIG_PATH = config_path)