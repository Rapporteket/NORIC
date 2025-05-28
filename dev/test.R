Sys.setenv(MYSQL_HOST="localhost")
Sys.setenv(MYSQL_USER="root")
Sys.setenv(MYSQL_PASSWORD="root")
Sys.setenv(GITHUB_ACTIONS_RUN_DB_UNIT_TESTS="true")

devtools::test()
