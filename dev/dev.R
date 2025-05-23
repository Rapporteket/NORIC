devtools::install("../rapbase/.", upgrade = FALSE)

sship::dec(
  "c://Users/ast046/Downloads/noric_unn1533a348d.sql.gz__20250523_092443.tar.gz",
  keyfile = "p://.ssh/id_rsa",
  target_dir = "c://Users/ast046/Downloads/."
)


source("dev/renv.R")
shiny::runApp('inst/shinyApps/noric')

