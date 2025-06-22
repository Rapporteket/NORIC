# sship::dec(
#   "c://Users/ast046/Downloads/noricStaging10296610fd24ad185ca.sql.gz__20250312_110414.tar.gz",
#   keyfile = "p://.ssh/id_rsa",
#   target_dir = "c://Users/ast046/Downloads/."
#)


#devtools::install("../rapbase/.", upgrade = FALSE)

#devtools::install(upgrade = FALSE)


source("dev/renv.R")

shiny::runApp(system.file('shinyApps/noric', package = 'noric'), launch.browser = TRUE)

