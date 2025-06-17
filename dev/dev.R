devtools::install("../rapbase/.", upgrade = FALSE)


tarfiles <- c(
  "noric_ahus149103a17.sql.gz__20250606_114543.tar.gz",
  "noric_bergen16d77e800.sql.gz__20250606_114726.tar.gz",
  "noric_bodoe12d20ca5a.sql.gz__20250606_115550.tar.gz",
  "noric_feiring1ed756a6.sql.gz__20250606_114431.tar.gz",
  "noric_ous_ull15e3ef6b2.sql.gz__20250606_115108.tar.gz",
  "noric_soerlandet1707cbd61.sql.gz__20250606_115402.tar.gz",
  "noric_stavanger138bdafa1.sql.gz__20250606_115232.tar.gz",
  "noric_unn12d8551e1.sql.gz__20250606_115509.tar.gz"
  )

for (i in tarfiles) {
  sship::dec(
    paste0("c://Users/ast046/Downloads/", i),
    keyfile = "p://.ssh/id_rsa",
    target_dir = "c://Users/ast046/Downloads/."
  )
}


source("dev/renv.R")
shiny::runApp('inst/shinyApps/noric', launch.browser = TRUE)

