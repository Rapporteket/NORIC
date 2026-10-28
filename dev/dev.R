devtools::install("../rapbase/.", upgrade = FALSE)

source("dev/renv.R")

source("../../felles/renv.R")

shiny::runApp('inst/shinyApps/noric', launch.browser = TRUE)

