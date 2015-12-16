# NORIC
Report generating scripts in R/knitr for the Norwegian Registry for Invasive Cardiology

## Install
The current package can be fetched directly from your R session. If not already
present, first install the devtools-package from your R terminal:

```r
install.packages("devtools")
```

Most R packages at Rapporteket will depend on the "rapbase" package. If not
already present, install the "rapbase" package from github:

```r
devtools::install_github("Rapporteket/rapbase")
```

To install the current package, first time or at any future upgrade, do:

```r
devtools::install_github("Rapporteket/[package-name]", ref = "rel")
```

NOTE: Communicating through a proxy might cause the above install command to
fail. If so, try the following prior to the above install command:

```r
library(httr)
set_config(
  use_proxy(url="18.91.12.23", port=8080, username="user",password="passwd")
)
```

replacing the example parameter values with whatever applies for the
system the package is being installed on

## Develop
Contributors submit their code to the rel (release) branch which is
subject to testing at Rapporteket. Upon acceptance rel will me merged to
the master branch and tagged