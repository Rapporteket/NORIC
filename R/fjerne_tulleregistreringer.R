#' Remove test-registrations
#' Remove registrations done before the hospitals officially joined NORIC
#'
#' @param df One of the NORIC-tables: AP, KA, FO etc (Must contain variable
#'  `AvdRESH`)
#' @param var variable name for date of procedure, used for filtering old
#' procedures. AP, anP, anD og SS uses `ProsedyreDato`, CT uses `undersokDato`
#' FO og SO uses `HovedDato`
#'
#' @return data.frame where old procedures are removed
#' @export
#'
#' @examples
#' x <- data.frame(AvdRESH = c(rep(102966,3), rep(109880,3)),
#'                 ProsedyreDato = rep(c("2012-01-31",
#'                                       "2013-02-28",
#'                                       "2020-06-19"),2))
fjerne_tulleregistreringer <- function(df, var = ProsedyreDato) {

  if (!("AvdRESH" %in% names(df))) stop("df must contain variable AvdRESH")

  message("Fjerner tulleregisreringer som ble utført før sykehusene offisielt
          ble med i NORIC")

  df %>%
    dplyr::filter(
      (# HUS
        (.data$AvdRESH == 102966) & (as.Date({{ var }}) >= "2013-01-01")) |

        (# UNN
        (.data$AvdRESH == 101619) & (as.Date({{ var }}) >= "2013-05-01")) |

        (# Ullevaal
        (.data$AvdRESH == 109880) & (as.Date({{ var }}) >= "2014-01-01")) |

        (# St Olav
        (.data$AvdRESH == 104284) & (as.Date({{ var }}) >= "2014-01-01")) |

        (# SSA
        (.data$AvdRESH == 114150) & (as.Date({{ var }}) >= "2014-01-01")) |

        (#SUS
        (.data$AvdRESH == 105502) & (as.Date({{ var }}) >= "2014-01-01")) |

        (# Riksen
        (.data$AvdRESH == 700422) & (as.Date({{ var }}) >= "2015-01-01")) |

        (# LHL Gardermoen = AHUS Gardermoen
        (.data$AvdRESH == 106944) & (as.Date({{ var }}) >= "2015-01-01")) |

        (# Ahus Nordbyhagen
        (.data$AvdRESH == 108141) & (as.Date({{ var }}) >= "2016-01-01")) |

        (# Nordlandsykehuset (bodø)
        (.data$AvdRESH == 4210141) & (as.Date({{ var }}) >= "2020-02-10"))
    )
}
