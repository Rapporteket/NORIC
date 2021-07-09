#' Fjerne tulleregistreringer
#' Fjerne registrerigner fra før de ulike sykehusene offisielt ble med i NORIC
#'
#' @param df tabell ap, ak, fo osv (Må inneholder variabelen `AvdRESH`)
#' @param var datoVariabel
#' AP, anP, anD og SS har ProsedyreDato
#' CT har undersokDato
#' FO og SO har HovedDato
#'
#' @return
#' @export
#'
#' @examples
fjerne_tulleregistreringer <- function(df, var = ProsedyreDato){

  message("Fjerner tulleregisreringer som ble utført før sykehusene offisielt
          ble med i NORIC")




  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  df %>%
    dplyr::filter(
      ( # HUS
        (.data$AvdRESH == 102966) & (as.Date({{ var }} ) >=
                                       "2013-01-01")) |
        (# UNN
        (.data$AvdRESH == 101619) & (as.Date({{ var }}) >=
                                       "2013-05-01")) |
        (# Ullevaal
        (.data$AvdRESH == 109880) & (as.Date({{ var }}) >=
                                       "2014-01-01"))|
        (# St Olav
        (.data$AvdRESH == 104284) & (as.Date({{ var }}) >=
                                       "2014-01-01"))|
        (# SSA
        (.data$AvdRESH == 114150) & (as.Date({{ var }}) >=
                                       "2014-01-01"))|
        (#SUS
        (.data$AvdRESH == 105502) & (as.Date({{ var }}) >=
                                       "2014-01-01"))|
        (# Riksen
        (.data$AvdRESH == 700422) & (as.Date({{ var }}) >=
                                       "2015-01-01"))|
        (# LHL Gardermoen = AHUS Gardermoen
        (.data$AvdRESH == 106944) & (as.Date({{ var }}) >=
                                       "2015-01-01"))|
        (# Ahus
        (.data$AvdRESH == 108141) & (as.Date({{ var }}) >=
                                       "2016-01-01"))|
        (# Nordlandsykehuset (bodø)
        (.data$AvdRESH == 4210141) & (as.Date({{ var }}) >=
                                        "2020-02-10"))
    )

}
