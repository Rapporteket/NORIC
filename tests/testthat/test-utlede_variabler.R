test_that("Utlede aldersklasser", {
  x <- data.frame(alder = c(-1, 10, 17, 18, 20,
                            59, 60, 61, 69, 70,
                            99, 100, NA, NA, NA))

  # sjekk at de som skal bli NA blir det:
  testthat::expect_equal(7,
                         utlede_aldersklasse(x, var = alder) %>%
                           dplyr::filter(is.na(aldersklasse)) %>%
                           nrow())

  # Sjekk at øvre/nedre grense for aldersklasse er er riktig
  testthat::expect_equal(60,
                         utlede_aldersklasse(x, var = alder) %>%
                           dplyr::filter(aldersklasse == "60-69") %>%
                           dplyr::pull(alder) %>%
                           min())

  testthat::expect_equal(69,
                         utlede_aldersklasse(x, var = alder) %>%
                           dplyr::filter(aldersklasse == "60-69") %>%
                           dplyr::pull(alder) %>%
                           max())

  # sjekk feilmelding dersom var har feil format
  testthat::expect_error(utlede_aldersklasse(x, var = toto))
  testthat::expect_error(utlede_aldersklasse(x, var = as.character(alder)))
})



testthat::test_that("Utlede OppholdsID fungerer", {

  x <- data.frame(
    Regtype = c(rep("Primær", 4), "Sekundær", "Sekundær"),
    ForlopsID = c(1, 2, 3, 4, 5, 6),
    PrimaerForlopsID = c(rep(NA, 4), 1, 3))
  x_out <-  noric::utlede_OppholdsID(x)

  testthat::expect_equal(
    names(x_out),
    c("Regtype", "ForlopsID", "PrimaerForlopsID", "OppholdsID")
  )

  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$ForlopsID %in% c(1, 5)) %>%
      dplyr::pull(.data$OppholdsID) == 1))

  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$ForlopsID %in% c(3, 6)) %>%
      dplyr::pull(.data$OppholdsID) == 3))

  testthat:: expect_equal(
    x_out %>%
      dplyr::filter(.data$Regtype  == "Primær") %>%
      dplyr::pull(.data$OppholdsID),

    x_out %>%
      dplyr::filter(.data$Regtype  == "Primær") %>%
      dplyr::pull(.data$ForlopsID))


})


#  TESTER FUNKSONER FOR FERDISTILTE SKJEMA
test_that("Ferdigstilt skjemaStatus works", {

  x <- data.frame(SkjemaStatusStart = c(-1, 0, 1, NA))
  testthat::expect_equal(1,
                         x %>%
                           utlede_ferdigstilt(df = .,
                                              var = SkjemaStatusStart,
                                              suffix = "toto") %>%
                           dplyr::filter(is.na(ferdigstilt_toto)) %>%
                           nrow())

  testthat::expect_equal(1,
                         x %>%
                           utlede_ferdigstilt(df = .,
                                              var = SkjemaStatusStart,
                                              suffix = "toto") %>%
                           dplyr::filter(ferdigstilt_toto == "ja") %>%
                           nrow())

  testthat::expect_equal(2,
                         x %>%
                           utlede_ferdigstilt(df = .,
                                              var = SkjemaStatusStart,
                                              suffix = "toto") %>%
                           dplyr::filter(ferdigstilt_toto == "nei") %>%
                           nrow())

  testthat::expect_equal(c("SkjemaStatusStart", "ferdigstilt_testerNavn"),
                         x %>%
                           utlede_ferdigstilt(df = .,
                                              var = SkjemaStatusStart,
                                              suffix = "testerNavn") %>%
                           names())
})


