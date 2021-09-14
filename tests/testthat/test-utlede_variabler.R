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



test_that("slas_sammen_variabler() works", {

  x <- data.frame(var1 = c(rep("A", 5), NA, NA),
                  var2 = c(NA, rep("B", 5), NA),
                  var3 = c(1, NA, 2, NA, 3:5),
                  var4 = 1:7,
                  var5 = as.Date(c("2020-01-10",
                                   "1998-12-24",
                                   "1780-06-01",
                                   NA_character_,
                                   NA_character_,
                                   NA_character_,
                                   "1998-05-05"), format = "%Y-%m-%d"),
                  var6 = as.Date(c("2020-01-10",
                                   NA_character_,
                                   NA_character_,
                                   NA_character_,
                                   "1971-11-15",
                                   "2000-08-28",
                                   "1998-05-05"), format = "%Y-%m-%d"))


  testthat::expect_equal(c(7,7),
                         dim(slaa_sammen_variabler(df = x,
                                                   var1 = var1,
                                                   var2 = var2,
                                                   var_name = "ny")))

  testthat::expect_equal(c(7,5),
                         dim(slaa_sammen_variabler(df = x,
                                                   var1 = var1,
                                                   var2 = var2,
                                                   var_name = "ny",
                                                   slette_gamle = TRUE)))

  testthat::expect_equal(c("var1", "var2" , "ny",
                           "var3", "var4", "var5", "var6"),
                         names(slaa_sammen_variabler(df = x,
                                                     var1 = var1,
                                                     var2 = var2,
                                                     var_name = "ny")))

  testthat::expect_equal(
    c(rep("A", 5), "B"),
    slaa_sammen_variabler(df = x,
                          var1 = var1,
                          var2 = var2,
                          var_name = "ny",
                          slette_gamle = TRUE)[1:6, "ny"])

  testthat::expect_true(is.na(
    slaa_sammen_variabler(df = x,
                          var1 = var1,
                          var2 = var2,
                          var_name = "ny",
                          slette_gamle = FALSE)[7, "ny"]))


  testthat::expect_error(
    slaa_sammen_variabler(df = x,
                          var1 = var3,
                          var2 = var1,
                          var_name = "ny",
                          slette_gamle = TRUE))

  testthat::expect_error(
    slaa_sammen_variabler(df = x,
                          var1 = var4,
                          var2 = var1,
                          var_name = "ny",
                          slette_gamle = TRUE))



  testthat::expect_error(
    slaa_sammen_variabler(df = x,
                          var1 = var5,
                          var2 = var1,
                          var_name = "ny",
                          slette_gamle = TRUE))


  testthat::expect_equal(
    as.Date(c("2020-01-10",
              "1998-12-24",
              "1780-06-01",
              NA_character_,
              "1971-11-15",
              "2000-08-28",
              "1998-05-05"), format = "%Y-%m-%d"),
    noric::slaa_sammen_variabler(df = x,
                                 var1 = var5,
                                 var2 = var6,
                                 var_name = "ny",
                                 slette_gamle = TRUE)[1:7,"ny"])

})
