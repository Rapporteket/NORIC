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
                  var4 = 1:7)


  testthat::expect_equal(c(7,5),
                         dim(slaa_sammen_variabler(df = x, var1 = var1, var2 = var2,
                                                   var_name = "ny")))

  testthat::expect_equal(c(7,3),
                         dim(slaa_sammen_variabler(df = x, var1 = var1, var2 = var2,
                                                   var_name = "ny",
                                                   slette_gamle = TRUE)))

  testthat::expect_equal(c("var1", "var2" , "ny", "var3", "var4"),
                         names(slaa_sammen_variabler(df = x, var1 = var1, var2 = var2,
                                                     var_name = "ny")))

  testthat::expect_equal(
    c(rep("A", 5), "B"),
    slaa_sammen_variabler(df = x, var1 = var1, var2 = var2,
                          var_name = "ny", slette_gamle = TRUE)[1:6, "ny"])

  testthat::expect_true(is.na(slaa_sammen_variabler(df = x, var1 = var1, var2 = var2,
                                                    var_name = "ny",
                                                    slette_gamle = FALSE)[7, "ny"]))


  testthat::expect_error(
    slaa_sammen_variabler(df = x, var1 = var3, var2 = var1,
                          var_name = "ny", slette_gamle = TRUE)[1:7, "ny"])

  testthat::expect_error(
    slaa_sammen_variabler(df = x, var1 = var4, var2 = var1,
                          var_name = "ny", slette_gamle = TRUE)[1:7, "ny"])

})
