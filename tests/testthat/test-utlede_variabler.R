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



testthat::test_that("Død under NORIC-forløpet fungerer", {
  
  
  x <- data.frame(LabKompDod = c(rep("Ja", 18),
                                 rep("Nei", 18), 
                                 rep("Ukjent",18), 
                                 rep(NA_character_, 18)), 
                  AvdKompDod = rep(c(rep("Ja", 6), 
                                     rep("Nei", 6), 
                                     rep(NA_character_, 6)),
                                   4),
                  UtskrevetDod = rep(c("Ja", "Ja",  
                                       "Nei", "Nei",
                                       NA_character_, NA_character_), 12), 
                  UtskrevetDodsdato = rep(as.Date(c("2021-01-01", 
                                                    NA_character_), 
                                                  format = "%Y-%m-%d"), 
                                          36))
  
  x_out <- noric::utlede_dod_noric(x)
  
  
  testthat::expect_equal(
    object = names(x_out), 
    expected = c("LabKompDod", "AvdKompDod",
                 "UtskrevetDod", "UtskrevetDodsdato", "dod_noric")
  )
  
  
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$LabKompDod %in% "Ja") %>% 
      dplyr::pull(.data$dod_noric) == "Ja"))
  
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$AvdKompDod %in% "Ja") %>% 
      dplyr::pull(.data$dod_noric) == "Ja"))
  
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$UtskrevetDod %in% "Ja") %>% 
      dplyr::pull(.data$dod_noric) == "Ja"))
  
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$UtskrevetDodsdato %in% 
                      as.Date("2021-01-01", format = "%Y-%m-%d")) %>% 
      dplyr::pull(.data$dod_noric) == "Ja"))
  
  
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(! .data$LabKompDod %in% "Ja", 
                    ! .data$AvdKompDod %in% "Ja", 
                    ! .data$UtskrevetDod %in% "Ja", 
                    is.na(.data$UtskrevetDodsdato)) %>% 
      dplyr::pull(.data$dod_noric) == "Nei"))
  
  testthat::expect_true(
    x_out %>%
      dplyr::filter(.data$dod_noric %in% "Nei") %>% 
      nrow() == 12)
  
  testthat::expect_true(
    x_out %>%
      dplyr::filter(.data$dod_noric %in% "Ja") %>% 
      nrow() == 60)
  
  testthat::expect_equal(
    object = x_out %>% 
      dplyr::select(.data$dod_noric) %>% 
      dplyr::distinct() %>% 
      dplyr::pull(),
    
    expected = c("Ja", "Nei"))
  
  
})


testthat::test_that("Død under NORIC-oppholdet fungerer", {
  
  x <- data.frame(AvdRESH = rep(123456, 8), 
                  OppholdsID = c(1, 1, 1, 2, 3, 3, 4, 5), 
                  dod_noric = c("Nei", "Nei", "Ja", "Nei", 
                                "Nei", "Nei", "Ja", "Nei"))
  x_out <- noric::avdod_opphold(x)   
  
  testthat::expect_equal(
    object = names(x_out), 
    expected = c("AvdRESH", "OppholdsID", "dod_noric", "dod_opphold"))
  
  testthat::expect_equal(
    object = x_out %>% 
      dplyr::select(dod_opphold) %>% 
      dplyr::distinct() %>% 
      dplyr::pull(), 
    expected = c("Ja", "Nei"))
  
  testthat::expect_true(all(
    x_out %>% 
      dplyr::filter(.data$OppholdsID %in% c(1, 4)) %>% 
      dplyr::pull(.data$dod_opphold) == "Ja"))
  
  testthat::expect_true(all(
    x_out %>% 
      dplyr::filter(.data$OppholdsID %in% c(2, 3, 5)) %>% 
      dplyr::pull(.data$dod_opphold) == "Nei"))
  
  
})
  
testthat::test_that("Tester at funksjonene samhandler", {
    
    x <- data.frame(AvdRESH = rep(123456, 6), 
                    ForlopsID = 1:6, 
                    Regtype = c(rep("Primær", 4), "Sekundær", "Sekundær"), 
                    PrimaerForlopsID = c(1:4, 4, 4), 
                    LabKompDod = c("Ja", rep("Nei", 4), "Ja"), 
                    AvdKompDod = c("Nei", "Nei", "Ja", "Nei", 
                                   NA_character_, NA_character_), 
                    UtskrevetDod = c(rep("Nei", 4), 
                                     NA_character_, NA_character_),
                    UtskrevetDodsdato = rep(NA_character_, 6))
    x_out <- x %>%  
      noric::utlede_OppholdsID(.) %>% 
      noric::utlede_dod_noric(.) %>% 
      noric::avdod_opphold(.)                
    
    

    testthat::expect_equal(
      object = names(x_out), 
      expected = c("AvdRESH", "ForlopsID", "Regtype", "PrimaerForlopsID", 
                   "LabKompDod", "AvdKompDod", 
                   "UtskrevetDod", "UtskrevetDodsdato", 
                   "OppholdsID", "dod_noric", "dod_opphold"))
    
    
    testthat::expect_true(all(
      x_out %>% 
        dplyr::filter(.data$ForlopsID %in% c(1, 3, 6)) %>% 
        dplyr::pull(.data$dod_noric) == "Ja"))

    testthat::expect_true(all(
      x_out %>% 
        dplyr::filter(! .data$ForlopsID %in% c(1, 3, 6)) %>% 
        dplyr::pull(.data$dod_noric) == "Nei"))
    
    
    testthat::expect_true(all(
      x_out %>% 
        dplyr::filter(.data$OppholdsID %in% c(1, 3, 4)) %>% 
        dplyr::pull(.data$dod_opphold) == "Ja"))
    
    testthat::expect_true(all(
      x_out %>% 
        dplyr::filter(! .data$OppholdsID %in% c(1, 3, 4)) %>% 
        dplyr::pull(.data$dod_opphold) == "Nei"))
    
  })
  