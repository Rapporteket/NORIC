test_that("Utlede aldersklasser", {
  x <- data.frame(alder = c(-1,10,17,18,20,
                            59,60,61,69, 70,
                            99,100,NA, NA, NA))

  # sjekk at de som skal bli NA blir det:
  expect_equal(7,
               utlede_aldersklasse(x, var = alder) %>%
                 filter(is.na(aldersklasse)) %>%
                 nrow())

  # Sjekk at øvre/nedre grense for aldersklasse er er riktig
  expect_equal(60,
               utlede_aldersklasse(x, var = alder) %>%
                 filter(aldersklasse == "60-69") %>%
                 pull(alder) %>% min())

  expect_equal(69,
               utlede_aldersklasse(x, var = alder) %>%
                 filter(aldersklasse == "60-69") %>%
                 pull(alder) %>% max())

  # sjekk feilmelding dersom var har feil format
  expect_error(utlede_aldersklasse(x, var = toto))
  expect_error(utlede_aldersklasse(x, var = as.character(alder)))
  })


#  TESTER FUNKSONER FOR FERDISTILTE SKJEMA
test_that("Ferdigstilt skjemaStatus works",{

  x = data.frame(SkjemaStatusStart = c(-1,0,1, NA))
  expect_equal(1,
               x %>%
                 utlede_ferdigstilt(df = .,
                                    var = SkjemaStatusStart,
                                    suffix = "toto") %>%
                 filter(is.na(ferdigstilt_toto)) %>%
                 nrow())

  expect_equal(1,
               x %>%
                 utlede_ferdigstilt(df = .,
                                    var = SkjemaStatusStart,
                                    suffix = "toto") %>%
                 filter(ferdigstilt_toto == "ja") %>%
                 nrow())

  expect_equal(2,
               x %>%
                 utlede_ferdigstilt(df = .,
                                    var = SkjemaStatusStart,
                                    suffix = "toto") %>%
                 filter(ferdigstilt_toto == "nei") %>%
                 nrow())

  expect_equal(c("SkjemaStatusStart", "ferdigstilt_testerNavn"),
               x %>%
                 utlede_ferdigstilt(df = .,
                                    var = SkjemaStatusStart,
                                    suffix = "testerNavn") %>%
                 names())



})
