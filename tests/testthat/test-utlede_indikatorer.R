test_that("ki_ferdigstilt_komplikasjoner works", {

  x <- data.frame(SkjemaStatusKomplikasjoner = c(-1, 1, 0, NA, NA, NA))
  x_out <- noric::ki_ferdigstilt_komplikasjoner(df_ap = x)

  expect_equal(names(x_out),
               c("SkjemaStatusKomplikasjoner",
                 "ki_komplik_ferdig_dg",
                 "ki_komplik_ferdig"))

  expect_equal(
    x_out %>%
          dplyr::filter(is.na(SkjemaStatusKomplikasjoner)) %>%
          dplyr::pull(ki_komplik_ferdig_dg),
    c("nei", "nei", "nei"))

  expect_true(
    all(x_out %>%
          dplyr::filter(ki_komplik_ferdig_dg == "nei") %>%
          dplyr::select(ki_komplik_ferdig) %>%
          is.na()))


  expect_error(
    noric::ki_ferdigstilt_komplikasjoner(
      df_ap = data.frame(tullenavn = c(1, 1, 1))))
})



test_that("ki_trykkmaaling_utfoert works", {

  x <- data.frame(
    Indikasjon = c(rep("Stabil koronarsykdom", 6), NA, NA, "Annet"),
    FFR = c(NA, "Ja", "Ja", NA, "Ja", "Ukjent", "Nei","Ja", "Ja"),
    IFR = c(NA, "Ja", "Nei","Ja", "Ukjent", NA, NA,"Ja", NA))

x_out <- noric::ki_trykkmaaling_utfoert(df_ap = x)

  expect_equal(names(x_out),
               c("Indikasjon",
                 "FFR",
                 "IFR",
                 "ki_trykkmaaling_dg",
                 "ki_trykkmaaling"))

  expect_true(all(
    x_out %>%
      dplyr::filter(Indikasjon == "Stabil koronarsykdom") %>%
      dplyr::pull(ki_trykkmaaling_dg) == "ja"))

  expect_true(all(
    x_out %>%
      dplyr::filter(Indikasjon != "Stabil koronarsykdom" |
                    is.na(Indikasjon)) %>%
      dplyr::pull(ki_trykkmaaling_dg) == "nei"))


  expect_true(
    all(x_out %>%
          dplyr::filter(ki_trykkmaaling_dg == "nei") %>%
          dplyr::select(ki_trykkmaaling) %>%
          is.na()))


  expect_true(all(
    x_out %>%
      dplyr::filter(ki_trykkmaaling_dg == "ja" & FFR == "Ja") %>%
      dplyr::pull(ki_trykkmaaling) == "ja"))


  expect_true(all(
    x_out %>%
      dplyr::filter(ki_trykkmaaling_dg == "ja" & IFR == "Ja") %>%
      dplyr::pull(ki_trykkmaaling) == "ja"))


  expect_true(all(
    x_out %>%
      dplyr::filter(ki_trykkmaaling_dg == "ja" &
                      (FFR != "Ja" | is.na(FFR)) &
                      (IFR != "Ja" | is.na(IFR))) %>%
      dplyr::pull(ki_trykkmaaling) == "nei"))



  expect_error(
    noric::ki_trykkmaaling_utfoert(
      df_ap = data.frame(tullenavn = c(1, 1, 1))))

  expect_error(
    noric::ki_trykkmaaling_utfoert(
      df_ap = data.frame(Indikasjon = "Stabil koronarsykdom",
                         FFR = "Ja")))
})
