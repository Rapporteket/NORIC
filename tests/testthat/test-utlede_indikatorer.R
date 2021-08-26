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
    FFR = c(NA, "Ja", "Ja", NA, "Ja", "Ukjent", "Nei", "Ja", "Ja"),
    IFR = c(NA, "Ja", "Nei", "Ja", "Ukjent", NA, NA, "Ja", NA))

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





test_that("ki_ivus_oct_ved_stenting_lms works", {

  ap_test <- data.frame(
    AvdRESH = rep(1, 30),
    ForlopsID = 1:30,
    Indikasjon = rep(c("Vitieutredning",
                         "Uklare brystsmerter",
                         "Annet",
                         "Hjertestans uten STEMI",
                         "Hjertesvikt/kardiomyopati",
                         "Komplettering av tidligere PCI",
                         "UAP",
                         "NSTEMI",
                         "NoeTull!",
                         NA_character_), 3),
    TidlABC = c(rep("Ja", 10),
                rep(c("Nei", "Ukjent", NA_character_), 6),
                NA_character_, NA_character_),
    IVUS = rep(c("Ja", "Ja", "Nei", "Ukjent", NA_character_), 6),
    OCT = rep(c("Nei", "Ja", "Nei", "Ukjent", NA_character_), 6))


  ss_test <- data.frame(
    AvdRESH = rep(1, 10),
    ForlopsID = c(1:3, 10:13, 20:22),
    Segment = c(1:5, 5, 5, 5, 10, 40),
    Graft = rep("Nei", 10),
    StentType = c(rep(NA_character_, 3), rep("A", 4), rep("B", 3))
  )


  x_out <- ap_test %<>%
    noric::satt_inn_stent_i_lms(., df_ss = ss_test) %>%
    noric::ki_ivus_oct_ved_stenting_lms()

  # Forventede kolonne-navn
  expect_equal(names(x_out),
               c("AvdRESH",
                 "ForlopsID",
                 "Indikasjon",
                 "TidlABC",
                 "IVUS",
                 "OCT",
                 "satt_inn_stent_i_LMS",
                 "ki_ivus_oct_ved_stenting_lms_dg",
                 "ki_ivus_oct_ved_stenting_lms"))

  # Forventer disse indikasjonene dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(ki_ivus_oct_ved_stenting_lms_dg == "ja") %>%
      dplyr::pull(Indikasjon) %in% c("Vitieutredning",
                                     "Uklare brystsmerter",
                                     "Annet",
                                     "Hjertestans uten STEMI",
                                     "Hjertesvikt/kardiomyopati",
                                     "Komplettering av tidligere PCI",
                                     "UAP",
                                     "NSTEMI")))

  # Forventer at stent satt inn i LMS dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(ki_ivus_oct_ved_stenting_lms_dg == "ja") %>%
      dplyr::pull(satt_inn_stent_i_LMS) == "ja"))

  # Forventer disse verdiene av TidlABC dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(ki_ivus_oct_ved_stenting_lms_dg == "ja") %>%
      dplyr::pull(TidlABC) %in% c("Ukjent", "Nei", NA)))



  # Forventer at datagrunnlag er nei, dersom disse indikasjonene
  expect_true(all(
    x_out %>%
      dplyr::filter(!Indikasjon %in% c("Vitieutredning",
                                     "Uklare brystsmerter",
                                     "Annet",
                                     "Hjertestans uten STEMI",
                                     "Hjertesvikt/kardiomyopati",
                                     "Komplettering av tidligere PCI",
                                     "UAP",
                                     "NSTEMI")) %>%
      dplyr::pull(ki_ivus_oct_ved_stenting_lms_dg)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom disse verdiene av TidlABC
  expect_true(all(
    x_out %>%
      dplyr::filter(!TidlABC %in% c("Nei", "Ukjent", NA)) %>%
      dplyr::pull(ki_ivus_oct_ved_stenting_lms_dg)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom stent ikke satt inn
  expect_true(all(
    x_out %>%
      dplyr::filter(!satt_inn_stent_i_LMS %in% c("ja")) %>%
      dplyr::pull(ki_ivus_oct_ved_stenting_lms_dg)  == "nei"))



  # Forventer at KI er NA dersom ikke i datagrunnlag
  expect_true(all(
    x_out %>%
      dplyr::filter(ki_ivus_oct_ved_stenting_lms_dg == "nei") %>%
      dplyr::pull(ki_ivus_oct_ved_stenting_lms) %>%
      is.na()))


  # Forventer at KI er ja dersom i datagrunnlaget og en IVUS/OCT er uført
  expect_true(all(
    x_out %>%
      dplyr::filter(ki_ivus_oct_ved_stenting_lms_dg == "ja",
                    IVUS == "Ja") %>%
      dplyr::pull(ki_ivus_oct_ved_stenting_lms) == "ja"))

  expect_true(all(
    x_out %>%
      dplyr::filter(ki_ivus_oct_ved_stenting_lms_dg == "ja",
                    OCT == "Ja") %>%
      dplyr::pull(ki_ivus_oct_ved_stenting_lms) == "ja"))


  # Forventer at KI er nei dersom i datagrunnlaget og ingen IVUS/OCT er uført
  expect_true(all(
    x_out %>%
      dplyr::filter(ki_ivus_oct_ved_stenting_lms_dg == "ja" &
                   (IVUS != "Ja" | is.na(IVUS)) &
                   (OCT != "Ja" | is.na(OCT))) %>%
      dplyr::pull(ki_ivus_oct_ved_stenting_lms) == "nei"))


# Forventer feilmelding dersom variabler mangler
  expect_error(
    noric::ki_ivus_oct_ved_stenting_lms(
      df_ap = data.frame(tullenavn = c(1, 1, 1))))

  # Forventer feilmelding dersom feil kolonnenavn
  expect_error(
    noric::ki_ivus_oct_ved_stenting_lms(
      df_ap = data.frame(Indikasjon = "Stabil koronarsykdom",
                         satt_inn_stent_i_LMS = "ja",
                         TidlABC = "Nei",
                         IVUS = "Ja",
                         OOCT = "Nei")))
})
