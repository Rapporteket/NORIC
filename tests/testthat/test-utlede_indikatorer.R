test_that("ki_ferdigstilt_komplikasjoner works", {

  x <- data.frame(SkjemaStatusKomplikasjoner = c(-1, 1, 0, NA, NA, NA))
  x_out <- noric::ki_ferdigstilt_komplikasjoner(df_ap = x)

  expect_equal(names(x_out),
               c("SkjemaStatusKomplikasjoner",
                 "ki_komplik_ferdig_dg",
                 "ki_komplik_ferdig"))

  expect_equal(
    x_out %>%
      dplyr::filter(is.na(.data$SkjemaStatusKomplikasjoner)) %>%
      dplyr::pull(.data$ki_komplik_ferdig_dg),
    c("nei", "nei", "nei"))

  expect_true(
    all(x_out %>%
          dplyr::filter(.data$ki_komplik_ferdig_dg == "nei") %>%
          dplyr::select(.data$ki_komplik_ferdig) %>%
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
      dplyr::filter(.data$Indikasjon == "Stabil koronarsykdom") %>%
      dplyr::pull(.data$ki_trykkmaaling_dg) == "ja"))

  expect_true(all(
    x_out %>%
      dplyr::filter(.data$Indikasjon != "Stabil koronarsykdom" |
                      is.na(.data$Indikasjon)) %>%
      dplyr::pull(.data$ki_trykkmaaling_dg) == "nei"))


  expect_true(
    all(x_out %>%
          dplyr::filter(.data$ki_trykkmaaling_dg == "nei") %>%
          dplyr::select(.data$ki_trykkmaaling) %>%
          is.na()))


  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_trykkmaaling_dg == "ja" & .data$FFR == "Ja") %>%
      dplyr::pull(.data$ki_trykkmaaling) == "ja"))


  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_trykkmaaling_dg == "ja" & .data$IFR == "Ja") %>%
      dplyr::pull(.data$ki_trykkmaaling) == "ja"))


  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_trykkmaaling_dg == "ja" &
                      (.data$FFR != "Ja" | is.na(.data$FFR)) &
                      (.data$IFR != "Ja" | is.na(.data$IFR))) %>%
      dplyr::pull(.data$ki_trykkmaaling) == "nei"))



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
      dplyr::filter(.data$ki_ivus_oct_ved_stenting_lms_dg == "ja") %>%
      dplyr::pull(.data$Indikasjon) %in% c("Vitieutredning",
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
      dplyr::filter(.data$ki_ivus_oct_ved_stenting_lms_dg == "ja") %>%
      dplyr::pull(.data$satt_inn_stent_i_LMS) == "ja"))

  # Forventer disse verdiene av TidlABC dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_ivus_oct_ved_stenting_lms_dg == "ja") %>%
      dplyr::pull(.data$TidlABC) %in% c("Ukjent", "Nei", NA)))



  # Forventer at datagrunnlag er nei, dersom disse indikasjonene
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Indikasjon %in% c("Vitieutredning",
                                             "Uklare brystsmerter",
                                             "Annet",
                                             "Hjertestans uten STEMI",
                                             "Hjertesvikt/kardiomyopati",
                                             "Komplettering av tidligere PCI",
                                             "UAP",
                                             "NSTEMI")) %>%
      dplyr::pull(.data$ki_ivus_oct_ved_stenting_lms_dg)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom disse verdiene av TidlABC
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$TidlABC %in% c("Nei", "Ukjent", NA)) %>%
      dplyr::pull(.data$ki_ivus_oct_ved_stenting_lms_dg)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom stent ikke satt inn
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$satt_inn_stent_i_LMS %in% c("ja")) %>%
      dplyr::pull(.data$ki_ivus_oct_ved_stenting_lms_dg)  == "nei"))



  # Forventer at KI er NA dersom ikke i datagrunnlag
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_ivus_oct_ved_stenting_lms_dg == "nei") %>%
      dplyr::pull(.data$ki_ivus_oct_ved_stenting_lms) %>%
      is.na()))


  # Forventer at KI er ja dersom i datagrunnlaget og en IVUS/OCT er uført
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_ivus_oct_ved_stenting_lms_dg == "ja",
                    .data$IVUS == "Ja") %>%
      dplyr::pull(.data$ki_ivus_oct_ved_stenting_lms) == "ja"))

  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_ivus_oct_ved_stenting_lms_dg == "ja",
                    .data$OCT == "Ja") %>%
      dplyr::pull(.data$ki_ivus_oct_ved_stenting_lms) == "ja"))


  # Forventer at KI er nei dersom i datagrunnlaget og ingen IVUS/OCT er uført
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_ivus_oct_ved_stenting_lms_dg == "ja" &
                      (.data$IVUS != "Ja" | is.na(.data$IVUS)) &
                      (.data$OCT != "Ja" | is.na(.data$OCT))) %>%
      dplyr::pull(.data$ki_ivus_oct_ved_stenting_lms) == "nei"))


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







test_that("ki_foreskr_blodfortynnende works", {

  x <- data.frame(

    antall_stent_under_opphold = c(NA, 0:18),
    Regtype = c("Primær", "Primær", "Sekundær", rep("Primær", 17)),
    SkjemaStatusUtskrivelse = c(rep(1, 3), -1, 0, rep(1, 15)),
    UtskrevetDod = c(rep("Nei", 5), "Ja", "Ukjent", NA,  rep("Nei", 12)),
    ASA = c(rep(c(NA, "Ja", "Nei", "Ukjent"), 2),
            rep("Ja", 4), rep(c("Nei", "Ukjent", NA, "Ja"), 2)),
    AndrePlatehemmere = c(rep(c(NA, "Annet", "Clopidogrel (Plavix)", "Nei"), 2),
                          rep(c("Annet", "Annet", "Nei", NA), 2),
                          "Nei", "Ukjent", NA, "Tiklopidin (Ticlid)"),
    Antikoagulantia = c(rep(c(NA, "Annet", "Lixiana", "Marevan"), 2),
                        rep(c("Annet", "Nei", "Exanta", NA), 2),
                        "Annet", "Ukjent", NA, "Exanta"))



  x_out <- noric::ki_foreskr_blodfortynnende(x)

  # Forventede kolonne-navn
  expect_equal(names(x_out),
               c("antall_stent_under_opphold",
                 "Regtype",
                 "SkjemaStatusUtskrivelse",
                 "UtskrevetDod",
                 "ASA",
                 "AndrePlatehemmere",
                 "Antikoagulantia",
                 "ki_foreskr_blodfortynnende_dg",
                 "ki_foreskr_blodfortynnende"))

  # Forventer postitivt antall stent dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_foreskr_blodfortynnende_dg == "ja") %>%
      dplyr::pull(.data$antall_stent_under_opphold) > 0))

  # Forventer primærforløp dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_foreskr_blodfortynnende_dg == "ja") %>%
      dplyr::pull(.data$Regtype) == "Primær"))

  # Forventer ferdigstilt dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_foreskr_blodfortynnende_dg == "ja") %>%
      dplyr::pull(.data$SkjemaStatusUtskrivelse)  == 1))

  # Forventer Ikke død dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_foreskr_blodfortynnende_dg == "ja") %>%
      dplyr::pull(.data$UtskrevetDod) %in% c("Nei")))


  # Forventer at datagrunnlag er nei, dersom ingen stent
  expect_true(all(
    x_out %>%
      dplyr::filter(is.na(.data$antall_stent_under_opphold) |
                      .data$antall_stent_under_opphold < 1) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende_dg)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom sekundærforløp
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Regtype == "Primær") %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende_dg)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom ikke ferdigstilt
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$SkjemaStatusUtskrivelse != 1) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende_dg)  == "nei"))


  # Forventer at datagrunnlag er nei, dersom ikke levende
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$UtskrevetDod %in% c("Ja", "Ukjent", NA)) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende_dg)  == "nei"))



  # Forventer at KI er NA dersom ikke i datagrunnlag
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_foreskr_blodfortynnende_dg == "nei") %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende) %>%
      is.na()))


  # Forventer at KI er ja dersom i datagrunnlaget og anbefalt kombinasjon av
  # medikamenter er greit
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          .data$ASA == "Ja" &
          !.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende) == "ja"))

  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          .data$ASA == "Ja" &
          !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende) == "ja"))

  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          (!.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
             !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_))) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende) == "ja"))


  # Forventer at KI er NEI dersom i datagrunnlaget men anbefalt kombinasjon av
  # medikamenter ikke er foreskrevet
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          .data$ASA == "Ja" &
          .data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
          .data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende) == "nei"))


  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          .data$ASA != "Ja" &
          !.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
          .data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende) == "nei"))


  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          .data$ASA != "Ja" &
          .data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
          !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende) == "nei"))


  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          .data$ASA != "Ja" &
          .data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
          .data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende) == "nei"))


  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$ki_foreskr_blodfortynnende_dg == "ja" &
          (!.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
             !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_))) %>%
      dplyr::pull(.data$ki_foreskr_blodfortynnende) == "ja"))


  # Forventer feilmelding dersom variabler mangler
  expect_error(
    noric::ki_foreskr_blodfortynnende(
      df_ap = data.frame(tullenavn = c(1, 1, 1))))

  # Forventer feilmelding dersom feil kolonnenavn
  expect_error(
    noric::ki_foreskr_blodfortynnende(
      df_ap = data.frame(antall_stent_under_opphold = 1,
                         Regtype = "Primær",
                         UtskrevetDod = "Nei",
                         SkjemaStatusUtskrivelse = 1,
                         ASA = "Ja",
                         AndrePlatehemmere = "Annet")))

})







test_that("ki_foreskr_kolesterolsenkende works", {

  x <- data.frame(

    antall_stent_under_opphold = c(NA, 0:18),
    Regtype = c("Primær", "Primær", "Sekundær", rep("Primær", 17)),
    SkjemaStatusUtskrivelse = c(rep(1, 3), -1, 0, rep(1, 15)),
    UtskrevetDod = c(rep("Nei", 5), "Ja", "Ukjent", NA,  rep("Nei", 12)),
    UtskrStatiner = c(rep(c(NA, "Ja", "Nei", "Ukjent"), 2),
                      rep("Ja", 4), rep(c("Nei", "Ukjent", NA, "Ja"), 2)))



  x_out <- noric::ki_foreskr_kolesterolsenkende(x)

  # Forventede kolonne-navn
  expect_equal(names(x_out),
               c("antall_stent_under_opphold",
                 "Regtype",
                 "SkjemaStatusUtskrivelse",
                 "UtskrevetDod",
                 "UtskrStatiner",
                 "ki_foreskr_kolesterolsenkende_dg",
                 "ki_foreskr_kolesterolsenkende"))

  # Forventer postitivt antall stent dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_foreskr_kolesterolsenkende_dg == "ja") %>%
      dplyr::pull(.data$antall_stent_under_opphold) > 0))

  # Forventer primærforløp dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_foreskr_kolesterolsenkende_dg == "ja") %>%
      dplyr::pull(.data$Regtype) == "Primær"))

  # Forventer ferdigstilt dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_foreskr_kolesterolsenkende_dg == "ja") %>%
      dplyr::pull(.data$SkjemaStatusUtskrivelse)  == 1))

  # Forventer Ikke død dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_foreskr_kolesterolsenkende_dg == "ja") %>%
      dplyr::pull(.data$UtskrevetDod) %in% c("Nei")))


  # Forventer at datagrunnlag er nei, dersom ingen stent
  expect_true(all(
    x_out %>%
      dplyr::filter(is.na(.data$antall_stent_under_opphold) |
                      .data$antall_stent_under_opphold < 1) %>%
      dplyr::pull(.data$ki_foreskr_kolesterolsenkende_dg)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom sekundærforløp
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Regtype == "Primær") %>%
      dplyr::pull(.data$ki_foreskr_kolesterolsenkende_dg)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom ikke ferdigstilt
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$SkjemaStatusUtskrivelse != 1) %>%
      dplyr::pull(.data$ki_foreskr_kolesterolsenkende_dg)  == "nei"))


  # Forventer at datagrunnlag er nei, dersom ikke levende
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$UtskrevetDod %in% c("Ja", "Ukjent", NA)) %>%
      dplyr::pull(.data$ki_foreskr_kolesterolsenkende_dg)  == "nei"))



  # Forventer at KI er NA dersom ikke i datagrunnlag
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ki_foreskr_kolesterolsenkende_dg == "nei") %>%
      dplyr::pull(.data$ki_foreskr_kolesterolsenkende) %>%
      is.na()))


  # Forventer at KI er ja dersom i datagrunnlaget og utskrevet statiner
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$ki_foreskr_kolesterolsenkende_dg == "ja" &
          .data$UtskrStatiner == "Ja") %>%
      dplyr::pull(.data$ki_foreskr_kolesterolsenkende) == "ja"))

  # Forventer at KI er nei dersom i datagrunnlaget og ikke utskrevet statiner
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$ki_foreskr_kolesterolsenkende_dg == "ja" &
          ! .data$UtskrStatiner == "Ja") %>%
      dplyr::pull(.data$ki_foreskr_kolesterolsenkende) == "nei"))

  # Forventer feilmelding dersom variabler mangler
  expect_error(
    noric::ki_foreskr_kolesterolsenkende(
      df_ap = data.frame(tullenavn = c(1, 1, 1))))

  # Forventer feilmelding dersom feil kolonnenavn
  expect_error(
    noric::ki_foreskr_kolesterolsenkende(
      df_ap = data.frame(antall_stent_under_opphold = 1,
                         Regtype = "Primær",
                         UtskrevetDod = "Nei",
                         SkjemaStatusUtskrivelse = 1,
                         UtskrSTATINER = "Ja")))

})

