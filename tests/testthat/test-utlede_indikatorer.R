testthat::test_that("ki_ferdigstilt_komplikasjoner works", {

  x <- data.frame(SkjemaStatusKomplikasjoner = c(-1, 1, 0, NA, NA, NA))
  x_out <- noric::ki_ferdigstilt_komplikasjoner(df_ap = x)

  testthat::expect_equal(names(x_out),
                         c("SkjemaStatusKomplikasjoner",
                           "indik_komplik_ferdig_data",
                           "indik_komplik_ferdig"))

  testthat::expect_equal(
    x_out %>%
      dplyr::filter(is.na(.data$SkjemaStatusKomplikasjoner)) %>%
      dplyr::pull(.data$indik_komplik_ferdig_data),
    c("nei", "nei", "nei"))

  testthat::expect_true(
    all(x_out %>%
          dplyr::filter(.data$indik_komplik_ferdig_data == "nei") %>%
          dplyr::select(.data$indik_komplik_ferdig) %>%
          is.na()))


  testthat::expect_error(
    noric::ki_ferdigstilt_komplikasjoner(
      df_ap = data.frame(tullenavn = c(1, 1, 1))))
})



testthat::test_that("ki_trykkmaaling_utfoert works", {

  x <- data.frame(
    Indikasjon = c(rep("Stabil koronarsykdom", 6), NA, NA, "Annet",
                   rep("Stabil koronarsykdom", 6)),
    FFR = c(NA, "Ja", "Ja", NA, "Ja", "Ukjent", "Nei", "Ja", "Ja", rep(NA, 6)),
    IFR = c(NA, "Ja", "Nei", "Ja", "Ukjent", NA, NA, "Ja", NA,  rep(NA, 6)),
    IMR = c(rep(NA, 9),  "Ja","Ja", "Ja", "Ukjent", "Nei", "Nei"),
    PdPa = c(rep(NA, 9), "Ja","Ja", "Ja" , "Ukjent", "Nei", "Nei"),
    Pa = c(rep(NA, 9),   "Ja","Ja", "Nei" , "Ukjent","Nei", "Nei"),
    Pd = c(rep(NA, 9),   "Ja", "Nei", "Nei", "Ukjent", "Ja", NA))


  x_out <- noric::ki_trykkmaaling_utfoert(df_ap = x)

  testthat::expect_equal(
    names(x_out),
    c("Indikasjon",
      "FFR",
      "IFR",
      "IMR", "PdPa", "Pa", "Pd",
      "indik_trykkmaaling_data",
      "indik_trykkmaaling"))

  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$Indikasjon == "Stabil koronarsykdom") %>%
      dplyr::pull(.data$indik_trykkmaaling_data) == "ja"))

  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$Indikasjon != "Stabil koronarsykdom" |
                      is.na(.data$Indikasjon)) %>%
      dplyr::pull(.data$indik_trykkmaaling_data) == "nei"))


  testthat::expect_true(
    all(x_out %>%
          dplyr::filter(.data$indik_trykkmaaling_data == "nei") %>%
          dplyr::select(.data$indik_trykkmaaling) %>%
          is.na()))


  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_trykkmaaling_data == "ja" & .data$FFR == "Ja") %>%
      dplyr::pull(.data$indik_trykkmaaling) == "ja"))


  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_trykkmaaling_data == "ja" & .data$IFR == "Ja") %>%
      dplyr::pull(.data$indik_trykkmaaling) == "ja"))


  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_trykkmaaling_data == "ja" & .data$IMR == "Ja") %>%
      dplyr::pull(.data$indik_trykkmaaling) == "ja"))


  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_trykkmaaling_data == "ja" & .data$PdPa == "Ja") %>%
      dplyr::pull(.data$indik_trykkmaaling) == "ja"))

  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_trykkmaaling_data == "ja" & .data$Pa == "Ja") %>%
      dplyr::pull(.data$indik_trykkmaaling) == "ja"))


  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_trykkmaaling_data == "ja" & .data$Pd == "Ja") %>%
      dplyr::pull(.data$indik_trykkmaaling) == "ja"))


  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_trykkmaaling_data == "ja" &
                      (.data$FFR != "Ja" | is.na(.data$FFR)) &
                      (.data$IFR != "Ja" | is.na(.data$IFR)) &
                      (.data$IMR != "Ja" | is.na(.data$IMR)) &
                      (.data$PdPa != "Ja" | is.na(.data$PdPa)) &
                      (.data$Pa!= "Ja" | is.na(.data$Pa)) &
                      (.data$Pd != "Ja" | is.na(.data$Pd))) %>%
      dplyr::pull(.data$indik_trykkmaaling) == "nei"))



  testthat::expect_error(
    noric::ki_trykkmaaling_utfoert(
      df_ap = data.frame(tullenavn = c(1, 1, 1))))


  testthat::expect_error(
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
                 "indik_ivus_oct_v_stent_lms_data",
                 "indik_ivus_oct_v_stent_lms"))

  # Forventer disse indikasjonene dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_ivus_oct_v_stent_lms_data == "ja") %>%
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
      dplyr::filter(.data$indik_ivus_oct_v_stent_lms_data == "ja") %>%
      dplyr::pull(.data$satt_inn_stent_i_LMS) == "ja"))

  # Forventer disse verdiene av TidlABC dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_ivus_oct_v_stent_lms_data == "ja") %>%
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
      dplyr::pull(.data$indik_ivus_oct_v_stent_lms_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom disse verdiene av TidlABC
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$TidlABC %in% c("Nei", "Ukjent", NA)) %>%
      dplyr::pull(.data$indik_ivus_oct_v_stent_lms_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom stent ikke satt inn
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$satt_inn_stent_i_LMS %in% c("ja")) %>%
      dplyr::pull(.data$indik_ivus_oct_v_stent_lms_data)  == "nei"))



  # Forventer at KI er NA dersom ikke i datagrunnlag
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_ivus_oct_v_stent_lms_data == "nei") %>%
      dplyr::pull(.data$indik_ivus_oct_v_stent_lms) %>%
      is.na()))


  # Forventer at KI er ja dersom i datagrunnlaget og en IVUS/OCT er uført
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_ivus_oct_v_stent_lms_data == "ja",
                    .data$IVUS == "Ja") %>%
      dplyr::pull(.data$indik_ivus_oct_v_stent_lms) == "ja"))

  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_ivus_oct_v_stent_lms_data == "ja",
                    .data$OCT == "Ja") %>%
      dplyr::pull(.data$indik_ivus_oct_v_stent_lms) == "ja"))


  # Forventer at KI er nei dersom i datagrunnlaget og ingen IVUS/OCT er uført
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_ivus_oct_v_stent_lms_data == "ja" &
                      (.data$IVUS != "Ja" | is.na(.data$IVUS)) &
                      (.data$OCT != "Ja" | is.na(.data$OCT))) %>%
      dplyr::pull(.data$indik_ivus_oct_v_stent_lms) == "nei"))


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
                 "indik_blodfortynnende_data",
                 "indik_blodfortynnende"))

  # Forventer postitivt antall stent dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_blodfortynnende_data == "ja") %>%
      dplyr::pull(.data$antall_stent_under_opphold) > 0))

  # Forventer primærforløp dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_blodfortynnende_data == "ja") %>%
      dplyr::pull(.data$Regtype) == "Primær"))


  # Forventer Ikke død dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_blodfortynnende_data == "ja") %>%
      dplyr::pull(.data$UtskrevetDod) %in% c("Nei", NA_character_)))


  # Forventer at datagrunnlag er nei, dersom ingen stent
  expect_true(all(
    x_out %>%
      dplyr::filter(is.na(.data$antall_stent_under_opphold) |
                      .data$antall_stent_under_opphold < 1) %>%
      dplyr::pull(.data$indik_blodfortynnende_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom sekundærforløp
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Regtype == "Primær") %>%
      dplyr::pull(.data$indik_blodfortynnende_data)  == "nei"))



  # Forventer at datagrunnlag er nei, dersom ikke levende
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$UtskrevetDod %in% c("Ja", "Ukjent")) %>%
      dplyr::pull(.data$indik_blodfortynnende_data)  == "nei"))



  # Forventer at KI er NA dersom ikke i datagrunnlag
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_blodfortynnende_data == "nei") %>%
      dplyr::pull(.data$indik_blodfortynnende) %>%
      is.na()))


  # Forventer at KI er ja dersom i datagrunnlaget og anbefalt kombinasjon av
  # medikamenter er greit
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_blodfortynnende_data == "ja" &
          .data$SkjemaStatusUtskrivelse == 1 &
          .data$ASA == "Ja" &
          !.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$indik_blodfortynnende) == "ja"))

  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_blodfortynnende_data == "ja" &
          .data$SkjemaStatusUtskrivelse == 1 &
          .data$ASA == "Ja" &
          !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$indik_blodfortynnende) == "ja"))

  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_blodfortynnende_data == "ja" &
          .data$SkjemaStatusUtskrivelse == 1 &
          (!.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
             !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_))) %>%
      dplyr::pull(.data$indik_blodfortynnende) == "ja"))


  # Forventer at KI er NEI dersom i datagrunnlaget men anbefalt kombinasjon av
  # medikamenter ikke er foreskrevet
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_blodfortynnende_data == "ja" &
          .data$SkjemaStatusUtskrivelse == 1 &
          .data$ASA == "Ja" &
          .data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
          .data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$indik_blodfortynnende) == "nei"))


  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_blodfortynnende_data == "ja" &
          .data$SkjemaStatusUtskrivelse == 1 &
          .data$ASA != "Ja" &
          !.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
          .data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$indik_blodfortynnende) == "nei"))


  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_blodfortynnende_data == "ja" &
          .data$SkjemaStatusUtskrivelse == 1 &
          .data$ASA != "Ja" &
          .data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
          !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$indik_blodfortynnende) == "nei"))


  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_blodfortynnende_data == "ja" &
          .data$SkjemaStatusUtskrivelse == 1 &
          .data$ASA != "Ja" &
          .data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
          .data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_)) %>%
      dplyr::pull(.data$indik_blodfortynnende) == "nei"))


  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_blodfortynnende_data == "ja" &
          .data$SkjemaStatusUtskrivelse == 1 &
          (!.data$AndrePlatehemmere %in% c("Nei", "Ukjent", NA_character_) &
             !.data$Antikoagulantia %in% c("Nei", "Ukjent", NA_character_))) %>%
      dplyr::pull(.data$indik_blodfortynnende) == "ja"))



  expect_true(all(
    x_out %>%
      dplyr::filter(.data$SkjemaStatusUtskrivelse != 1,
                    .data$indik_blodfortynnende_data == "ja") %>%
      dplyr::pull(.data$indik_blodfortynnende)  == "ikke ferdigstilt"))

  # Forventer "ikke ferdigstilt" dersom datagrunnlag = ja, men ikke ferdigsilt
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_blodfortynnende == "ikke ferdigstilt",
                    .data$indik_blodfortynnende_data == "ja") %>%
      dplyr::pull(.data$SkjemaStatusUtskrivelse) %in% c(-1, 0)))


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
                 "indik_kolesterolsenkende_data",
                 "indik_kolesterolsenkende"))

  # Forventer postitivt antall stent dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_kolesterolsenkende_data == "ja") %>%
      dplyr::pull(.data$antall_stent_under_opphold) > 0))

  # Forventer primærforløp dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_kolesterolsenkende_data == "ja") %>%
      dplyr::pull(.data$Regtype) == "Primær"))


  # Forventer Ikke død dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_kolesterolsenkende_data == "ja") %>%
      dplyr::pull(.data$UtskrevetDod) %in% c("Nei", NA_character_)))


  # Forventer at datagrunnlag er nei, dersom ingen stent
  expect_true(all(
    x_out %>%
      dplyr::filter(is.na(.data$antall_stent_under_opphold) |
                      .data$antall_stent_under_opphold < 1) %>%
      dplyr::pull(.data$indik_kolesterolsenkende_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom sekundærforløp
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Regtype == "Primær") %>%
      dplyr::pull(.data$indik_kolesterolsenkende_data)  == "nei"))


  # Forventer at datagrunnlag er nei, dersom ikke levende
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$UtskrevetDod %in% c("Ja", "Ukjent")) %>%
      dplyr::pull(.data$indik_kolesterolsenkende_data)  == "nei"))



  # Forventer at KI er NA dersom ikke i datagrunnlag
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_kolesterolsenkende_data == "nei") %>%
      dplyr::pull(.data$indik_kolesterolsenkende) %>%
      is.na()))


  # Forventer at KI er ja dersom i datagrunnlaget og utskrevet statiner
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_kolesterolsenkende_data == "ja" &
          .data$SkjemaStatusUtskrivelse == 1 &
          .data$UtskrStatiner == "Ja") %>%
      dplyr::pull(.data$indik_kolesterolsenkende) == "ja"))

  # Forventer at KI er nei dersom i datagrunnlaget og ikke utskrevet statiner
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_kolesterolsenkende_data == "ja" &
          .data$SkjemaStatusUtskrivelse == 1 &
          ! .data$UtskrStatiner == "Ja") %>%
      dplyr::pull(.data$indik_kolesterolsenkende) == "nei"))


  # Forventer "ikke ferdigstilt" dersom datagrunnlag = ja, men ikke ferdigsilt
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$SkjemaStatusUtskrivelse != 1,
                    .data$indik_kolesterolsenkende_data == "ja") %>%
      dplyr::pull(.data$indik_kolesterolsenkende)  == "ikke ferdigstilt"))

  # Forventer "ikke ferdigstilt" dersom datagrunnlag = ja, men ikke ferdigsilt
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_kolesterolsenkende == "ikke ferdigstilt",
                    .data$indik_kolesterolsenkende_data == "ja") %>%
      dplyr::pull(.data$SkjemaStatusUtskrivelse) %in% c(-1, 0)))





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









test_that("ki_nstemi_utredet_innen24t works", {

  x <- data.frame(

    Indikasjon = c(NA, "Annet", "STEMI", rep("NSTEMI", 17)),
    Regtype = c("Primær", "Primær", "Primær", "Sekundær", rep("Primær", 16)),
    Innkomstarsak = c(rep("Brystsmerter", 4),
                      NA, "Øvrig","Dyspne", "Ukjent",
                      rep(c("Dyspne", "Ukjent", "Sirkulasjonsstans"), 4)),
    Hastegrad = c(rep(c("Akutt", "Subakutt"), 3),
                  "Planlagt", "Akutt",
                  rep(c("Akutt", "Subakutt"), 6)),
    OverflyttetFra = c(rep("Annet sykehus", 7),
                       NA, "Annen  avdeling på sykehuset",
                       rep("Annet sykehus", 11)),
    ventetid_nstemi_timer = c(
      rep(c(-8000, NA, 0, 0.01, 15, 4, 24.0, 24.1, 337),2),
      25, 03))

  x_out <- noric::ki_nstemi_utredet_innen24t(x)

  testthat::expect_equal(names(x_out),
                         c("Indikasjon",
                           "Regtype",
                           "Innkomstarsak",
                           "Hastegrad",
                           "OverflyttetFra",
                           "ventetid_nstemi_timer",
                           "indik_nstemi_angio_innen24t_data",
                           "indik_nstemi_angio_innen24t"))

  # Forventer Indikasjon NSTEMI dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen24t_data == "ja") %>%
      dplyr::pull(.data$Indikasjon) == "NSTEMI"))

  # Forventer primærforløp dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen24t_data == "ja") %>%
      dplyr::pull(.data$Regtype) == "Primær"))

  # Forventer ferdigstilt dersom datagrunnlag = ja
  expect_equal(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen24t_data == "ja") %>%
      dplyr::pull(.data$Innkomstarsak) %in% "Øvrig",
    rep(FALSE, 12))

  # Forventer akutt eller subakutt dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen24t_data == "ja") %>%
      dplyr::pull(.data$Hastegrad) %in% c("Akutt", "Subakutt")))

  # Forventet ikke overflyttet fra sykeshus eller NA dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen24t_data == "ja") %>%
      dplyr::pull(.data$OverflyttetFra) != "Annen  avdeling på sykehuset"))


  # Forventer at datagrunnlag er nei, dersom indikasjon ulik NSTEMI
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Indikasjon %in% "NSTEMI") %>%
      dplyr::pull(.data$indik_nstemi_angio_innen24t_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom sekundærforløp
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Regtype == "Primær") %>%
      dplyr::pull(.data$indik_nstemi_angio_innen24t_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom Innkomstårsak er Øvrig
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$Innkomstarsak == "Øvrig") %>%
      dplyr::pull(.data$indik_nstemi_angio_innen24t_data)  == "nei"))


  # Forventer at datagrunnlag er nei, dersom Planlagt forløp
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$Hastegrad == "Planlagt") %>%
      dplyr::pull(.data$indik_nstemi_angio_innen24t_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom overført eget shus
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$OverflyttetFra %in%
                      c(NA, "Annen  avdeling på sykehuset")) %>%
      dplyr::pull(.data$indik_nstemi_angio_innen24t_data)  == "nei"))



  # Forventer at KI er NA dersom ikke i datagrunnlag
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen24t_data == "nei") %>%
      dplyr::pull(.data$indik_nstemi_angio_innen24t) %>%
      is.na()))


  # Forventer at KI er ja dersom i datagrunnlaget og OK tidsdiff
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_nstemi_angio_innen24t_data == "ja" &
          !is.na(.data$ventetid_nstemi_timer) &
          .data$ventetid_nstemi_timer > 0.0 &
          .data$ventetid_nstemi_timer <= 24.0) %>%
      dplyr::pull(.data$indik_nstemi_angio_innen24t) == "ja"))

  # Forventer at KI er nei dersom i datagrunnlaget og for lang tidsdiff
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_nstemi_angio_innen24t_data == "ja" &
          !is.na(.data$ventetid_nstemi_timer) &
          .data$ventetid_nstemi_timer > 24.0 &
          .data$ventetid_nstemi_timer < 14 * 24) %>%
      dplyr::pull(.data$indik_nstemi_angio_innen24t) == "nei"))

  # Forventer at KI ugyldig dersom i datagrunnlaget, men tid mangler, er
  # negativ eller for lang
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_nstemi_angio_innen24t_data == "ja" &
          (is.na(.data$ventetid_nstemi_timer) |
             .data$ventetid_nstemi_timer <= 0.0 |
             .data$ventetid_nstemi_timer >= 14 * 24)) %>%
      dplyr::pull(.data$indik_nstemi_angio_innen24t) == "ugyldig/manglende"))


  # Forventer feilmelding dersom variabler mangler
  expect_error(
    noric::ki_nstemi_utredet_innen24t(
      df_ap = data.frame(tullenavn = c(1, 1, 1))))

  # Forventer feilmelding dersom feil kolonnenavn
  expect_error(
    noric::ki_nstemi_utredet_innen24t(
      df_ap = data.frame(Indikasjon = "NSTEMI",
                         Regtype = "Primær",
                         Innkomstarsak = "Ukjent",
                         Hastegrad = "Akutt",
                         OverfffflyttttetFraaa = "Ja")))

})








test_that("ki_nstemi_utredet_innen72t works", {

  x <- data.frame(

    Indikasjon = c(NA, "Annet", "STEMI", rep("NSTEMI", 17)),
    Regtype = c("Primær", "Primær", "Primær", "Sekundær", rep("Primær", 16)),
    Innkomstarsak = c(rep("Brystsmerter", 4),
                      NA, "Øvrig","Dyspne", "Ukjent",
                      rep(c("Dyspne", "Ukjent", "Sirkulasjonsstans"), 4)),
    Hastegrad = c(rep(c("Akutt", "Subakutt"), 3),
                  "Planlagt", "Akutt",
                  rep(c("Akutt", "Subakutt"), 6)),
    OverflyttetFra = c(rep("Annet sykehus", 7),
                       NA, "Annen  avdeling på sykehuset",
                       rep("Annet sykehus", 11)),
    ventetid_nstemi_timer = c(
      rep(c(-8000, NA, 0, 0.01, 15, 24, 72.0, 72.1, 337),2),
      25, 03))



  x_out <- noric::ki_nstemi_utredet_innen72t(x)

  testthat::expect_equal(names(x_out),
                         c("Indikasjon",
                           "Regtype",
                           "Innkomstarsak",
                           "Hastegrad",
                           "OverflyttetFra",
                           "ventetid_nstemi_timer",
                           "indik_nstemi_angio_innen72t_data",
                           "indik_nstemi_angio_innen72t"))

  # Forventer Indikasjon NSTEMI dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen72t_data == "ja") %>%
      dplyr::pull(.data$Indikasjon) == "NSTEMI"))

  # Forventer primærforløp dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen72t_data == "ja") %>%
      dplyr::pull(.data$Regtype) == "Primær"))

  # Forventer ferdigstilt dersom datagrunnlag = ja
  expect_equal(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen72t_data == "ja") %>%
      dplyr::pull(.data$Innkomstarsak) %in% "Øvrig",
    rep(FALSE, 12))

  # Forventer akutt eller subakutt dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen72t_data == "ja") %>%
      dplyr::pull(.data$Hastegrad) %in% c("Akutt", "Subakutt")))

  # Forventet ikke overflyttet fra sykeshus eller NA dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen72t_data == "ja") %>%
      dplyr::pull(.data$OverflyttetFra) != "Annen  avdeling på sykehuset"))


  # Forventer at datagrunnlag er nei, dersom indikasjon ulik NSTEMI
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Indikasjon %in% "NSTEMI") %>%
      dplyr::pull(.data$indik_nstemi_angio_innen72t_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom sekundærforløp
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Regtype == "Primær") %>%
      dplyr::pull(.data$indik_nstemi_angio_innen72t_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom Innkomstårsak er Øvrig
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$Innkomstarsak == "Øvrig") %>%
      dplyr::pull(.data$indik_nstemi_angio_innen72t_data)  == "nei"))


  # Forventer at datagrunnlag er nei, dersom Planlagt forløp
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$Hastegrad == "Planlagt") %>%
      dplyr::pull(.data$indik_nstemi_angio_innen72t_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom overført eget shus
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$OverflyttetFra %in%
                      c(NA, "Annen  avdeling på sykehuset")) %>%
      dplyr::pull(.data$indik_nstemi_angio_innen72t_data)  == "nei"))



  # Forventer at KI er NA dersom ikke i datagrunnlag
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_nstemi_angio_innen72t_data == "nei") %>%
      dplyr::pull(.data$indik_nstemi_angio_innen72t) %>%
      is.na()))


  # Forventer at KI er ja dersom i datagrunnlaget og OK tidsdiff
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_nstemi_angio_innen72t_data == "ja" &
          !is.na(.data$ventetid_nstemi_timer) &
          .data$ventetid_nstemi_timer > 0.0 &
          .data$ventetid_nstemi_timer <= 72.0) %>%
      dplyr::pull(.data$indik_nstemi_angio_innen72t) == "ja"))

  # Forventer at KI er nei dersom i datagrunnlaget og for lang tidsdiff
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_nstemi_angio_innen72t_data == "ja" &
          !is.na(.data$ventetid_nstemi_timer) &
          .data$ventetid_nstemi_timer > 72.0 &
          .data$ventetid_nstemi_timer < 14 * 24) %>%
      dplyr::pull(.data$indik_nstemi_angio_innen72t) == "nei"))

  # Forventer at KI ugyldig dersom i datagrunnlaget, men tid mangler, er
  # negativ eller for lang
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_nstemi_angio_innen72t_data == "ja" &
          (is.na(.data$ventetid_nstemi_timer) |
             .data$ventetid_nstemi_timer <= 0.0 |
             .data$ventetid_nstemi_timer >= 14 * 24)) %>%
      dplyr::pull(.data$indik_nstemi_angio_innen72t) == "ugyldig/manglende"))


  # Forventer feilmelding dersom variabler mangler
  expect_error(
    noric::ki_nstemi_utredet_innen72t(
      df_ap = data.frame(tullenavn = c(1, 1, 1))))

  # Forventer feilmelding dersom feil kolonnenavn
  expect_error(
    noric::ki_nstemi_utredet_innen72t(
      df_ap = data.frame(Indikasjon = "NSTEMI",
                         Regtype = "Primær",
                         Innkomstarsak = "Ukjent",
                         Hastegrad = "Akutt",
                         OverfffflyttttetFraaa = "Ja")))

})







test_that("ki_stemi_pci_innen120min works", {

  x <- data.frame(
    AvdRESH = c(NA, 106944,
                rep(c(102966, 1:3), 7)),
    Indikasjon = c("STEMI", "STEMI", "NSTEMI", "Annet", NA,
                   rep("STEMI", 25)),
    Regtype = c(rep("Primær", 5), "Sekundær", NA, rep("Primær", 23)),
    GittTrombolyse = c(rep("Nei", 7),
                       "Ja, etter innkomst annet sykehus",
                       "Ja, etter innkomst ved PCI sykehus",
                       "Ja, prehospitalt",
                       "Ja, ukjent sted", NA,
                       rep(c("Nei",NA), 9)),
    Hastegrad = c(rep("Akutt", 11),
                  "Subakutt", "Planlagt",
                  rep("Akutt", 17)),
    HLRForSykehus = c(rep("Nei", 13), "Ja", "Ukjent",
                      "Nei", rep(c("Nei", NA), 7)),
    ProsedyreType = c(rep("PCI", 15), "Angio", rep("Angio + PCI", 14)),
    BeslutningsutlosendeEKG = c(rep("Prehospitalt", 20),
                                rep("Ukjent", 8),
                                "Prehospitalt", "Prehospitalt"),

    ventetid_stemi_min = c(-10, 0, 15, 19.8, 100, 120,
                           120.1, 120.0, 150, 1440, 1441, 2000,
                           rep(NA, 8),
                           -10, 0, 15, 100, 120.0,
                           120.1,  150, 1440, 0, 2000))
  x_out <- noric::ki_stemi_pci_innen120min(df_ap = x)



  # Forventede variabelnavn
  testthat::expect_equal(
    names(x_out),
    c("AvdRESH",
      "Indikasjon",
      "Regtype",
      "GittTrombolyse",
      "Hastegrad",
      "HLRForSykehus",
      "ProsedyreType",
      "BeslutningsutlosendeEKG",
      "ventetid_stemi_min",
      "indik_stemi_pci_innen2t_data",
      "indik_stemi_pci_innen2t"))


  # Forventede RESHID dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_stemi_pci_innen2t_data == "ja") %>%
      dplyr::pull(.data$AvdRESH) != 106944))

  # Forventer Indikasjon STEMI dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_stemi_pci_innen2t_data == "ja") %>%
      dplyr::pull(.data$Indikasjon) == "STEMI"))

  # Forventer primærforløp dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_stemi_pci_innen2t_data == "ja") %>%
      dplyr::pull(.data$Regtype) == "Primær"))

  # Forventer ikke Gitt trimbolyse dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_stemi_pci_innen2t_data == "ja") %>%
      dplyr::pull(.data$GittTrombolyse) %in% c(NA, "Nei")))

  # Forventer akutt eller subakutt dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_stemi_pci_innen2t_data == "ja") %>%
      dplyr::pull(.data$Hastegrad) %in% c("Akutt")))

  # Forventet ikke HLR dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_stemi_pci_innen2t_data == "ja") %>%
      dplyr::pull(.data$HLRForSykehus) %in% c("Nei", NA)))

  # Forventet ikke HLR dersom datagrunnlag = ja
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_stemi_pci_innen2t_data == "ja") %>%
      dplyr::pull(.data$ProsedyreType) %in% c("Angio + PCI", "PCI")))




  # Forventer at datagrunnlag er nei, dersom indikasjon ulik STEMI
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Indikasjon %in% "STEMI") %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersomGardermoen
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$AvdRESH %in% 106944) %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t_data)  == "nei"))


  # Forventer at datagrunnlag er nei, dersom sekundærforløp
  expect_true(all(
    x_out %>%
      dplyr::filter(!.data$Regtype == "Primær") %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom Gitt Trombolyse
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$GittTrombolyse %in%
                      c("Ja, etter innkomst annet sykehus",
                        "Ja, etter innkomst ved PCI sykehus",
                        "Ja, prehospitalt",
                        "Ja, ukjent sted")) %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t_data)  == "nei"))


  # Forventer at datagrunnlag er nei, dersom Planlagt forløp
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$Hastegrad %in% c("Planlagt", "Subakutt")) %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t_data)  == "nei"))

  # Forventer at datagrunnlag er nei, dersom HLR Gitt
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$HLRForSykehus %in%
                      c("Ja", "Ukjent")) %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t_data)  == "nei"))

  # Forventer at datagrunnlag er nei, Angio
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$ProsedyreType %in%
                      c("Angio")) %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t_data)  == "nei"))









  # Forventer at KI er NA dersom ikke i datagrunnlag
  expect_true(all(
    x_out %>%
      dplyr::filter(.data$indik_stemi_pci_innen2t_data == "nei") %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t) %>%
      is.na()))


  # Forventer at KI er ja dersom i datagrunnlaget og OK tidsdiff
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_stemi_pci_innen2t_data == "ja" &
          !is.na(.data$ventetid_stemi_min) &
          .data$ventetid_stemi_min > 0 &
          .data$ventetid_stemi_min <= 120) %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t) == "ja"))



  # Forventer at KI er ja dersom i datagrunnlaget og OK ikke Besutl = prehosp
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_stemi_pci_innen2t_data == "ja" &
          .data$ventetid_stemi_min == 0 &
          !.data$BeslutningsutlosendeEKG %in% "Prehospitalt") %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t) == "ja"))


  # Forventer at KI er nei her
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_stemi_pci_innen2t_data == "ja" &
          .data$ventetid_stemi_min == 0.0 &
          .data$BeslutningsutlosendeEKG =="Prehospitalt") %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t) ==  "ugyldig/manglende"))


  # Forventer at KI er nei dersom i datagrunnlaget og for lang tidsdiff
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_stemi_pci_innen2t_data == "ja" &
          !is.na(.data$ventetid_stemi_min) &
          .data$ventetid_stemi_min > 120 &
          .data$ventetid_stemi_min <= 24 * 60) %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t) == "nei"))

  # Forventer at KI ugyldig dersom i datagrunnlaget, men tid mangler, er
  # negativ eller for lang
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$indik_stemi_pci_innen2t_data == "ja" &
          (is.na(.data$ventetid_stemi_min) |
             .data$ventetid_stemi_min < 0 |
             .data$ventetid_stemi_min > 24 * 60)) %>%
      dplyr::pull(.data$indik_stemi_pci_innen2t) == "ugyldig/manglende"))


  # Forventer feilmelding dersom variabler mangler
  expect_error(
    noric::ki_stemi_pci_innen120min(
      df_ap = data.frame(tullenavn = c(1, 1, 1))))

  # Forventer feilmelding dersom feil kolonnenavn
  expect_error(
    noric::ki_stemi_pci_innen120min(
      df_ap = data.frame(Indikasjon = "NSTEMI",
                         Regtype = "Primær",
                         Innkomstarsak = "Ukjent",
                         Hastegrad = "Akutt",
                         OverfffflyttttetFraaa = "Ja")))


})

