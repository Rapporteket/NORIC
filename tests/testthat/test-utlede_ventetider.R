test_that("legg_til_ventetid_nstemi_timer() works", {
  
  x <- data.frame(
    OverflyttetFra = c(
      "Annen  avdeling på sykehuset", NA,
      rep(c("Omdirigert ambulanse", "Nei, direkte inn til dette sykehus"), 4),
      rep("Annet sykehus",8)),
    
    ProsedyreDato = as.Date(
      c("2020-01-30", "2021-11-15",
        "2020-11-11",NA_character_, "1901-01-01", "2020-01-01",
        "2015-10-26", "1926-06-23", "2018-04-27", "2021-12-23",
        "1980-11-11",NA_character_, "1901-01-01", "2020-01-01",
        "2015-10-26", "2021-06-23", "2018-04-27", "2021-12-24"),
      format = "%Y-%m-%d"),
    
    ProsedyreTid = c(
      "22:30:00", "01:10:00",
      rep(c("22:30:00", "01:10:00", "16:30:00", "09:35:00",
            "02:59:00", "10:02:00", "13:45:00", "12:20:00"), 2)),
    
    
    AnkomstPCIDato = as.Date(
      c("2020-01-30", "2021-11-14",
        "2010-11-11", "2021-01-20", "2021-12-01", "2020-01-01",
        NA_character_, "2021-12-15", "2018-04-24", "2021-12-22",
        NA_character_, NA_character_, "2021-12-22", NA_character_,
        "2010-11-11", "2021-01-20", "2018-04-24", "2020-01-01"),
      format = "%Y-%m-%d"),
    
    AnkomstPCITid = c(
      "21:10:00", "23:10:00",
      "21:30:00", "00:10:00", NA_character_, "07:10:00",
      NA_character_, "05:10:00", "12:15:00", "00:20:00",
      NA_character_, NA_character_, "19:00:00", NA_character_,
      NA_character_, NA_character_, "05:20:00", NA_character_),
    
    InnleggelseHenvisendeSykehusDato = as.Date(
      c("2020-01-30", "2021-11-14",
        rep(NA_character_, 8),
        "2016-11-11", "2020-06-18", "2001-01-01", "2019-12-29",
        "2015-10-24", "2021-06-24", "2018-04-26", "2021-12-21"),
      format = "%Y-%m-%d"),
    
    InnleggelseHenvisendeSykehusTid = c(
      "01:00:00", "02:00:00",
      rep(NA_character_, 8),
      NA_character_, NA_character_, "01:30:00", "09:35:00",
      "09:35:00", "19:50:00", "11:25:00", "23:20:00"
    ))
  
  
  x_out <- noric::legg_til_ventetid_nstemi_timer(x)
  
  
  # Forventede variablenavn
  expect_equal(names(x_out),
               c("OverflyttetFra",
                 "ProsedyreDato",
                 "ProsedyreTid",
                 "AnkomstPCIDato",
                 "AnkomstPCITid",
                 "InnleggelseHenvisendeSykehusDato",
                 "InnleggelseHenvisendeSykehusTid",
                 "ventetid_nstemi_timer"))
  
  # Forventer at ventetid mangler dersom en av dato-tidspunkt mangler
  # for direkte innlagt
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$OverflyttetFra %in% c("Omdirigert ambulanse",
                                    "Nei, direkte inn til dette sykehus"),
        (is.na(.data$ProsedyreDato) | is.na(.data$ProsedyreTid) |
           is.na(.data$AnkomstPCIDato) | is.na(.data$AnkomstPCITid))) %>%
      dplyr::pull(.data$ventetid_nstemi_timer) %>%
      is.na()))
  
  
  # Forventer at ventetid mangler dersom en av dato-tidspunkt mangler
  # for overførte pasienter
  expect_true(all(
    x_out %>%
      dplyr::filter(
        .data$OverflyttetFra %in% c("Annet sykehus"),
        (is.na(.data$ProsedyreDato) | is.na(.data$ProsedyreTid) |
           is.na(.data$InnleggelseHenvisendeSykehusDato) |
           is.na(.data$InnleggelseHenvisendeSykehusTid))) %>%
      dplyr::pull(.data$ventetid_nstemi_timer) %>%
      is.na()))
  
  
  # Forventer NA dersom verken overført eller direkte
  expect_true(all(
    x_out %>%
      dplyr::filter(
        !.data$OverflyttetFra %in%
          c("Annet sykehus",
            "Omdirigert ambulanse",
            "Nei, direkte inn til dette sykehus")) %>%
      dplyr::pull(.data$ventetid_nstemi_timer) %>%
      is.na()))
  
  # Forventer disse tidene dersom direkte innlagt
  expect_equal(
    c(87673.00, NA, NA, 2.42, NA, -836971.13, 73.50, 36.00),
    
    x_out %>%
      dplyr::filter(
        .data$OverflyttetFra %in%
          c("Omdirigert ambulanse",
            "Nei, direkte inn til dette sykehus")) %>%
      dplyr::pull(.data$ventetid_nstemi_timer))
  
  
  # Forventer disse tidene dersom overført
  expect_equal(
    c(NA, NA, -876585.00, 72.00, 41.40, -33.80, 26.33, 61.00),
    
    x_out %>%
      dplyr::filter(
        .data$OverflyttetFra %in%
          c("Annet sykehus")) %>%
      dplyr::pull(.data$ventetid_nstemi_timer))
  
  
})


test_that("legg_til_ventetid_stemi_min() works", {
  
  
  
  x <- data.frame(
    ProsedyreDato = as.Date(c("2020-01-30", "2021-11-15",
                              "2020-11-11", "2021-12-24",
                              "2020-01-19", "2020-01-19",
                              "2020-01-19", "2020-01-19",
                              NA, "2020-01-19", 
                              "2020-01-19", "2020-01-19"),
                            format = "%Y-%m-%d"),
    
    ProsedyreTid = c("22:30:00", "01:10:00",
                     "13:45:00", "12:20:00",
                     "12:00:00", "12:00:00",
                     "12:00:00", "12:00:00",
                     "12:00:00", NA, 
                     "12:00:00", "12:00:00"),
    
    BeslEKGDato = as.Date(c("2020-01-30", "2021-11-14",
                            "2018-04-24", "2020-01-01",
                            "2020-01-19", "2020-01-20",
                            "2020-01-19", "2020-01-19",
                            "2020-01-19", "2020-01-19", 
                            NA, NA),
                          format = "%Y-%m-%d"),
    
    BeslEKGTid = c( "21:10:00", "23:10:00",
                    "05:20:00", NA_character_,
                    "13:05:00", "12:00:00",
                    "12:00:00", "11:30:30",
                    "12:00:00", "12:00:00", 
                    NA_character_, NA_character_),
    
    BesUtlEKGDato = as.Date(c("2020-01-30", "2021-11-14",
                              "2018-04-24", "2021-12-24",
                              "2020-01-19", "2020-01-20",
                              "2020-01-19", "2020-01-19",
                              "2020-01-19", "2020-01-19", 
                              "2020-01-19", "2020-01-19"),
                            format = "%Y-%m-%d"),
    
    BesUtlEKGTid = c( "21:10:00", "23:10:00",
                      "05:20:00", "10:00:00",
                      "13:05:00", "12:00:00",
                      "12:00:00", "11:30:30",
                      "12:00:00", "12:00:00", 
                      "11:00:00", NA_character_),
    
    BeslutningsutlosendeEKG = c(
      rep(c("Etter ankomst dette sykehus",
            "Prehospitalt",
            "Ukjent",
            "Ved annet sykehus", NA), 2), 
    "Prehospitalt","Prehospitalt"))
  
  x_out <- noric::legg_til_ventetid_stemi_min(x)
  
  
  
  # Forventede variablenavn
  expect_equal(names(x_out),
               c("ProsedyreDato",
                 "ProsedyreTid",
                 "BeslEKGDato", 
                 "BeslEKGTid",
                 "BesUtlEKGDato",
                 "BesUtlEKGTid",
                 "BeslutningsutlosendeEKG",
                 "ventetid_stemi_min"))
  
  # Forventer at ventetid mangler dersom prosedyredato mangler
  expect_true(all(
    x_out %>%
      dplyr::filter(is.na(.data$ProsedyreDato) | is.na(.data$ProsedyreTid))  %>%
      dplyr::pull(.data$ventetid_stemi_min) %>%
      is.na()))
 
  # Forventer at ventetid mangler dersom ingen EKG
  expect_true(all(
    x_out %>%
      dplyr::filter(
        
        (is.na(.data$BeslEKGDato) | is.na(.data$BeslEKGTid)) &
          (is.na(.data$BesUtlEKGDato) | is.na(.data$BesUtlEKGTid)))   %>%
      dplyr::pull(.data$ventetid_stemi_min) %>%
      is.na()))
  
  # Forventet ventetid er denne vektoren
  expect_equal(
    x_out %>% dplyr::pull(.data$ventetid_stemi_min),
    c(80.0, 120.0, 1342585.0, 140.0, -65.0, 
      -1440.0, NA, 29.5, NA, NA, 60.0, NA))
  
})





testthat::test_that("legg_til_liggedogn fungerer", {
  
  
  df <- data.frame(
    
    Regtype = c("Sekundær", rep("Primær", 9)),
    
    OverflyttetFra = c( "Annet sykehus",
                        "Annen  avdeling på sykehuset",
                        NA,
                        "Omdirigert ambulanse",
                        "Nei, direkte inn til dette sykehus",
                        rep("Annet sykehus", 5)),
    
    AnkomstPCIDato = as.Date(c(rep("2021-11-30", 3), NA, rep("2021-11-30", 6)),
                             format = "%Y-%m-%d"),
    
    
    Utskrivningsdato = as.Date(c(NA, "2021-11-30", "2021-11-30", "2021-11-30",
                                 NA,
                                 "2021-12-01", "2022-11-30",
                                 "1940-05-02", "2018-12-01",
                                 "2022-12-30"),
                               format = "%Y-%m-%d"))
  
  df_out <- noric::legg_til_liggedogn(df)
  
  
  testthat::expect_true(all(
    names(df_out) %in% c("Regtype",
                         "OverflyttetFra",
                         "AnkomstPCIDato",
                         "Utskrivningsdato",
                         "liggedogn",
                         "liggedogn_data")))
  
  
  testthat::expect_true(
    df_out %>%
      dplyr::filter(.data$Regtype == "Sekundær") %>%
      dplyr::pull(.data$liggedogn_data) == "nei")
  
  
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(
        .data$Regtype == "Primær" &
          (is.na(.data$OverflyttetFra) |
             .data$OverflyttetFra == "Annen  avdeling på sykehuset")) %>%
      dplyr::pull(.data$liggedogn_data) == "nei"))
  
  
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(
        .data$Regtype == "Primær",
        .data$OverflyttetFra %in% c("Nei, direkte inn til dette sykehus",
                                    "Omdirigert ambulanse",
                                    "Annet sykehus") &
          .data$liggedogn >= 0 &
          .data$liggedogn <= 60) %>%
      dplyr::pull(.data$liggedogn_data) == "ja"))
  
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(
        .data$Regtype == "Primær",
        .data$OverflyttetFra %in% c("Nei, direkte inn til dette sykehus",
                                    "Omdirigert ambulanse",
                                    "Annet sykehus") &
          (.data$liggedogn < 0 | .data$liggedogn > 60)) %>%
      dplyr::pull(.data$liggedogn_data) == "ugyldig tid"
  ))
  
  
  testthat::expect_true(all(
    df_out %>%
      dplyr::filter(
        .data$Regtype == "Primær" &
          .data$OverflyttetFra %in% c("Nei, direkte inn til dette sykehus",
                                      "Omdirigert ambulanse",
                                      "Annet sykehus") &
          is.na(liggedogn)) %>%
      dplyr::pull(.data$liggedogn_data) == "manglende" ))
  
  
  
  
  testthat::expect_equal(
    df_out %>%
      dplyr::filter(
        .data$liggedogn_data %in% c("ja", "ugyldig tid")) %>%
      dplyr::pull(.data$liggedogn),
    
    c(1, 365, -29797, -1095, 395))
  
  testthat::expect_error(
    noric::legg_til_liggedogn(df_ap = data.frame(tull1 = 1, tull2 = NA))
  )
  
})
