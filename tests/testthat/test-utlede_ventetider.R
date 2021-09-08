test_that("legg_til_ventetid_nstemi_timer() works", {

  x <- data.frame(
    OverflyttetFra = c(
      "Annen avdeling på sykehuset", NA,
      rep(c("Omdirigert ambulanse", "Nei, direkte inn til dette sykehuset"), 4),
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
                                    "Nei, direkte inn til dette sykehuset"),
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
            "Nei, direkte inn til dette sykehuset")) %>%
      dplyr::pull(.data$ventetid_nstemi_timer) %>%
      is.na()))

  # Forventer disse tidene dersom direkte innlagt
  expect_equal(
    c(87673.0, NA, NA, 2.4, NA, -836971.1, 73.5, 36.0),

    x_out %>%
      dplyr::filter(
        .data$OverflyttetFra %in%
          c("Omdirigert ambulanse",
            "Nei, direkte inn til dette sykehuset")) %>%
      dplyr::pull(.data$ventetid_nstemi_timer))


  # Forventer disse tidene dersom overført
  expect_equal(
    c(NA, NA, -876585.0, 72.0, 41.4, -33.8, 26.3, 61.0),

    x_out %>%
      dplyr::filter(
        .data$OverflyttetFra %in%
          c("Annet sykehus")) %>%
      dplyr::pull(.data$ventetid_nstemi_timer))


})
