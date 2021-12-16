test_that("utlede_kar_annen_diag is correct", {
  x <- data.frame(ForlopsID = 1:23,
                  AvdRESH = rep(1, 23),
                  segment = c("Proximale RCA (1)",
                              "Midtre RCA (2)",
                              "Distale RCA (3)",
                              "PDA/RPD (4)",
                              "PLA (18)",
                              "Høyrekammergren (19)",
                              "Proximale LAD (6)",
                              "Midtre LAD (7)",
                              "Distale LAD (8)",
                              "Første diagonal (9)",
                              "Andre diagonal (10)",
                              "Septal (20)",
                              "Proximale LCx (11)",
                              "Første obtusa marginal (12)",
                              "Andre obtusa marginal (13)",
                              "Distale LCx (14)",
                              "LPD (15)",
                              "PLA fra venstre (16)",
                              "Intermediær (17)",
                              "Ve hovedstamme (5)",
                              NA,
                              "Intermediær (17)",
                              "Ve hovedstamme (5)"),
                  segment_num = c(1:4, 18:19, 6:10, 20, 11:17, 5, NA, 17, 5),
                  graft = c(rep("Nei", 20), "Arterie", "Vene", NA))

  x %<>% noric::utlede_kar_annen_diag(.)

  testthat::expect_equal(c(23, 6), dim(x))
  testthat::expect_true("kar" %in% names(x))

  testthat::expect_equal(
    expected = rep("RCA", 6),
    object = x %>%
      dplyr::filter(segment_num %in% c(1, 2, 3, 4, 18, 19),
                    !graft %in% c("Arterie", "Vene")) %>%
      dplyr::pull(kar) %>%
      as.character())

  testthat::expect_equal(rep("LAD", 6),
                         x %>%
                           dplyr::filter(segment_num %in% c(6, 7, 8, 9, 10, 20),
                                         !graft %in% c("Arterie", "Vene")) %>%
                           dplyr::pull(kar) %>%
                           as.character())

  testthat:: expect_equal(rep("CX", 7),
                          x %>%
                            dplyr::filter(segment_num %in%
                                            c(11, 12, 13, 14, 15, 16, 17),
                                          !graft %in% c("Arterie", "Vene")) %>%
                            dplyr::pull(kar) %>%
                            as.character())

  testthat::expect_equal(rep("LMS", 2),
                         x %>%
                           dplyr::filter(segment_num == 5,
                                         !graft %in% c("Arterie", "Vene")) %>%
                           dplyr::pull(kar) %>%
                           as.character())

  testthat::expect_equal(rep("Graft", 2),
                         x %>%
                           dplyr::filter(graft %in% c("Arterie", "Vene")) %>%
                           dplyr::pull(kar) %>%
                           as.character())


})







test_that("legg_til_trykk_bilde_per_kar is correct", {

  test_ad <- data.frame(ForlopsID = c(1, 1, 1, 1, 1, 3, 4, 5, 5, 5),
                        AvdRESH = c(rep(1, 6), rep(2, 4)),
                        segment = c("Proximale RCA (1)",
                                    "Ve hovedstamme (5)",
                                    "PLA (18)",
                                    "Proximale LAD (6)",
                                    "Midtre LAD (7)",
                                    "Distale LAD (8)",
                                    "Første diagonal (9)",
                                    "Andre diagonal (10)",
                                    "Proximale RCA (1)",
                                    "Ve hovedstamme (5)"),
                        graft = c(rep("Nei", 5),
                                  "Arterie", "Nei",
                                  "Nei",  "Nei", "Vene"),
                        metode = c(rep(c("FFR", "IVUS", "IVUS", "FFR", "tull"),
                                       2)))

  test_ap <- data.frame(ForlopsID = c(1:5),
                        AvdRESH = c(1, 1, 1, 2, 2))

  x_ffr <- noric::legg_til_trykk_bilde_per_kar(df_ap = test_ap,
                                               df_ad = test_ad,
                                               metodeType = "FFR")

  x_ffr_ifr_ivus_oct <-  noric::legg_til_trykk_bilde_per_kar(df_ap = test_ap,
                                                             df_ad = test_ad,
                                                             metodeType = "FFR") %>%
    noric::legg_til_trykk_bilde_per_kar(df_ap = .,
                                        df_ad = test_ad,
                                        metodeType = "iFR") %>%

    noric::legg_til_trykk_bilde_per_kar(df_ap = .,
                                        df_ad = test_ad,
                                        metodeType = "IVUS") %>%
    noric::legg_til_trykk_bilde_per_kar(df_ap = .,
                                        df_ad = test_ad,
                                        metodeType = "OCT")





  # Test dimensions
  testthat::expect_equal(c(5, 7), dim(x_ffr))
  testthat::expect_equal(c(5, 22), dim(x_ffr_ifr_ivus_oct))

  # Test name and order of new variables
  testthat::expect_equal(
    names(x_ffr_ifr_ivus_oct),
    c("ForlopsID", "AvdRESH",
      "FFR_LMS", "FFR_LAD", "FFR_RCA", "FFR_CX",  "FFR_Graft",
      "iFR_LMS", "iFR_LAD", "iFR_RCA", "iFR_CX",  "iFR_Graft",
      "IVUS_LMS", "IVUS_LAD", "IVUS_RCA", "IVUS_CX",  "IVUS_Graft",
      "OCT_LMS", "OCT_LAD", "OCT_RCA", "OCT_CX",  "OCT_Graft"))

  # Test that procedures without AD-data have value "NA" for all new variables
  testthat::expect_true(all(is.na(
    x_ffr_ifr_ivus_oct %>%
      dplyr::filter(ForlopsID == 2, AvdRESH == 1) %>%
      dplyr::select(-AvdRESH, -ForlopsID))))


  # Test that procedure without any rows with method FFR has "nei" for
  # all FFR-variables
  testthat::expect_true(all(x_ffr_ifr_ivus_oct %>%
                              dplyr::filter(ForlopsID == 4, AvdRESH == 2) %>%
                              dplyr::select(contains("FFR")) == "nei"))

  # Test that ForlopsID = 1 is correct
  testthat::expect_true(all(x_ffr_ifr_ivus_oct %>%
                              dplyr::filter(ForlopsID == 1) %>%
                              dplyr::select(FFR_RCA,
                                            IVUS_LMS,
                                            IVUS_RCA,
                                            FFR_LAD) == "ja"))

  testthat::expect_true(all(x_ffr_ifr_ivus_oct %>%
                              dplyr::filter(ForlopsID == 1) %>%
                              dplyr::select(-ForlopsID,
                                            -AvdRESH,
                                            - FFR_RCA,
                                            - IVUS_LMS,
                                            - IVUS_RCA,
                                            - FFR_LAD)  == "nei"))


  # Test that ForlopsID = 3 is correct
  testthat::expect_true(all(
    x_ffr_ifr_ivus_oct %>%
      dplyr::filter(ForlopsID == 3) %>%
      dplyr::select(FFR_Graft) == "ja"))

  testthat::expect_true(all(
    x_ffr_ifr_ivus_oct %>%
      dplyr::filter(ForlopsID == 3) %>%
      dplyr::select(- ForlopsID, -AvdRESH, - FFR_Graft)  == "nei"))



  # Test that ForlopsID 5 is correct (test that metode = "tull" doesn't count)
  testthat::expect_true(all(
    x_ffr_ifr_ivus_oct %>%
      dplyr::filter(ForlopsID == 5) %>%
      dplyr::select(FFR_RCA, IVUS_LAD) == "ja"))

  testthat::expect_true(all(
    x_ffr_ifr_ivus_oct %>%
      dplyr::filter(ForlopsID == 5) %>%
      dplyr::select(- ForlopsID, -AvdRESH, - FFR_RCA, - IVUS_LAD)  == "nei"))


  testthat::expect_error(
    noric::legg_til_ffr_per_kar(df_ap = test_ap %>% dplyr::select(-ForlopsID),
                                df_ad = test_ad,
                                metodeType = "FFR"))

  testthat::expect_error(
    noric::legg_til_ffr_per_kar(df_ap = test_ap,
                                df_ad = test_ad %>% dplyr::select(-metode),
                                metodeType = "iFR"))

  testthat::expect_error(
    noric::legg_til_ffr_per_kar(df_ap = test_ap,
                                df_ad = test_ad,
                                metodeType = "tulleType"))





})
