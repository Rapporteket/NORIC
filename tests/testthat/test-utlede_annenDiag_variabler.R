test_that("Testing utlede_kar_annen_diag is correct", {
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
                  segment_num = c(1:4, 18:19, 6:10, 20, 11:17, 5, NA, 17,5),
                  graft = c(rep("Nei", 20), "Arterie", "Vene", NA))

  x %<>% utlede_kar_annen_diag(.)

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
