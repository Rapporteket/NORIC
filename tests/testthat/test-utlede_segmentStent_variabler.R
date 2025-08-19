test_that("Number of stents is correct", {
  
  test_ap <- data.frame(ForlopsID = 1:6,
                        AvdRESH = rep(1, 6))
  
  test_ss <- data.frame(ForlopsID = c(1, 1, 1, 3, 4, 5, 5, 5, 6, 6),
                        AvdRESH = rep(1, 10),
                        StentType = c("A", "A", NA,
                                      "B", "C",
                                      NA, NA, NA,
                                      "A", "B"))
  
  # If two stents + one missing, total number of stents should be 2
  testthat::expect_equal(
    object = legg_til_antall_stent(df_ap = test_ap, df_ss = test_ss) %>%
      dplyr::filter(ForlopsID == 1) %>%
      dplyr::pull(antall_stent),
    expected = 2)
  
  # If no information on segment-level : value should be NA
  testthat::expect_true(legg_til_antall_stent(df_ap = test_ap,
                                              df_ss = test_ss) %>%
                          dplyr::filter(ForlopsID == 2) %>%
                          dplyr::pull(antall_stent) %>%
                          is.na())
  
  
  # If one stent , total number of stents should be 1
  testthat::expect_equal(legg_til_antall_stent(df_ap = test_ap,
                                               df_ss = test_ss) %>%
                           dplyr::filter(ForlopsID == 3) %>%
                           dplyr::pull(antall_stent),
                         1)
  
  # If only missing StentType , total number of stents should be 0
  testthat::expect_equal(legg_til_antall_stent(df_ap = test_ap,
                                               df_ss = test_ss) %>%
                           dplyr::filter(ForlopsID == 5) %>%
                           dplyr::pull(antall_stent),
                         0)
  
  # If different 2 values for StentType, total number of stents should be 2
  testthat::expect_equal(legg_til_antall_stent(df_ap = test_ap,
                                               df_ss = test_ss) %>%
                           dplyr::filter(ForlopsID == 6) %>%
                           dplyr::pull(antall_stent),
                         2)
  
  # Number of columns should be 3
  testthat::expect_equal(legg_til_antall_stent(df_ap = test_ap,
                                               df_ss = test_ss) %>%
                           ncol(),
                         3)
  
})


test_that("satt inn stent i segment 5 is correct", {
  
  test_ap <- data.frame(ForlopsID = 1:6,
                        AvdRESH = rep(1, 6))
  
  test_ss <- data.frame(ForlopsID = c(1, 1, 1, 3, 4, 5, 5, 5, 6, 6),
                        AvdRESH = rep(1, 10),
                        Segment = c(5, 1, 1, 5, 2, 5, 1, 1, 5, 5),
                        Graft = rep("Nei", 10),
                        StentType = c("A", "A", NA,
                                      "B", "C",
                                      NA, NA, NA,
                                      "A", "B"))
  
  # For disse forløpene forventes det at "satt_inn_stent_i_LMS" = ja
  testthat::expect_true(all(
    noric::satt_inn_stent_i_lms(df_ap = test_ap,
                                df_ss = test_ss) %>%
      dplyr::filter(ForlopsID %in% c(1, 3, 6)) %>%
      dplyr::pull(satt_inn_stent_i_LMS) == "ja"))
  
  # For disse forløpene forventes det at satt_inn_stent_i_LMS = nei
  testthat::expect_true(all(
    noric::satt_inn_stent_i_lms(df_ap = test_ap,
                                df_ss = test_ss) %>%
      dplyr::filter(ForlopsID %in% c(4, 5)) %>%
      dplyr::pull(satt_inn_stent_i_LMS) == "nei"))
  
  
  # For disse forløpene forventes det at satt_inn_stent_i_LMS = NA
  testthat::expect_true(noric::satt_inn_stent_i_lms(df_ap = test_ap,
                                                    df_ss = test_ss) %>%
                          dplyr::filter(ForlopsID == 2) %>%
                          dplyr::pull(satt_inn_stent_i_LMS) %>%
                          is.na())
  
  
  
  # Forventet antall rader og kolonner
  testthat::expect_equal(
    noric::satt_inn_stent_i_lms(df_ap = test_ap, df_ss = test_ss) %>%
      dim(),
    c(6,3))
  
  # Forventede navn på rader
  testthat::expect_equal(
    noric::satt_inn_stent_i_lms(df_ap = test_ap, df_ss = test_ss) %>%
      names(),
    c("ForlopsID","AvdRESH", "satt_inn_stent_i_LMS"))
})




test_that("legg_til_antall_stent_opphold is correct", {
  
  x <- data.frame(AvdRESH = rep(1, 13),
                  OppholdsID = c(101:106, 101, 102, 102, 103, 104, 106, 50),
                  antall_stent = c(0, 5, NA, 1, NA, NA,
                                   3, 1, 2, 3, NA, NA, 10))
  x_out <- noric::legg_til_antall_stent_opphold(x)
  
  
  testthat::expect_equal(
    names(x_out),
    c("AvdRESH", "OppholdsID", "antall_stent", "antall_stent_under_opphold"))
  
  
  
  # Dersom flere forløp for et opphold skal alle forløp ha totalsummen
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(OppholdsID %in% c(102)) %>%
      dplyr::pull(antall_stent_under_opphold)  == 8))
  
  # Dersom ingen forløp har noe informasjon i SegmentStent, skal NA returneres
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(OppholdsID %in% c(105, 106)) %>%
      dplyr::pull(antall_stent_under_opphold) %>%
      is.na())
  )
  
  # Dersom minst et forløp har en registrering, skal ikke NA returneres
  testthat::expect_true(all(
    x_out %>%
      dplyr::filter(OppholdsID %in% c(103)) %>%
      dplyr::pull(antall_stent_under_opphold) == 3)
  )
  
})



test_that("utlede_kar_segmen_stent is correct", {
  x <- data.frame(ForlopsID = 1:23,
                  AvdRESH = rep(1, 23),
                  Segment = c("(1) Proximale RCA",
                              "(2) Midtre RCA",
                              "(3) Distale RCA",
                              "(4) PDA/RPD",
                              "(5) Ve hovedstamme",
                              "(6) Proximale LAD",
                              "(7) Midtre LAD",
                              "(8) Distale LAD",
                              "(9) Første diagonal",
                              "(10) Andre diagonal",
                              "(11) Proximale LCx",
                              "(12) Første obtusa marginal",
                              "(13) Andre obtusa marginal",
                              "(14) Distale LCx",
                              "(15) LPD",
                              "(16) PLA fra venstre",
                              "(17) Intermediær",
                              "(18) PLA",
                              "(19) Høyrekammergren",
                              "(20) Septal",
                              "(1) Proximale RCA",
                              "(2) Midtre RCA",
                              "(3) Distale RCA"),
                  Graft = c(rep("Nei", 20), "Arteriell", "Vene", NA))
  
  x %<>% utlede_kar_segment_stent(.)
  
  testthat::expect_equal(c(23, 5), dim(x))
  testthat::expect_true("kar" %in% names(x))
  
  testthat::expect_equal(
    expected = rep("RCA", 7),
    object = x %>%
      dplyr::filter(Segment %in% c("(1) Proximale RCA", 
                                   "(2) Midtre RCA", 
                                   "(3) Distale RCA", 
                                   "(4) PDA/RPD", 
                                   "(18) PLA", 
                                   "(19) Høyrekammergren"),
                    !Graft %in% c("Arteriell", "Vene")) %>%
      dplyr::pull(kar) %>%
      as.character())
  
  testthat::expect_equal(rep("LAD", 6),
                         x %>%
                           dplyr::filter(Segment %in% c("(6) Proximale LAD",
                                                        "(7) Midtre LAD", 
                                                        "(8) Distale LAD", 
                                                        "(9) Første diagonal", 
                                                        "(10) Andre diagonal",
                                                        "(20) Septal"),
                                         !Graft %in% c("Arteriell", "Vene")) %>%
                           dplyr::pull(kar) %>%
                           as.character())
  
  testthat:: expect_equal(rep("CX", 7),
                          x %>%
                            dplyr::filter(
                              Segment %in% c(
                                "(11) Proximale LCx", 
                                "(12) Første obtusa marginal", 
                                "(13) Andre obtusa marginal",
                                "(14) Distale LCx",
                                "(15) LPD",
                                "(16) PLA fra venstre",
                                "(17) Intermediær"),
                              !Graft %in% c("Arteriell", "Vene")) %>%
                            dplyr::pull(kar) %>%
                            as.character())
  
  testthat::expect_equal(rep("LMS", 1),
                         x %>%
                           dplyr::filter(
                             Segment == "(5) Ve hovedstamme",
                             !Graft %in% c("Arteriell", "Vene")) %>%
                           dplyr::pull(kar) %>%
                           as.character())
  
  testthat::expect_equal(rep("Graft", 2),
                         x %>%
                           dplyr::filter(Graft %in% c("Arteriell", "Vene")) %>%
                           dplyr::pull(kar) %>%
                           as.character())
  
  
})

test_that("utlede_kar_graft_segment_stent is correct", {
  x <- data.frame(ForlopsID = 1:60,
                  AvdRESH = rep(1, 60),
                  Segment = rep(
                    c("(1) Proximale RCA",
                      "(2) Midtre RCA",
                      "(3) Distale RCA",
                      "(4) PDA/RPD",
                      "(5) Ve hovedstamme",
                      "(6) Proximale LAD",
                      "(7) Midtre LAD",
                      "(8) Distale LAD",
                      "(9) Første diagonal",
                      "(10) Andre diagonal",
                      "(11) Proximale LCx",
                      "(12) Første obtusa marginal",
                      "(13) Andre obtusa marginal",
                      "(14) Distale LCx",
                      "(15) LPD",
                      "(16) PLA fra venstre",
                      "(17) Intermediær",
                      "(18) PLA",
                      "(19) Høyrekammergren",
                      "(20) Septal"), 
                    3),
                  
                  Graft = c(rep("Nei", 20),
                            rep("Arteriell", 20),
                            rep("Vene", 20)))
  
  x %<>% utlede_kar_graft_segment_stent(.)
  
  testthat::expect_equal(c(60, 5), dim(x))
  testthat::expect_true("kar_graft" %in% names(x))
  
  testthat:: expect_true(all(
    x %>%
      dplyr::filter(Segment %in% c("(1) Proximale RCA",
                                   "(2) Midtre RCA", 
                                   "(3) Distale RCA", 
                                   "(4) PDA/RPD", 
                                   "(18) PLA", 
                                   "(19) Høyrekammergren")) %>%
      dplyr::pull(kar_graft) %in%
      c("RCA", "RCA_veneGraft", "RCA_arterieGraft")))
  
  
  testthat::expect_true(all(
    x %>%
      dplyr::filter(Segment %in% c("(6) Proximale LAD",
                                   "(7) Midtre LAD",
                                   "(8) Distale LAD", 
                                   "(9) Første diagonal", 
                                   "(10) Andre diagonal", 
                                   "(20) Septal")) %>%
      dplyr::pull(kar_graft) %in% c("LAD", "LAD_veneGraft", "LAD_arterieGraft")))
  
  testthat::expect_true(all(
    x %>%
      dplyr::filter(Segment %in% c("(11) Proximale LCx", 
                                   "(12) Første obtusa marginal", 
                                   "(13) Andre obtusa marginal", 
                                   "(14) Distale LCx", 
                                   "(15) LPD",
                                   "(16) PLA fra venstre", 
                                   "(17) Intermediær")) %>%
      dplyr::pull(kar_graft) %in% c("CX", "CX_veneGraft", "CX_arterieGraft")))
  
  
  
  testthat::expect_equal("LMS",
                         x %>%
                           dplyr::filter(Segment == "(5) Ve hovedstamme",
                                         !Graft %in% c("Arteriell", "Vene")) %>%
                           dplyr::pull(kar_graft) %>%
                           as.character())
  
  
  testthat::expect_true(all(is.na(
    x %>%
      dplyr::filter(Segment == "(5) Ve hovedstamme",
                    Graft %in% c("Arteriell", "Vene")) %>%
      dplyr::pull(kar_graft))))
  
})





test_that("legg_til_pci_per_kar is correct", {
  
  test_ap <- data.frame(ForlopsID = 1:5,
                        AvdRESH = rep(1, 5))
  
  # All these variables er mandatory, no need to simulate missing values
  test_ss <- data.frame(ForlopsID = c(1, 2, 3, 3, 3),
                        AvdRESH = rep(1, 5),
                        Segment = c("(1) Proximale RCA", 
                                    "(5) Ve hovedstamme", 
                                    "(10) Andre diagonal", 
                                    "(12) Første obtusa marginal", 
                                    "(13) Andre obtusa marginal"),
                        Graft = c(rep("Nei", 3),
                                  rep("Arteriell", 1),
                                  rep("Vene", 1)),
                        ProsedyreType = c("Ballong + Stent",
                                          "Wireforsøk",
                                          "Rotablator",
                                          "Wireforsøk",
                                          "Direktestent"))
  
  x <- test_ap %>% legg_til_pci_per_kar(., df_ss = test_ss)
  
  # Test dimetions
  testthat::expect_equal(c(5, 12), dim(x))
  
  # Test name and order of new variables
  testthat::expect_equal(
    names(x),
    c("ForlopsID", "AvdRESH", "PCI_LMS", "PCI_LAD", "PCI_RCA",
      "PCI_CX",  "PCI_LAD_arterieGraft", "PCI_RCA_arterieGraft",
      "PCI_CX_arterieGraft", "PCI_LAD_veneGraft",
      "PCI_RCA_veneGraft", "PCI_CX_veneGraft"))
  
  
  # Test that procedures without SS-data have value "NA" for all new variables
  testthat::expect_true(all(is.na(
    x %>%
      dplyr::filter(ForlopsID %in% 4:5) %>%
      dplyr::select(contains("PCI_")))))
  
  
  # Test that procedure with only "Wireforsøk" has "nei" for all new variables
  testthat::expect_true(all(x %>%
                              dplyr::filter(ForlopsID == 2) %>%
                              dplyr::select(contains("PCI")) == "nei"))
  
  # Test that ForlopsID = 1 is correct
  testthat::expect_true(all(x %>%
                              dplyr::filter(ForlopsID == 1) %>%
                              dplyr::select(contains("PCI_")) %>%
                              dplyr::select(- PCI_RCA) == "nei"))
  
  testthat::expect_true(x %>%
                          dplyr::filter(ForlopsID == 1) %>%
                          dplyr::pull(PCI_RCA)  == "ja")
  
  
  # Test that ForlopsID = 3 is correct
  testthat::expect_true(all(
    x %>%
      dplyr::filter(ForlopsID == 3) %>%
      dplyr::select(contains("PCI_")) %>%
      dplyr::select(- PCI_LAD, - PCI_CX_veneGraft) == "nei"))
  
  testthat::expect_true(all(
    x %>%
      dplyr::filter(ForlopsID == 3) %>%
      dplyr::select(PCI_LAD, PCI_CX_veneGraft)  == "ja"))
  
  
  testthat::expect_error(
    noric::legg_til_pci_per_kar(df_ap = test_ap %>%
                                  dplyr::select(-ForlopsID),
                                df_ss = test_ss))
  
  testthat::expect_error(
    noric::legg_til_pci_per_kar(df_ap = test_ap,
                                df_ss = test_ss %>%
                                  dplyr::select(-ProsedyreType)))
  
})




test_that("legg_til_wirefosok_per_kar is correct", {
  
  test_ap <- data.frame(ForlopsID = 1:5,
                        AvdRESH = rep(1, 5))
  
  # Alle these variables er mandatory, no need to simulate missing values
  test_ss <- data.frame(ForlopsID = c(1, 2, 3, 3, 3),
                        AvdRESH = rep(1, 5),
                        Segment = c("(1) Proximale RCA", 
                                    "(5) Ve hovedstamme", 
                                    "(10) Andre diagonal", 
                                    "(12) Første obtusa marginal", 
                                    "(13) Andre obtusa marginal"),
                        Graft = c(rep("Nei", 3),
                                  rep("Arteriell", 1),
                                  rep("Vene", 1)),
                        ProsedyreType = c("Ballong + Stent",
                                          "Wireforsøk",
                                          "Rotablator",
                                          "Wireforsøk",
                                          "Direktestent"))
  
  x <- legg_til_wireforsok_per_kar(df_ap = test_ap,
                                   df_ss = test_ss)
  
  # Test dimetions
  testthat::expect_equal(c(5, 12), dim(x))
  
  # Test name and order of new variables
  testthat::expect_equal(
    names(x),
    c("ForlopsID", "AvdRESH", "wireforsok_LMS", "wireforsok_LAD",
      "wireforsok_RCA", "wireforsok_CX",  "wireforsok_LAD_arterieGraft",
      "wireforsok_RCA_arterieGraft", "wireforsok_CX_arterieGraft",
      "wireforsok_LAD_veneGraft", "wireforsok_RCA_veneGraft",
      "wireforsok_CX_veneGraft"))
  
  
  # Test that procedures without SS-data have value "NA" for all new variables
  testthat::expect_true(all(is.na(
    x %>%
      dplyr::filter(ForlopsID %in% 4:5) %>%
      dplyr::select(contains("wireforsok_")))))
  
  
  # Test that procedure with only no "Wireforsøk" has "nei" for all
  # new variables
  testthat::expect_true(all(x %>%
                              dplyr::filter(ForlopsID == 1) %>%
                              dplyr::select(contains("wireforsok")) == "nei"))
  
  # Test that ForlopsID = 2 is correct
  testthat::expect_true(all(x %>%
                              dplyr::filter(ForlopsID == 2) %>%
                              dplyr::select(contains("wireforsok")) %>%
                              dplyr::select(- wireforsok_LMS) == "nei"))
  
  testthat::expect_true(x %>%
                          dplyr::filter(ForlopsID == 2) %>%
                          dplyr::pull(wireforsok_LMS)  == "ja")
  
  
  # Test that ForlopsID = 3 is correct
  testthat::expect_true(all(
    x %>%
      dplyr::filter(ForlopsID == 3) %>%
      dplyr::select(contains("wireforsok_")) %>%
      dplyr::select(- wireforsok_CX_arterieGraft) == "nei"))
  
  testthat::expect_true(
    x %>%
      dplyr::filter(ForlopsID == 3) %>%
      dplyr::pull(wireforsok_CX_arterieGraft)  == "ja")
  
  
  testthat::expect_error(
    noric::legg_til_wireforsok_per_kar(df_ap = test_ap %>%
                                         dplyr::select(-ForlopsID),
                                       df_ss = test_ss))
  
  testthat::expect_error(
    noric::legg_til_wireforsok_per_kar(df_ap = test_ap,
                                       df_ss = test_ss %>%
                                         dplyr::select(-ProsedyreType)))
  
})
