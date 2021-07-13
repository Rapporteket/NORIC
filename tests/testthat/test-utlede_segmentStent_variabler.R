test_that("Number of stents is correct", {

  test_ap <- data.frame(ForlopsID = 1:6,
                        AvdRESH = rep(1,6))

  test_ss <- data.frame(ForlopsID = c(1,1,1,3,4,5,5,5,6,6),
                        AvdRESH = rep(1, 10),
                        StentType = c("A", "A", NA,
                                      "B", "C",
                                      NA, NA, NA,
                                      "A", "B"))

  # If two stents + one missing, total number of stents should be 2
  expect_equal(legg_til_antall_stent(ap = test_ap, ss = test_ss) %>%
                 filter(ForlopsID == 1) %>% pull(antall_stent),
               2)

  # If no information on segment-level : value should be NA
  expect_true(legg_til_antall_stent(ap = test_ap, ss = test_ss) %>%
                filter(ForlopsID == 2) %>%
                pull(antall_stent) %>%
                is.na())


  # If one stent , total number of stents should be 1
  expect_equal(legg_til_antall_stent(ap = test_ap, ss = test_ss) %>%
                 filter(ForlopsID == 3) %>% pull(antall_stent),
               1)

  # If only missing StentType , total number of stents should be 0
  expect_equal(legg_til_antall_stent(ap = test_ap, ss = test_ss) %>%
                 filter(ForlopsID == 5) %>% pull(antall_stent),
               0)

  # If different 2 values for StentType, total number of stents should be 2
  expect_equal(legg_til_antall_stent(ap = test_ap, ss = test_ss) %>%
                 filter(ForlopsID == 6) %>% pull(antall_stent),
               2)

  # Number of columns should be 3
  expect_equal(legg_til_antall_stent(ap = test_ap, ss = test_ss) %>%
                 ncol(),
               3)

})



test_that("Testing utlede_kar_segmen_stent is correct", {
  x <- data.frame(ForlopsID = 1:23,
                  AvdRESH = rep(1,23),
                  Segment = c(1:20, 1:3),
                  Graft = c(rep("Nei", 20), "Arteriell", "Vene", NA))

  x %<>% utlede_kar_segment_stent(.)

  expect_equal(c(23, 5), dim(x))
  expect__true("kar" %in% names(x))

  expect_equal(rep("RCA", 7),
               x %>%
                 filter(Segment %in% c(1, 2, 3, 4, 18, 19),
                       !Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar))

  expect_equal(rep("LAD", 6),
               x %>%
                 filter(Segment %in% c(6, 7, 8, 9, 10, 20),
                        !Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar))

  expect_equal(rep("CX", 7),
               x %>%
                 filter(Segment %in% c(11, 12, 13, 14, 15, 16, 17),
                        !Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar))

  expect_equal(rep("LMS", 1),
               x %>%
                 filter(Segment == 5,
                        !Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar))

  expect_equal(rep("Graft", 2),
               x %>%
                 filter(Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar))


  })





test_that("Testing utlede_kar_graft_segment_stent is correct", {
  x <- data.frame(ForlopsID = 1:60,
                  AvdRESH = rep(1,60),
                  Segment = rep(1:20,3),
                  Graft = c(rep("Nei", 20),
                            rep("Arteriell", 20),
                            rep("Vene", 20)))

  x %<>% utlede_kar_segment_stent(.)

  expect_equal(c(60, 5), dim(x))
  expect__true("kar_graft" %in% names(x))

  expect_true(all(x %>%
                    filter(Segment %in% c(1, 2, 3, 4, 18, 19)) %>%
                    pull(kar_graft) %in%
                    c("RCA", "RCA_veneGraft", "RCA_arterieGraft")))


  expect_true(all(x %>%
                    filter(Segment %in% c(6, 7, 8, 9, 10, 20)) %>%
                    pull(kar_graft) %in%
                    c("LAD", "LAD_veneGraft", "LAD_arterieGraft")))

  expect_true(all(x %>%
                    filter(Segment %in%  c(11, 12, 13, 14, 15, 16, 17)) %>%
                    pull(kar_graft) %in%
                    c("CX", "CX_veneGraft", "CX_arterieGraft")))



  expect_equal("LMS",
               x %>%
                 filter(Segment == 5,
                        !Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar_graft))


  expect_true(all(is.na(x %>%
                          filter(Segment == 5,
                                 Graft %in% c("Arteriell", "Vene")) %>%
                          pull(kar_graft))))

})





test_that("Testing legg_til_pci_per_kar", {

  test_ap <- data.frame(ForlopsID = 1:5,
                        AvdRESH = rep(1,5))

  # Alle these variables er mandatory, no need to simulate missing values
  test_ss <- data.frame(ForlopsID = c(1,2,3,3,3),
                        AvdRESH = rep(1,5),
                        Segment = c(1,5,10,12,13),
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
  expect_equal(c(5,12),
               dim(x))

  # Test name and order of new variables
  expect_equal(names(x),
               c("ForlopsID", "AvdRESH", "PCI_LMS", "PCI_LAD" , "PCI_RCA",
                 "PCI_CX",  "PCI_LAD_arterieGraft", "PCI_RCA_arterieGraft",
                 "PCI_CX_arterieGraft", "PCI_LAD_veneGraft", "PCI_RCA_veneGraft",
                 "PCI_CX_veneGraft"))


  # Test that procedures without SS-data have value "NA" for all new variables
  expect_true(all(is.na(x %>%
                          filter(ForlopsID %in% 4:5) %>%
                          select(contains("PCI_")))))


  # Test that procedure with only "Wireforsøk" has "nei" for all new variables
  expect_true(all(x %>%
                    filter(ForlopsID == 2) %>%
                    select(contains("PCI")) == "nei"))

  # Test that ForlopsID = 1 is correct
  expect_true(all(x %>%
                    filter(ForlopsID == 1) %>%
                    select(contains("PCI_")) %>%
                    select(- PCI_RCA) == "nei"))

  expect_true(x %>%
                filter(ForlopsID == 1) %>%
                pull(PCI_RCA)  == "ja")


  # Test that ForlopsID = 3 is correct
  expect_true(all(x %>%
                    filter(ForlopsID == 3) %>%
                    select(contains("PCI_")) %>%
                    select(- PCI_LAD, -PCI_CX_veneGraft) == "nei"))

  expect_true(all(x %>%
                    filter(ForlopsID == 3) %>%
                    select(PCI_LAD, PCI_CX_veneGraft)  == "ja"))


 expect_error(test_ap %>%
                select(-ForlopsID) %>%
                legg_til_pci_per_kar(., df_ss = test_ss))
 expect_error(test_ap %>%
                legg_til_pci_per_kar(., df_ss = test_ss %>%
                                       select(-ProsedyreType)))

})


