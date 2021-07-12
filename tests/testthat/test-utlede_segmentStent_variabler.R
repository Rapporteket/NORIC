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



test_that("Testing utlede_kar_graft is correct", {
  x <- data.frame(ForlopsID = 1:23,
                  AvdRESH = rep(1,23),
                  Segment = c(1:20, 1:3),
                  Graft = c(rep("Nei", 20), "Arteriell", "Vene", NA))

  x %<>% utlede_kar_graft_segmentStent(.)

  expect_equal(c(23, 5), dim(x))
  expect__true("kar_graft" %in% names(x))

  expect_equal(rep("RCA", 7),
               x %>%
                 filter(Segment %in% c(1, 2, 3, 4, 18, 19),
                       !Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar_graft))

  expect_equal(rep("LAD", 6),
               x %>%
                 filter(Segment %in% c(6, 7, 8, 9, 10, 20),
                        !Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar_graft))

  expect_equal(rep("CX", 7),
               x %>%
                 filter(Segment %in% c(11, 12, 13, 14, 15, 16, 17),
                        !Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar_graft))

  expect_equal(rep("LMS", 1),
               x %>%
                 filter(Segment == 5,
                        !Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar_graft))

  expect_equal(rep("Graft", 2),
               x %>%
                 filter(Graft %in% c("Arteriell", "Vene")) %>%
                 pull(kar_graft))


  })
