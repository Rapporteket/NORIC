test_that("Sykehusnavn is as expected", {

  x <- data.frame(AvdRESH = c(108141,
                              102966,
                              106944,
                              4210141,
                              700422,
                              114150,
                              104284,
                              105502,
                              109880,
                              101619))

  testthat::expect_equal(
    object = fikse_sykehusnavn(x)[, 2],
    expected = c("Ahus Nordbyhagen",
                 "HUS",
                 "AHUS Gardermoen",
                 "NLSH Bodø",
                 "OUS Rikshospitalet",
                 "SSHF Arendal",
                 "St.Olavs hospital",
                 "SUS",
                 "OUS Ullevål",
                 "UNN Tromsø"))
})


test_that("When wrong AvdResh, sykehusnavn should be missing", {
  x <- data.frame(AvdRESH = c(456, 987, 4445673, NA))
  testthat::expect_true(all(is.na(fikse_sykehusnavn(x)[, 2])))
})


test_that("Error message is produced when AvdRESH is missing", {
  x <- data.frame(toto = c(101619, 102966))
  testthat::expect_error(fikse_sykehusnavn(x))
})
