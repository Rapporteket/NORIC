testthat::test_that("getKodebokMedUtledetedVar fungerer", {
  
  kb <- noric::getKodebokMedUtledetedVar()
  
  testthat::expect_true(all(
    names(kb) %in% c("skjemanavn", 
                     "fysisk_feltnavn", 
                     "ledetekst", 
                     "listeverdier", 
                     "listetekst")))
  
  testthat::expect_true("data.frame" %in% is(kb))
  
})