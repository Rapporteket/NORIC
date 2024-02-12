testthat::test_that("getKodebokMedUtledetedVar fungerer", {
  
  kb_var <- noric::getKodebokMedUtledetedVar()
  
  testthat::expect_true(all(
    names(kb_var) %in% c("skjemanavn", 
                         "fysisk_feltnavn", 
                         "ledetekst", 
                         "listeverdier", 
                         "listetekst", 
                         "aktiveringsspm", 
                         "underspm", 
                         "innfort", 
                         "tabell"  )))
  
  testthat::expect_true("data.frame" %in% is(kb_var))
  
})


testthat::test_that("getKodebokData fungerer", {
  
  kb_data<- noric::getKodebokData()
  
  testthat::expect_true(all(
    names(kb_data) %in% c("tabell", 
                          "variabel_navn", 
                          "skjemabygger", 
                          "fysisk_feltnavn", 
                          "ledetekst", 
                          "listeverdier", 
                          "listetekst", 
                          "aktiveringsspm", 
                          "underspm", 
                          "innfort", 
                          "skjemanavn", 
                          "type")))
  
  testthat::expect_true("data.frame" %in% is(kb_data))
  
})