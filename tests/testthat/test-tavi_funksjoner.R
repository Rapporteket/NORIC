test_that("legg til tavi status", {
  x <- data.frame(ePromStatus = 0:5)
  
  
  # Sjekk at navnene er riktige
  testthat::expect_equal(c("ePromStatus", "ePromStatus_tekst"),
                         x %>% 
                           noric::legg_til_taviStatus() %>%
                           names())
  
  # sjekk at de som skal bli NA blir det:
  testthat::expect_equal(5,
                         x %>% noric::legg_til_taviStatus() %>%
                           dplyr::filter(!is.na(ePromStatus_tekst)) %>% 
                        nrow())
  
  # Sjekk at alle labels er riktige
  testthat::expect_equal("created", 
                         x %>% noric::legg_til_taviStatus() %>% 
                           dplyr::filter(ePromStatus == 0) %>% 
                           dplyr::pull(ePromStatus_tekst))
 
  testthat::expect_equal("ordered", 
                         x %>% noric::legg_til_taviStatus() %>% 
                           dplyr::filter(ePromStatus == 1) %>% 
                           dplyr::pull(ePromStatus_tekst))
  
  testthat::expect_equal("expired", 
                         x %>% noric::legg_til_taviStatus() %>% 
                           dplyr::filter(ePromStatus == 2) %>% 
                           dplyr::pull(ePromStatus_tekst))
 
  testthat::expect_equal("completed", 
                         x %>% noric::legg_til_taviStatus() %>% 
                           dplyr::filter(ePromStatus == 3) %>% 
                           dplyr::pull(ePromStatus_tekst))
   
  testthat::expect_equal("failed", 
                         x %>% noric::legg_til_taviStatus() %>% 
                           dplyr::filter(ePromStatus == 4) %>% 
                           dplyr::pull(ePromStatus_tekst))
  
  testthat::expect_error(
    data.frame(tullenavn = 1:5) %>% noric::legg_til_taviStatus())
  
})
