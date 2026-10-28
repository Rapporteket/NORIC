test_that("legg til prom status", {
  x <- data.frame(ePromStatus = 0:6)
  
  
  # Sjekk at navnene er riktige
  testthat::expect_equal(c("ePromStatus", "ePromStatus_tekst"),
                         x %>% 
                           noric::legg_til_promStatus() %>%
                           names())
  
  # Sjekk at alle labels er riktige
  testthat::expect_equal("created", 
                         x %>% noric::legg_til_promStatus() %>% 
                           dplyr::filter(ePromStatus == 0) %>% 
                           dplyr::pull(ePromStatus_tekst))
 
  testthat::expect_equal("ordered", 
                         x %>% noric::legg_til_promStatus() %>% 
                           dplyr::filter(ePromStatus == 1) %>% 
                           dplyr::pull(ePromStatus_tekst))
  
  testthat::expect_equal("expired", 
                         x %>% noric::legg_til_promStatus() %>% 
                           dplyr::filter(ePromStatus == 2) %>% 
                           dplyr::pull(ePromStatus_tekst))
 
  testthat::expect_equal("completed", 
                         x %>% noric::legg_til_promStatus() %>% 
                           dplyr::filter(ePromStatus == 3) %>% 
                           dplyr::pull(ePromStatus_tekst))
   
  testthat::expect_equal("failed", 
                         x %>% noric::legg_til_promStatus() %>% 
                           dplyr::filter(ePromStatus %in% 4) %>% 
                           dplyr::pull(ePromStatus_tekst))
  
  testthat::expect_equal("failed", 
                         x %>% noric::legg_til_promStatus() %>% 
                           dplyr::filter(ePromStatus %in% 6) %>% 
                           dplyr::pull(ePromStatus_tekst))
  
  testthat::expect_equal("eprox hjerteinfarkt blokkerer utsending", 
                         x %>% noric::legg_til_promStatus() %>% 
                           dplyr::filter(ePromStatus == 5) %>% 
                           dplyr::pull(ePromStatus_tekst))
  
  testthat::expect_error(
    data.frame(tullenavn = 1:6) %>% noric::legg_til_promStatus())
  
})
