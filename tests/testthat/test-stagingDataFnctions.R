testthat::test_that("bulletinProcessorStaging works", {
  
  
  
  # Wrong input, no dataset
  melding <- noric::bulletinProcessorStaging(dataset = "noe_annet_enn_ki", 
                                             orgName = "unknown organization",
                                             orgId = 999999,
                                             registryName = "noric",
                                             userFullName = "unknown person name",
                                             userRole = "unknown role",
                                             userOperator = "unknown operator", 
                                             rendered_by_shiny = FALSE, 
                                             author = "ingen")
  
  testthat::expect_equal(class(melding), "character")
  testthat::expect_equal(
    base::readLines(melding), 
    paste0("Denne bulletin'en laget ingen datasett. ", 
           "Sjekk ikke OK, ingen datasett er laget i dag"))
  
  
})


testthat::test_that("checkValidStagingData works",{
  
  # Wrong input returns empty list
  out <- noric::checkValidStagingData(registryName = "dette_er_ikke_register", 
                                      diffDaysCheck = 0)
  
  testthat::expect_equal(class(out), "list")
  testthat::expect_equal(out$valid_staging_data, FALSE)
  testthat::expect_equal(out$nyeste_staging_data, FALSE)
  
  
  
})
