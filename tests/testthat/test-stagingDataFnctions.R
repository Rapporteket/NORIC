# store current instance
currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

# test data
registryName <- "testReg"
Sys.setenv(R_RAP_CONFIG_PATH = tempdir())
dataName <- "testData"
d <- mtcars
testPath <- file.path(
  Sys.getenv("R_RAP_CONFIG_PATH"),
  "stagingData",
  registryName
)
testFile <- file.path(testPath, dataName)



testthat::test_that("makeStagingDataFrame when empty", {

  rapbase::cleanStagingData(0)
  out <- noric::makeStagingDataFrame(registryName = "testReg")

  testthat::expect_equal(class(out), "data.frame")
  testthat::expect_equal(names(out),
                         c("Staging data",
                           "Dato"))

  testthat::expect_true(out %>% nrow() %in% 0)
})


testthat::test_that("makeStagingDataFrame when not empty", {

  # Make one entry, and check
  rapbase::cleanStagingData(0)
  rapbase::saveStagingData(registryName, dataName, d)
  out <- noric::makeStagingDataFrame(registryName = "testReg")

  testthat::expect_equal(class(out), "data.frame")
  testthat::expect_equal(names(out),
                         c("Staging data",
                           "Dato"))
  testthat::expect_true(out %>% nrow() %in% 1)
})



testthat::test_that("checkValidStagingData works when empty", {

  # Wrong input returns empty list
  out <- noric::checkValidStagingData(registryName = "dette_er_ikke_register",
                                      diffDaysCheck = 0)

  testthat::expect_equal(class(out), "list")
  testthat::expect_false(out$valid_staging_data)
  testthat::expect_false(out$nyeste_staging_data)

})


testthat::test_that("checkValidStagingData works when not empty", {

  rapbase::cleanStagingData(0)
  rapbase::saveStagingData(registryName, dataName, d)

  out <- noric::checkValidStagingData(registryName = registryName,
                                      diffDaysCheck = 0)


  testthat::expect_equal(class(out), "list")
  testthat::expect_true(out$valid_staging_data)
  testthat::expect_equal(out$nyeste_staging_data,
                         expected = "testData")


})


testthat::test_that("deleteOldStagingData works when empty", {
  rapbase::cleanStagingData(0)
  out1 <- noric::deleteOldStagingData(registryName = "ikke_et_register",
                                     diffDaysDelete = 0)

  testthat::expect_equal(class(out1),
                         "NULL")

})


testthat::test_that("deleteOldStagingData works when not empty", {
  rapbase::cleanStagingData(0)

  rapbase::saveStagingData(registryName, dataName, d)
  noric::deleteOldStagingData(registryName = registryName,
                                      diffDaysDelete = (-1))
  out <- noric::makeStagingDataFrame(registryName = registryName)
  testthat::expect_true(out %>% nrow() %in% 0)

})

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
    melding,
    paste0("Denne bulletin'en laget ingen datasett. ",
           "Sjekk ikke OK, ingen datasett er laget i dag"))


})




# Restore environment
Sys.setenv(R_RAP_CONFIG_PATH = currentConfigPath)
