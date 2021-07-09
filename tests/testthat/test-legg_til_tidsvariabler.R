test_that("Only NA's if wrong date-format", {
  x = data.frame(a = c("2020-31-01", "2020-01-31"))
  expect_true(all(is.na(legg_til_tidsvariabler(x, var = a)[1, c("aar",
                                                                "kvartal")])))

  expect_true(all(legg_til_tidsvariabler(x, var = a)[1, c("maaned_nr",
                                                          "uke")] == "NA"))

  expect_true(all(legg_til_tidsvariabler(x, var = a)[1, c("maaned",
                                                        "aar_uke")] == "NA-NA"))
})

test_that("New variables are correct", {

  x = data.frame(ProsedyreDato = c("2016-01-01", "2020-01-31", "2017-05-19"))


  expect_true(all(legg_til_tidsvariabler(x)$aar == c(2016, 2020, 2017)))
  expect_true(all(legg_til_tidsvariabler(x)$maaned_nr == c("01", "01", "05")))
  expect_true(all(legg_til_tidsvariabler(x)$maaned == c("2016-01",
                                                        "2020-01",
                                                        "2017-05")))
  expect_true(all(legg_til_tidsvariabler(x)$uke == c("53", "05", "20")))

})

test_that("Number and names of new variables are correct", {

  x = data.frame(ProsedyreDato = c("2016-01-01", "2020-01-31", "2017-05-19"))
  expect_true(nrow(legg_til_tidsvariabler(x)) == 3)
  expect_true(ncol(legg_til_tidsvariabler(x)) == 7)
  expect_true(all(names(legg_til_tidsvariabler(x)) == c("ProsedyreDato",
                                                        "aar",
                                                        "maaned_nr",
                                                        "maaned",
                                                        "kvartal",
                                                        "uke",
                                                        "aar_uke")))
})
