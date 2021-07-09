test_that("test that non-valid AvdRESH are deleted", {
  x <- data.frame(
    AvdRESH =  c(rep(1234, 2)),
    ProsedyreDato = as.Date(c("2019-01-01",
                              "2001-05-09"),
                            format = "%Y-%m-%d"))
   expect_true(nrow(fjerne_tulleregistreringer(x,  var = ProsedyreDato)) == 0)
  })


test_that("test that non-valid dates are deleted", {
  x <- data.frame(
    AvdRESH =  c(rep(102966, 2), rep(104284, 2)),
    ProsedyreDato = as.Date(c("2006-09-30",
                              "2013-01-01",
                              "2020-08-26",
                              "2013-01-31"),
                            format = "%Y-%m-%d"))

  expect_true(nrow(fjerne_tulleregistreringer(x, var = ProsedyreDato)) == 2)
})

test_that("test that error message is produced when missing variables", {
  x <- data.frame(
    AvdRESH =  c(rep(102966, 2), rep(104284, 2)),
    toto = as.Date(c("2006-09-30",
                     "2013-01-01",
                     "2020-08-26",
                     "2013-01-31"),
                   format = "%Y-%m-%d"))

  y <- data.frame(
    toto =  c(rep(102966, 2), rep(104284, 2)),
    ProsedyreDato = as.Date(c("2006-09-30",
                              "2013-01-01",
                              "2020-08-26",
                              "2013-01-31"),
                            format = "%Y-%m-%d"))
  expect_error(fjerne_tulleregistreringer(x, var = ProsedyreDato))
  expect_error(fjerne_tulleregistreringer(y, var = ProsedyreDato))

  })
