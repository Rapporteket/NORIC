test_that("function returns kable objects", {
  expect_true("kableExtra" %in% class(mst(tab = mtcars[1:10, ],
                                          type = "html")))
  expect_true("knitr_kable" %in% class(mst(tab = mtcars[1:10, ])))
  expect_true("matrix" %in% class(prettyTab(as.matrix(mtcars[1:10, ]))))
})