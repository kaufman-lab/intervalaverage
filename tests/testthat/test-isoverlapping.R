test_that("is.overlapping", {

  x <- data.table(a=c(1L,5L),b=c(2L,NA))
  expect_error(is.overlapping(x, c("a","b")),
               "values in columns specified by interval_vars must be non-missing")



  x <- data.table(a=c(NA,5L),b=c(2L,5L))
  expect_error(is.overlapping(x, c("a","b")),
               "values in columns specified by interval_vars must be non-missing")


  x <- data.table(a=c(5L,1L),b=c(2L,1L))
  expect_error(is.overlapping(x, c("a","b")),
               "interval_vars must specify columns corresponding to increasing intervals")


  x <- data.table(a=c(1L,5L),b=c(6L,10L))
  expect_true(is.overlapping(x, c("a","b")))


  x <- data.table(a=c(5L,1L),b=c(10L,6L))
  expect_true(is.overlapping(x, c("a","b")))


  x <- data.table(a=c(1L,3L,5L,-3L),b=c(2L,4L,6L,2L))
  expect_true(is.overlapping(x, c("a","b")))


  x <- data.table(a=c(1L,3L,5L,20L,7L,-1e9L),b=c(2L,4L,6L,300000L,10L,0L))
  expect_false(is.overlapping(x, c("a","b")))



  x <- data.table(a=c(1L,3L,5L,20L,7L,-1e9L),b=c(2L,4L,6L,300000L,10L,0L),id=c(1,1,1,2,2,2))
  expect_false(is.overlapping(x, c("a","b"),group_vars="id"))


  x <- data.table(a=c(1L,3L,5L,20L,7L,-1e9L),b=c(2L,4L,1e9L,300000L,10L,0L),id=c(1,1,1,2,2,2))
  expect_false(is.overlapping(x, c("a","b"),group_vars="id"))


  x <- data.table(a=c(1L,3L,5L,20L,7L,-1e9L),b=c(20L,4L,1e9L,300000L,10L,0L),id=c(1,1,1,2,2,2))
  expect_true(is.overlapping(x, c("a","b"),group_vars="id"))


})
