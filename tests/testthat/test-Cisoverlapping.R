test_that("Cisoverlapping", {
  expect_false(Cisoverlapping(c(1,5),c(1,5)))

  expect_false(Cisoverlapping(1,2))

  expect_true(Cisoverlapping(c(1,1),c(2,2)))

  expect_true(Cisoverlapping(c(1,2),c(2,3)))
})

