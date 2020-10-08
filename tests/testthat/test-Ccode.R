test_that("Cisoverlapping", {

  x <- c(8e130, 3.141593e-130, 3.141593e-135, 3.141593e-140)
  out <- intervalaverage:::Cintervalaverage(values_list=list(a=x),
                   start_vector=c(1L,6L,11L),
                   end_vector=c(5L,10L,15L),
                   start_scalar=1L,
                   end_scalar=15L,
                   value_names=c("a","nobs_a","maxgap_a"))


  expect_identical(out$a,weighted.mean(x))

  #out$a is 2.66667e130 but it shoudl be 2e130
   #it's like sum(x) is being divided by 3 not 4. why?

})
