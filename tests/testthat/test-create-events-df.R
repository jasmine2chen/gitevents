test_that("create_events_df() creates df of Github events",{
  y <- create_events_df()
  expect_equal(ncol(y),4)
  expect_equal(nrow(y),30)
})