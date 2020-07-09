test_that("get_events_lists() generates lists for github event actor and repo",{
  y <- create_events_df()
  expect_equal(ncol(y),4)
  expect_equal(nrow(y),30)
})
