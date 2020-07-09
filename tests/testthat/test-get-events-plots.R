test_that("get_events_plots() generates plots for github event type and payload",{
  y <-get_events_plots()
  expect_equal(length(y[[1]]$grobs), 3)
  expect_identical(y[[1]]$layout$clip[1], "off")
  expect_identical(y[[1]]$layout$name[1], "arrange")
})