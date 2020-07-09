test_that("get_git_events() accesses Github public events",{
  y <- get_git_events()
  expect_equal(length(y), 2)
  expect_equal(y$response$status_code, 200)
  expect_identical(typeof(y$content),'list')
})
