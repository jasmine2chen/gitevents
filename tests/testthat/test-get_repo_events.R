test_that("get_repo_events() can access GitHub events API", {
  owner <- "nganlyle"
  repo <- "534_project"
  y <- get_repo_events(owner, repo)
  expect_equal(length(y), 3)
  expect_equal(y$response$status_code, 200)
})
