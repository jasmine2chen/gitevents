test_that("get_repo-log() returns events for a repository on a date", {
  y <- get_repo_log("nganlyle", "534_project", "2020-01-26")
  expect_equal(ncol(y), 5)
})
