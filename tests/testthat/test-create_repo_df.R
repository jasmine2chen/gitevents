test_that("create_repo_df() creates df of repository events", {
  owner <- "nganlyle"
  repo <- "534_project"
  y <- create_repo_df(owner, repo)
  expect_equal(ncol(y), 6)
})
