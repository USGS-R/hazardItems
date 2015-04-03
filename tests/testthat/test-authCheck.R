context("testing auth check")

test_that("check stale token", {
  setBaseURL('dev')
  expect_error(hazardItems:::checkAuth()) # not authenticated
  pkg.env$authToken = 'badtokenwillfail'
  expect_false(hazardItems:::checkAuth(updateIfStale = FALSE)) # bad token
  expect_error(hazardItems:::checkAuth(username = 'bad',password = 'bad')) # not authenticated
})
