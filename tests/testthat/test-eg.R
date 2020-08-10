context("testing setting endpoints to CCHP tiers")

test_that("setting endpoints", {
  setBaseURL('prod')
  expect_equal(pkg.env$item_json, "https://marine.usgs.gov/coastalchangehazardsportal/data/item/")
  setBaseURL('qa')
  expect_equal(pkg.env$item_json, "https://cida-test.er.usgs.gov/coastalchangehazardsportal/data/item/")
  setBaseURL('dev')
  expect_equal(pkg.env$item_json, "https://cida-test.er.usgs.gov/dev/coastalchangehazardsportal/data/item/")
})


context("testing bad auth on QA and dev")

test_that("setting endpoints", {
  setBaseURL('qa')
  expect_error(authenticateUser('badUser','badPass'))
  setBaseURL('dev')
  expect_error(authenticateUser('badUser','badPass'))
})