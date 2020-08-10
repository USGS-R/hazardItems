context("testing checkItemExists")

test_that ("check if item exists", {
  setBaseURL("prod")
  expect_false(checkItemExists("CHEX123")) # bad itemID
  expect_true(checkItemExists("CCGftiy")) # good itemID
  
})