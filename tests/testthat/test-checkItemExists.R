context("testing checkItemExists")

test_that ("check if item exists", {
  setBaseURL("dev")
  expect_false(hazardItems:::checkItemExists("CHEX123")) # bad itemID
  expect_true(hazardItems:::checkItemExists("CAQw7M1")) # good itemID
  
})