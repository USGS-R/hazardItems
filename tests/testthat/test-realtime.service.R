context("testing realtime.service")

test_that ("check if item exists", {
  meta <- system.file("extdata/Sandy_PCOI_metadata_updated2015May29.xml", package = "hazardItems")
  result <- realtime.service(serviceEndpoint = meta)
  result <- realtime.service(serviceEndpoint = meta, attribute = "PCOL")
  result <- realtime.service(serviceEndpoint = meta, attribute = "POVW")
  result <- realtime.service(serviceEndpoint = meta, attribute = "PIND")
  result <- realtime.service(serviceEndpoint = meta, attribute = "DHIGH")
  result <- realtime.service(serviceEndpoint = meta, attribute = "DLOW")
  result <- realtime.service(serviceEndpoint = meta, attribute = "MEAN")
  result <- realtime.service(serviceEndpoint = meta, attribute = "EXTREME")
  expect_true("exceedence" %in% result$full$text)
})