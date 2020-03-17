context('create_corrgrapher working properly')

df <- as.data.frame(datasets::Seatbelts)[,1:7]
cgr <- create_corrgrapher(df)

test_that("Output type",{
  expect_is(cgr, 'corrgrapher')
  expect_true(all(c("nodes", "edges") %in% names(cgr)))
})

