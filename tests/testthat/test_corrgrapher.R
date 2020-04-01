context('corrgrapher working properly')

df <- as.data.frame(datasets::Seatbelts)[,1:7]
cgr <- corrgrapher(df)

test_that("Output type",{
  expect_is(cgr, 'corrgrapher')
  expect_true(all(c("nodes", "edges") %in% names(cgr)))
})

test_that('Incorrect argument x caught',{
  expect_error(corrgrapher(1:5))
})

test_that('Incorrect argument cutoff caught',{
  expect_error(corrgrapher(df, cutoff = 'ABC'))
  expect_error(corrgrapher(df, cutoff = 1:5))
})

test_that('Unusual argument cutoff recognised', {
  expect_warning(corrgrapher(df, cutoff = 2))
  expect_warning(corrgrapher(df, cutoff = -1))
})