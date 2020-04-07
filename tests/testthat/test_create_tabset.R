context('create_tabset working properly')

data("Seatbelts")
df <- as.data.frame(Seatbelts)[,-8]
cgr_df <- corrgrapher(df)
test_that('create_tabset working properly for explainers',
          expect_is(create_tabset(cgr_exp), 'shiny.tag'))

test_that('create_tabset working properly for data.frame',
          expect_is(create_tabset(cgr_df), 'shiny.tag'))