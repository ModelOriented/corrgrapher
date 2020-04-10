context('create_tabset working properly')

test_that('create_tabset working properly with numerical data', {
  expect_is(create_tabset(cgr_exp), 'shiny.tag')
  expect_is(create_tabset(cgr_df), 'shiny.tag')
})

test_that('create_tabset working properly with mixed data', {
  expect_is(create_tabset(tit_cgr_exp), 'shiny.tag')
  expect_is(create_tabset(cgr_df_mixed), 'shiny.tag')
})
