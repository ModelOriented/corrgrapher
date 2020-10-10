context('utils working properly')

# wrap_with_html_tag --------------------------------------- 

test_that('wrap_with_html_tag working properly with numerical data', {
  expect_is(wrap_with_html_tag(cgr_exp), 'shiny.tag')
  expect_is(wrap_with_html_tag(cgr_df), 'shiny.tag')
})

test_that('wrap_with_html_tag working properly with mixed data', {
  expect_is(wrap_with_html_tag(tit_cgr_exp), 'shiny.tag')
  expect_is(wrap_with_html_tag(cgr_df_mixed), 'shiny.tag')
})

# plot_distribution ---------------------------------

test_that('plot_distibution working properly',{
  expect_is(plot_distribution(dragons$height, 'height'), 'gg')
  expect_is(plot_distribution(dragons$colour, 'colour'), 'gg')
})