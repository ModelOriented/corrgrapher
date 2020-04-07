context('plot_distribution working properly')

test_that('plot_distibution working properly',{
  expect_is(plot_distribution(dragons$height, 'height'), 'gg')
  expect_is(plot_distribution(dragons$colour, 'colour'), 'gg')
})