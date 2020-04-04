context('insert_image working properly')
library(ggplot2)
data("dragons", package = 'DALEX')
pl <- ggplot(dragons, aes(x = height,
                          y = weight)) +
  geom_point()

absolutePath <- getwd()
if(file.exists(file.path(absolutePath, 'plt.png'))) file.remove(file.path(absolutePath, 'plt.png'))
test_that('insert_image not throwing errors',{
  expect_is(insert_image(pl, tf = file.path(absolutePath, 'plt.png')), 'shiny.tag')
})

test_that('cleans after itself',{
  expect_true({
    insert_image(pl, tf = file.path(absolutePath, 'remove_me.png'))
    !file.exists(file.path(absolutePath, 'remove_me.png'))
  })
})

test_that('handles existing file error',{
  expect_error({
    file.create(file.path(absolutePath, 'dont_append_me.png'))
    insert_image(pl, file.path(absolutePath('dont_append_me.png')))
    })
})
file.remove(file.path(absolutePath, 'dont_append_me.png'))