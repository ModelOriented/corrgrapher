context('encode_image working properly')
library(ggplot2)
data("dragons", package = 'DALEX')
pl <- ggplot(dragons, aes(x = height,
                          y = weight)) +
  geom_point()

absolutePath <- getwd()
container_id <- 'cgr_id_123'
if(file.exists(file.path(absolutePath, 'plt.png'))) file.remove(file.path(absolutePath, 'plt.png'))
<<<<<<< HEAD
test_that('encode_image not throwing errors',{
  expect_is(encode_image(pl, tf = file.path(absolutePath, 'plt.png')), 'shiny.tag')
=======
test_that('insert_image not throwing errors',{
  expect_is(insert_image(pl, container_id, tf = file.path(absolutePath, 'plt.png')), 'shiny.tag')
>>>>>>> static_plots
})

test_that('cleans after itself',{
  expect_true({
<<<<<<< HEAD
    encode_image(pl, tf = file.path(absolutePath, 'remove_me.png'))
=======
    insert_image(pl, container_id, tf = file.path(absolutePath, 'remove_me.png'))
>>>>>>> static_plots
    !file.exists(file.path(absolutePath, 'remove_me.png'))
  })
})

test_that('handles existing file error',{
  expect_error({
    file.create(file.path(absolutePath, 'dont_append_me.png'))
<<<<<<< HEAD
    encode_image(pl, file.path(absolutePath('dont_append_me.png')))
=======
    insert_image(pl, container_id, file.path(absolutePath('dont_append_me.png')))
>>>>>>> static_plots
    })
})
file.remove(file.path(absolutePath, 'dont_append_me.png'))