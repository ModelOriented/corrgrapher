context('knit_print working properly')

if(!exists('cgr_exp')) skip('Corrgrapher did not create the object')

test_that('knit_print works',
          expect_message(knitr::knit(input = test_path('test.Rmd'),
                                    output = 'test.html'),
                         regexp = 'output file: test.html'))

file.remove('test.html')