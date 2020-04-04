context('knit_print working properly')

if(!exists('cgr_exp')) skip('Corrgrapher did not create the object')

test_that('knit_print works',
         expect_s3_class(knit_print.corrgrapher(cgr_exp), 'knit_asis')
)