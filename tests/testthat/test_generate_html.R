context('generate_html working properly')

if(!exists('cgr_exp')) skip('Corrgrapher did not create the object')

if(dir.exists('temp_output')) unlink('temp_output', recursive = TRUE)
absolutePath <- file.path(getwd(), 'temp_output')
dir.create(absolutePath)
file.create(file.path(absolutePath, 'do_not_overwrite_me.'))
test_that('HTML generating with no errors',
         expect_output_file(generate_html(cgr_exp, file = file.path()),
                            file = 'temp_output/report.html'))

test_that('Existing file error handling',
         exprect_error(generate_html(cgr_exp, file = 'temp_output/do_not_overwrite_me.html')),
         expect_output_file(generate_html(cgr_exp, file = 'temp_output/report.html', overwrite = TRUE)))

unlink('temp_output', recursive = TRUE)
