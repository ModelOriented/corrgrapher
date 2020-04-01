context('generate_html working properly')

if(!exists('cgr_exp')) skip('Corrgrapher did not create the object')
abs_path_to_dir <- file.path(getwd(), 'temp_output')

if(dir.exists(abs_path_to_dir)) unlink('temp_output', recursive = TRUE)
dir.create(abs_path_to_dir)
file.create(file.path(abs_path_to_dir, 'do_not_overwrite_me.html'))
abs_path_to_file <- file.path(abs_path_to_dir, 'report.html')

test_that('HTML generating with no errors',
         expect_true({
           generate_html(cgr_exp, file = abs_path_to_file)
           file.exists(abs_path_to_file)}))

test_that('Existing file error handling',{
         expect_error(generate_html(cgr_exp, 
                                     file = file.path(abs_path_to_dir, 'do_not_overwrite_me.html')))
         expect_silent(generate_html(cgr_exp, 
                                          file = abs_path_to_file, 
                                          overwrite = TRUE))
})
unlink(abs_path_to_dir, recursive = TRUE)
