context('save_cgr_to_html working properly')

skip('Deal with it later')

if(!exists('cgr_exp')) skip('Corrgrapher did not create the object')
abs_path_to_dir <- file.path(getwd(), 'temp_output')

if(dir.exists(abs_path_to_dir)) unlink('temp_output', recursive = TRUE)
dir.create(abs_path_to_dir)
file.create(file.path(abs_path_to_dir, 'do_not_overwrite_me.html'))
abs_path_to_file <- file.path(abs_path_to_dir, paste0('report', 1:4, '.html'))

test_that('HTML generating with no errors',{
         expect_true({
           save_cgr_to_html(cgr_exp, file = abs_path_to_file[1])
           file.exists(abs_path_to_file)})
         expect_true({
           save_cgr_to_html(tit_cgr_exp, file = abs_path_to_file[2])
           file.exists(abs_path_to_file)})
         expect_true({
           save_cgr_to_html(cgr_df, file = abs_path_to_file[3])
           file.exists(abs_path_to_file)})
         expect_true({
           save_cgr_to_html(cgr_df_mixed, file = abs_path_to_file[4])
           file.exists(abs_path_to_file)})
})

test_that('Existing file error handling',{
         expect_error(save_cgr_to_html(cgr_exp, 
                                     file = file.path(abs_path_to_dir, 'do_not_overwrite_me.html')))
         expect_silent(save_cgr_to_html(cgr_exp, 
                                          file = abs_path_to_file[1], 
                                          overwrite = TRUE))
})
unlink(abs_path_to_dir, recursive = TRUE)
