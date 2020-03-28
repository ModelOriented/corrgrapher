context('generate_html working properly')

library(DALEX)
library(randomForest)
library(ingredients)

data(dragons, package = 'DALEX')
model <- randomForest::randomForest(colour ~ ., data = dragons, num.trees = 50)
model_exp <- DALEX::explain(model, data = dragons[,-5], y = dragons$colour)
model_fi <- feature_importance(model_exp, type = 'raw', loss_function = loss_cross_entropy)

tryCatch(cgr_exp <- create_corrgrapher(model_exp, feature_importance = model_fi),
         error = function(e) {
           unlink('temp_output', recursive = TRUE)
           skip('Error in create_corrgrapher')})

dir.create('temp_output')
file.create('temp_output/do_not_overwrite_me.html')
test_that('HTML generating with no errors',
         expect_output_file(generate_html(cgr_exp, file = 'temp_output/report.html'),
                            file = 'temp_output/report.html'))

test_that('Existing file error handling',
         exprect_error(generate_html(cgr_exp, file = 'temp_output/do_not_overwrite_me.html')),
         expect_output_file(generate_html(cgr_exp, file = 'temp_output/report.html', overwrite = TRUE)))

unlink('temp_output', recursive = TRUE)
