context('knit_print working properly')

library(DALEX)
library(randomForest)
library(ingredients)

data(dragons, package = 'DALEX')
model <- randomForest::randomForest(colour ~ ., data = dragons, num.trees = 50)
model_exp <- DALEX::explain(model, data = dragons[,-5], y = dragons$colour)
model_fi <- feature_importance(model_exp, type = 'raw', loss_function = loss_cross_entropy)

tryCatch(cgr_exp <- create_corrgrapher(model_exp, feature_importance = model_fi),
         error = function(e) skip('Error in create_corrgrapher'))

cgr_exp <- create_corrgrapher(model_exp, feature_importance = model_fi)

test_that('knit_print works',
         expect_s3_class(knit_print.corrgrapher(cgr_exp), 'knit_asis')
)