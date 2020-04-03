library(ranger)
library(randomForest)
library(DALEX)

data(dragons, package='DALEX')
model <- ranger::ranger(colour ~ ., data = dragons, num.trees = 100, probability = TRUE)
model_exp <- DALEX::explain(model, data = dragons[,-5], y = dragons$colour)
model_fi <- ingredients::feature_importance(model_exp, loss_function = loss_accuracy, type = 'raw')
model_pd <- ingredients::partial_dependence(model_exp, N=100, grid_points = 81)

cgr_exp <- corrgrapher(model_exp, 
                       feature_importance = model_fi,
                       partial_dependency = model_pd)

data("freeny")
simple_model <- lm(y ~ ., data = freeny)
simple_model_exp <- DALEX::explain(simple_model, data = freeny[,-1], y = freeny$y)
print('helper ended!')
