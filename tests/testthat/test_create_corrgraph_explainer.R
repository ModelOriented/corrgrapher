context('create_corrgrapher working properly for explainers')

library(DALEX)
library(randomForest)
library(ingredients)

data(dragons, package = 'DALEX')
model <- randomForest(colour ~ ., data = dragons, num.trees = 50)
model_exp <- explain(model, data = dragons[,-5], y = dragons$colour)
model_fi <- feature_importance(model_exp, type = 'raw', loss_function = loss_cross_entropy)
cgr <- create_corrgrapher(model_exp, feature_importance = model_fi)

# data('fifa20', package = 'CorrGrapheR')
# fifa20_selected <- fifa20[,c(4,5,7,8,11:13,17,25:26,45:78)]
# # Value is skewed. Will be much easier to model sqrt(Value).
# 
# fifa20_selected$value_eur <- log10(fifa20_selected$value_eur)
# fifa20_selected$team_position <- factor(fifa20_selected$team_position)
# fifa20_selected <- na.omit(fifa20_selected)
# fifa20_selected <- fifa20_selected[fifa20_selected$value_eur > 0,]
# fifa20_selected <- fifa20_selected[!duplicated(fifa20_selected[,1]),]
# rownames(fifa20_selected) <- fifa20_selected[,1]
# fifa20_selected <- fifa20_selected[,-1]
# 
# # create a gbm model
# 
# set.seed(1313)
# 
# # 4:5 are overall and potential, too strong predictors
# fifa_gbm <- gbm::gbm(value_eur ~ . , data = fifa20_selected[,-(4:5)], n.trees = 250, interaction.depth = 3)
# 
# 
# fifa_gbm_exp <- DALEX::explain(fifa_gbm, 
#                         data = fifa20_selected[, -6], 
#                         y = 10^fifa20_selected$value_eur, 
#                         predict_function = function(m,x) 
#                           10^predict(m, x, n.trees = 250))
# 
# 
# fifa_feat <- ingredients::feature_importance(fifa_gbm_exp)

test_that("Output type",{
  expect_is(cgr_exp, 'corrgrapher')
  expect_true(all(c("nodes", "edges") %in% names(cgr_exp)))
})

test_that('Incorrect argument feature_importance caught', {
  expect_error(create_corrgrapher(model_exp, feature_importance = 'ABC'))
})

