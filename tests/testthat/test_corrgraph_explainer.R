context('corrgrapher working properly for explainers')

test_that('Function is not returning errors', {
  expect_is(corrgrapher(model_exp,
                            feature_importance = model_fi,
                            partial_dependency = model_pd), 'corrgrapher')
  # expect_is(corrgrapher(model_exp), 'corrgrapher')
  # expect_is(corrgrapher(model_exp,
  #                           feature_importance = model_fi), 'corrgrapher')
  # expect_silent(corrgrapher(model_exp,
  #                           partial_dependency = model_pd))
})
  
test_that("Output type",{
  expect_is(cgr_exp, 'corrgrapher')
  expect_true(all(c("nodes", "edges", "pds") %in% names(cgr_exp)))
})

test_that('Incorrect argument feature_importance caught', {
  expect_error(corrgrapher(model_exp, 
                           feature_importance = 'ABC'),
              regexp = 'feature_importance must be of feature_importance_explainer class')
})

test_that('Incorrect argument partial_dependency caught', {
  expect_error(corrgrapher(model_exp, 
                           feature_importance = model_fi,
                           partial_dependency = 'ABC'),
               regexp = 'partial_dependence must be of aggregated_profiles_explainer class')
})

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