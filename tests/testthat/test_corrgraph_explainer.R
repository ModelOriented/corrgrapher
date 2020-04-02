context('corrgrapher working properly for explainers')

options(check.attributes = FALSE)
set.seed(2020)

custom_values <- data.frame(label = colnames(dragons)[-5],
                            value = rep(15, ncol(dragons) - 1))
custom_values <- custom_values[order(custom_values$label),]
rownames(custom_values) <- NULL

expected_fi <- model_fi[model_fi$permutation == 0 &
                          substr(model_fi$variable, 1, 1) != '_',
                        c('variable', 'dropout_loss')]
class(expected_fi) <- 'data.frame'
colnames(expected_fi) <- c('label', 'value')
expected_fi <- expected_fi[order(expected_fi$label),]
expected_fi$label <- factor(expected_fi$label)

expected_pd <- split(model_pd, model_pd[['_vname_']])

test_that(
  'Function is working properly with just necessary arguments',
  {
    expect_is({
      corrgrapher(simple_model_exp)
    },
    'corrgrapher')
  }
)

test_that('Values argument working', {
  expect_equal({
    df <- corrgrapher(model_exp,
                      values = custom_values,
                      partial_dependency = model_pd)[['nodes']][, c('label', 'value')]
    df[order(df$label),]
  },
  custom_values)})

test_that('Values argument overrides feature_importance_*',{
  expect_warning(
    cgr <- corrgrapher(
      model_exp,
      values = custom_values,
      feature_importance = model_fi,
      partial_dependency = model_pd
    )
  )
  expect_equal({
    df <- cgr[['nodes']][, c('label', 'value')]
    df <- df[order(df$label), ]
    rownames(df) <- NULL
    df
  },
  custom_values)
  expect_warning(
    cgr <- corrgrapher(
      model_exp,
      values = custom_values,
      feature_importance_opts = list(),
      partial_dependency = model_pd
    )
  )
  expect_equal({
    df <- cgr[['nodes']][,c('label','value')]
    df <- df[order(df$label),]
    rownames(df) <- NULL
    df
  },
  custom_values)
})

test_that('Feature_importance argument working',{
  expect_equal({
    cgr <- corrgrapher(model_exp,
                       feature_importance = model_fi,
                       partial_dependency = model_pd)
    df <- cgr$nodes[,c('label','value')]
    df[order(df$label),]
  },
  expected_fi,
  check.attributes = FALSE)
})

test_that('Feature_importance_opts argument working',{
  expect_equal({
    cgr <- corrgrapher(model_exp,
                       feature_importance_opts = list(loss_function = DALEX::loss_accuracy, 
                                                      type = 'raw'),
                       partial_dependency = model_pd)
    df <- cgr$nodes[,c('label','value')]
    df[order(df$label),]
  },
  expected_fi,
  check.attributes = FALSE)
})

test_that('Partial_dependency argument working',{
  expect_equal({
    corrgrapher(model_exp,
    feature_importance = model_fi,
    partial_dependency = model_pd)[['pds']]
    },
    expected_pd)
})

test_that('Partial_dependency_opts argument working',{
  expect_equal({
    corrgrapher(model_exp,
                feature_importance = model_fi,
                partial_dependency_opts = list(N = 100,
                                               grid_points = 81))[['pds']]
  },
  expected_pd,
  tolerance = 0.1)
})

test_that('Partial_dependency overrides partial_dependency_opts',{
  expect_warning(cgr <- corrgrapher(model_exp,
                             feature_importance = model_fi,
                             partial_dependency = model_pd,
                             partial_dependency_opts = list(N = 1000,
                                                            grid_point = 41)))
  expect_equal(cgr$pds,
               expected_pd)
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
               regexp = 'partial_dependency must be of aggregated_profiles_explainer class')
})