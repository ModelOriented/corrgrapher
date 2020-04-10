context('corrgrapher working properly for explainers on numerical data')

options(check.attributes = FALSE)
set.seed(2020)

test_that(
  'Function is working properly with just necessary arguments',
  {
    expect_is({
      corrgrapher(tit_model_exp)
    },
    'corrgrapher')
  }
)


test_that('Partial_dependency argument working',{
  expect_equal({
    corrgrapher(tit_model_exp,
                feature_importance = tit_model_fi,
                partial_dependency = tit_model_pd)[['pds']]
  },
  tit_model_pd)
})

test_that('Partial_dependency_opts argument working',{
  expect_equal({
    corrgrapher(tit_model_exp,
                feature_importance = tit_model_fi,
                partial_dependency_opts = list(N = 100,
                                               grid_points = 81))[['pds']]
  },
  tit_model_pd,
  tolerance = 0.1)
})

test_that('Partial_dependency overrides partial_dependency_opts',{
  expect_warning(cgr <- corrgrapher(tit_model_exp,
                                    feature_importance = tit_model_fi,
                                    partial_dependency = tit_model_pd,
                                    partial_dependency_opts = list(N = 1000,
                                                                   grid_point = 41)))
  expect_equal(cgr$pds,
               tit_model_pd)
})

test_that('cor_functions argument working properly',{
  expect_equal(cgr <- corrgrapher(model_exp,
                                  feature_importance = model_fi,
                                  partial_dependency = model_pd_list,
                                  cor_functions = list(num_num_f = function(x, y) cor.test(titanic$fare, titanic$parch, method = 'spearman', exact = FALSE)[['estimate']] * 100,
                                                       num_cat_f = function(x, y) -log10(kruskal.test(x, y)[['p.value']]),
                                                       cat_cat_f = function(x, y) -log10(chisq.test(x, y)[['p.value']]),
                                                       max_cor = 100)))
})

test_that("Output type",{
  expect_is(tit_cgr_exp, 'corrgrapher')
  expect_true(all(c("nodes", "edges", "pds") %in% names(tit_cgr_exp)))
})

test_that('Incorrect argument partial_dependency caught', {
  expect_error(corrgrapher(tit_model_exp, 
                           feature_importance = tit_model_fi,
                           partial_dependency = 'ABC'),
               regexp = 'partial_dependency must be a list')
  expect_error(corrgrapher(tit_model_exp,
                           feature_importance = tit_model_fi,
                           partial_dependency = list(a = 'ABC')))
  expect_error(corrgrapher(tit_model_exp,
                           feature_importance = tit_model_fi,
                           partial_dependency = list(numerical = 'ABC')))
  expect_error(corrgrapher(tit_model_exp,
                           feature_importance = tit_model_fi,
                           partial_dependency = list(categorical = 'ABC')))
  expect_error(corrgrapher(tit_model_exp,
                           feature_importance = tit_model_fi,
                           partial_dependency = list(numerical = 'ABC',
                                                     categorical = 'ABC')))
})