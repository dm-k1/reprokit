test_that("impossible requests signal a classed error with alternatives", {
  cnd <- expect_error(
    abort_impossible("refit a logistic multilevel model", "the likelihood needs the raw data"),
    class = "reprokit_impossible"
  )
  expect_true(length(cnd$alternatives) > 0)
  expect_match(conditionMessage(cnd), "DataSHIELD")
})
