test_that("adjust_pvalues appends a BH-adjusted column matching stats::p.adjust", {
  df <- data.frame(id = 1:5, cosinor_p_value = c(0.001, 0.02, 0.2, 0.5, 0.9))
  out <- adjust_pvalues(df, p_col = "cosinor_p_value", method = "BH")
  expect_true("cosinor_p_value_adj" %in% names(out))
  expect_equal(out$cosinor_p_value_adj,
               stats::p.adjust(df$cosinor_p_value, method = "BH"))
})

test_that("adjust_pvalues errors clearly when the column is missing", {
  df <- data.frame(id = 1:3)
  expect_error(adjust_pvalues(df, p_col = "nope"), regexp = "nope")
})

test_that("adjust_pvalues forwards the method argument", {
  df <- data.frame(cosinor_p_value = c(0.001, 0.02, 0.2, 0.5, 0.9))
  out <- adjust_pvalues(df, p_col = "cosinor_p_value", method = "bonferroni")
  expect_equal(out$cosinor_p_value_adj,
               stats::p.adjust(df$cosinor_p_value, method = "bonferroni"))
})
