context("Test merging of models")

# create two identical models and merge them
lmfit1 = stats::lm(V11 ~ ., data = exampleData)
lmmod1 = list(level = 1,
              numberObs = nrow(exampleData),
              summaryStats = coef(summary(lmfit1))[, c("Estimate", "Std. Error")],
              terms = terms(lmfit1))

lmfit2 = stats::lm(V11 ~ ., data = exampleData)
lmmod2 = list(level = 1,
              numberObs = nrow(exampleData),
              summaryStats = coef(summary(lmfit2))[, c("Estimate", "Std. Error")],
              terms = terms(lmfit1))

lmmod3 = mergeModels(lmmod1, lmmod2, l = lmmod1$level, approach = '1')

test_that("Merge identical models, approach 1", {
  expect_equal(lmmod1$summaryStats, lmmod3$summaryStats)
  expect_equal(lmmod1$numberObs + lmmod2$numberObs, lmmod3$numberObs)
  expect_equal(lmmod1$level + 1, lmmod3$level)
  expect_equal(lmmod2$terms, lmmod3$terms)
  expect_equal(lmmod1$terms, lmmod3$terms)
})

ap1mr1 = mrfrequentist(dataMr = exampleData, approach = "1", obsPerBlock = 1500, 
                       formula = V11 ~ .)
ap1mr2 = mrfrequentist(dataMr = exampleData, approach = "1", obsPerBlock = 1500, 
                       formula = V11 ~ .)
ap1mr3 = mergeModels(ap1mr1, ap1mr2, l = ap1mr1$level, approach = "1")

test_that("Merge identical models, approach 1", {
  expect_equal(ap1mr1, ap1mr2)
  expect_equal(ap1mr1$summaryStats, ap1mr3$summaryStats)
  expect_equal(ap1mr1$level + 1, ap1mr3$level)
  expect_equal(ap1mr1$numberObs + ap1mr2$numberObs, ap1mr3$numberObs)
  expect_equal(ap1mr1$terms, ap1mr3$terms)
  expect_equal(ap1mr2$terms, ap1mr3$terms)
})

ap2mr1 = mrbayes(dataMr = exampleData, obsPerBlock = 1500, y = "V11", refresh = 0, verbose = FALSE)
ap2mr2 = mrbayes(dataMr = exampleData, obsPerBlock = 1500, y = "V11", refresh = 0, verbose = FALSE)
ap2mr3 = mergeModels(ap2mr1, ap2mr1, l = ap2mr1$level, approach = "2")
ap2mr4 = mergeModels(ap2mr1, ap2mr2, l = ap2mr1$level, approach = "2")

test_that("Merge identical models, approach 2", {
  expect_equal(ap2mr1, ap2mr2, tolerance = 0.2)
  expect_equal(ap2mr1$summaryStats, ap2mr3$summaryStats)
  expect_equal(ap2mr1$level+1, ap2mr3$level)
  expect_equal(ap2mr1$numberObs + ap2mr2$numberObs, ap2mr3$numberObs)
  expect_equal(ap2mr1$summaryStats, ap2mr4$summaryStats, tolerance = 0.02)
  expect_equal(ap2mr1$level+1, ap2mr4$level)
  expect_equal(ap2mr1$numberObs + ap2mr2$numberObs, ap2mr4$numberObs)
})

ap3mr1 = mrfrequentist(dataMr = exampleData, approach = "3", obsPerBlock = 1500, 
                       formula = V11 ~ .)
ap3mr2 = mrfrequentist(dataMr = exampleData, approach = "3", obsPerBlock = 1500, 
                       formula = V11 ~ .)
ap3mr3 = mergeModels(ap3mr1, ap3mr2, l = ap3mr1$level, approach = "3")

test_that("Merge identical models, approach 3", {
  expect_equal(ap3mr1, ap3mr2)
  expect_equal(ap3mr1$XTX + ap3mr2$XTX, ap3mr3$XTX)
  expect_equal(ap3mr1$yTX + ap3mr2$yTX, ap3mr3$yTX)
  expect_equal(ap3mr1$yTy + ap3mr2$yTy, ap3mr3$yTy)
  expect_equal(ap3mr1$level + 1, ap3mr3$level)
  expect_equal(ap3mr1$numberObs + ap3mr2$numberObs, ap3mr3$numberObs)
  expect_equal(ap3mr1$terms, ap3mr3$terms)
  expect_equal(ap3mr2$terms, ap3mr3$terms)
})
