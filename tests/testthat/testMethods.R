context("Test methods.")

data(exampleData)

test_that("Predict ap1: data with and without response variable, 
          comparison to lm, different order of columns,
          fail because of factor", {

  fit1 = mrfrequentist(dataMr = exampleData, approach = "1",
                       obsPerBlock = nrow(exampleData),
                       formula = V11 ~ .)
  fitLm = lm(V11 ~ ., data = exampleData)

  dat2 = subset(exampleData, select = -V11)
  dat3 = dat2[, sample(1:ncol(dat2), replace = FALSE)]

  pred1 = predict(fit1, exampleData)
  pred2 = predict(fit1, dat2)
  pred3 = predict(fit1, dat3)
  predLm = predict(fitLm)

  expect_equal(pred1, pred2)
  expect_equal(pred1, pred3)
  expect_equal(pred1, predLm)
  
  dat4 = exampleData
  dat4$V10 = factor(rbinom(1500, 1, 0.5))
  expect_error(predict(fit1, dat4))
  
})

test_that("Predict ap3: data with and without response variable, 
          comparison to lm, different order of columns,
          fail because of factor", {
            
            fit1 = mrfrequentist(dataMr = exampleData, approach = "3",
                                 obsPerBlock = nrow(exampleData),
                                 formula = V11 ~ .)
            fitLm = lm(V11 ~ ., data = exampleData)
            
            dat2 = subset(exampleData, select = -V11)
            dat3 = dat2[, sample(1:ncol(dat2), replace = FALSE)]
            
            pred1 = predict(fit1, exampleData)
            pred2 = predict(fit1, dat2)
            pred3 = predict(fit1, dat3)
            predLm = predict(fitLm)
            
            expect_equal(pred1, pred2)
            expect_equal(pred1, pred3)
            expect_equal(pred1, predLm)
            
            dat4 = exampleData
            dat4$V10 = factor(rbinom(1500, 1, 0.5))
            expect_error(predict(fit1, dat4))
            
            })

test_that("Summaries for approach 3 and original linear model are identical", {
  path = tempdir()
  files = list.files(path = path, pattern = "dat")
  
  if (length(files) == 0) {
    set.seed(214)
    makeTestData()
    files = list.files(path = path, pattern = "dat")
  }
  dat = data.table::fread(paste(path, "/", files[40], sep = ""),
                          fill = TRUE, col.names = c("del", "A", "B", "C", "D", "y"),
                          dec = ".", header = TRUE,
                          sep = "\t", data.table = FALSE)
  mod4 = mrfrequentist(formula = y ~ A + B + C + D, dataMr = dat, obsPerBlock = 480, approach = "3")
  lm4 = lm(formula = y ~ A + B + C + D, data = dat)
  expect_equal(summary(lm4)$coefficients, summary(mod4)$summaryStats)
})
