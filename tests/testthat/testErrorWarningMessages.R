context("Check error messages and warnings in mrfrequentist and mrbayes.")

path = tempdir()
files = list.files(path = path, pattern = "dat\\^header")

if (length(files) == 0) {
  makeTestData()
  files = list.files(path = path, pattern = "dat\\^header")
}

filename = list.files(path = path, pattern = "dat\\^headerFALSE\\^sep\\ \\^dec\\.\\^rownamesFALSE\\^skip0\\^nastringNA")
fileMr = paste0(path, "/", filename)

data(exampleData)

obsPerBlock = 300
formula = V5 ~ .
y = "V5"

test_that("Expect error message because both fileMr and dataMr are not
          specified.", {
            expect_error(mrfrequentist(approach = "1", obsPerBlock = obsPerBlock,
                                       formula = formula),
                         "Please specify either fileMr or dataMr.")
            expect_error(mrfrequentist(approach = "3", obsPerBlock = obsPerBlock,
                                       formula = formula),
                         "Please specify either fileMr or dataMr.")
          })

test_that("Expect error message because both fileMr and dataMr are
          specified.", {
            expect_error(mrfrequentist(dataMr = exampleData,
                                       fileMr = fileMr, approach = "1",
                                       obsPerBlock = obsPerBlock, formula = formula,
                                       sep = " ", dec = ","),
                         "Both fileMr and dataMr given. Please specify only one of them.")
            expect_error(mrfrequentist(dataMr = exampleData,
                                       fileMr = fileMr, approach = "3",
                                       obsPerBlock = obsPerBlock, formula = formula,
                                       sep = " ", dec = ","),
                         "Both fileMr and dataMr given. Please specify only one of them.")
          })

test_that("Expect error message because dataMr is not a data.frame.", {
  expect_error(mrfrequentist(dataMr = as.matrix(exampleData),
                             approach = "1", obsPerBlock = obsPerBlock,
                             formula = formula, sep = " ", dec = ","),
               "dataMr must inherit from class data.frame.")
  expect_error(mrfrequentist(dataMr = as.matrix(exampleData),
                             approach = "3", obsPerBlock = obsPerBlock,
                             formula = formula, sep = " ", dec = ","),
               "dataMr must inherit from class data.frame.")
})

test_that("mrfrequentist: test singular fit checks", {
  expect_error(mrfrequentist(dataMr = exampleData, obsPerBlock = 4,
                             approach = "1", formula = formula),
               "singular fit encountered")
  expect_error(mrfrequentist(fileMr = fileMr, header = FALSE, obsPerBlock = 3,
                             approach = "1", formula =  formula),
               "singular fit encountered")
  expect_error(mrfrequentist(dataMr = exampleData, obsPerBlock = 4,
                             approach = "3", formula = formula),
               "Singular fit encountered.")
  expect_error(mrfrequentist(fileMr = fileMr, header = FALSE, obsPerBlock = 3,
                             approach = "3", formula = formula),
               "Singular fit encountered.")
})

test_that("mrfrequentist: errorMessagesObsPerBlock()", {
  expect_error(mrfrequentist(dataMr = exampleData, obsPerBlock = 11,
                             approach = "1", formula = formula),
               "Number of regression coefficients is larger than or equal to the number of observations in each block.")
  expect_error(mrfrequentist(fileMr = fileMr, header = FALSE, obsPerBlock = 5,
                             approach = "1", formula =  formula),
               "Number of regression coefficients is larger than or equal to the number of observations in each block.")
  expect_error(mrfrequentist(dataMr = exampleData, obsPerBlock = 11,
                             approach = "3", formula = formula),
               "Number of regression coefficients is larger than or equal to the number of observations in each block.")
  expect_error(mrfrequentist(fileMr = fileMr, header = FALSE, obsPerBlock = 5,
                             approach = "3", formula = formula),
               "Number of regression coefficients is larger than or equal to the number of observations in each block.")
})

test_that("mrfrequentist: test warningMessageObsPerBlock()", {
  expect_warning(mrfrequentist(dataMr = exampleData, obsPerBlock = 299,
                               approach = "1", formula = formula),
                 "Singular fit encountered in last block. Last block with 5 observations is not included in model.")
  expect_warning(mrfrequentist(fileMr = fileMr, header = FALSE, obsPerBlock = 222,
                               approach = "1", formula = formula),
                 "Singular fit encountered in last block. Last block with 2 observations is not included in model.")
  expect_warning(mrfrequentist(dataMr = exampleData, obsPerBlock = 299,
                               approach = "3", formula = formula),
                 "Singular fit encountered in last block. Last block with 5 observations is not included in model.")
  expect_warning(mrfrequentist(fileMr = fileMr, header = FALSE, obsPerBlock = 222,
                               approach = "3", formula = formula),
                 "Singular fit encountered in last block. Last block with 2 observations is not included in model.")
})

test_that("mrfrequentist: test warningMessageObsPerVariable()", {
  expect_warning(mrfrequentist(dataMr = exampleData, approach = "1",
                               obsPerBlock = 100, formula = V11 ~ .),
                 "Number of observations per regression coefficient <= 25 in all blocks. Models could be unreliable.")
  expect_warning(mrfrequentist(fileMr = fileMr, approach = "1", header = FALSE,
                               obsPerBlock = 100, formula = formula),
                 "Number of observations per regression coefficient <= 25 in all blocks. Models could be unreliable.")
})

test_that("mrfrequentist: error when factors variables are in dataMr", {
  dat = cbind(exampleData, factor(rbinom(n = nrow(exampleData), size = 1, prob = 0.5)))
  expect_error(mrfrequentist(formula = V11 ~ ., dataMr = dat, obsPerBlock = 500, approach = "1"), 
               "Predictors and target variable must be numeric.")
  expect_error(mrfrequentist(formula = V11 ~ ., dataMr = dat, obsPerBlock = 500, approach = "3"), 
               "Predictors and target variable must be numeric.")
  rm(dat)
})

skip_if_not_installed("rstan")

test_that("Expect error message because both fileMr and dataMr are not
          specified.", {
          expect_error(mrbayes(obsPerBlock = obsPerBlock, y = "V11"),
                       "Please specify either fileMr or dataMr.")
         })

test_that("Expect error message because both fileMr and dataMr are
          specified.", {
            expect_error(mrbayes(dataMr = exampleData, fileMr = fileMr,
                                 obsPerBlock = obsPerBlock, y = "V11"),
                         "Both fileMr and dataMr given. Please specify only one of them.")
          })

test_that("Expect error message because dataMr is not a data.frame.", {
  expect_error(mrbayes(dataMr = as.matrix(exampleData), obsPerBlock = obsPerBlock,
                       y = "V11"),
               "dataMr must inherit from class data.frame.")
})

test_that("mrbayes: test errorMessageObsPerBlock()", {
  expect_error(mrbayes(dataMr = exampleData, header = FALSE, obsPerBlock = 4,
                       intercept = TRUE, y = y),
               "Number of regression coefficients is larger than or equal to the number of observations in each block.")
  expect_error(mrbayes(fileMr = fileMr, header = FALSE, obsPerBlock = 3,
                       intercept = TRUE, y = y),
               "Number of regression coefficients is larger than or equal to the number of observations in each block.")
})

skip_on_cran()

test_that("mrbayes: test warningMessageObsPerBlock()", {
  expect_warning(mrbayes(dataMr = exampleData, obsPerBlock = 299, y = "V11",
                         refresh = 0, verbose = FALSE),
                 "Fewer observations than coefficients in last block. Last block with 5 observations is not included in model.")
  expect_warning(mrbayes(fileMr = fileMr, header = FALSE, obsPerBlock = 222,
                         y = y, refresh = 0, verbose = FALSE),
                 "Fewer observations than coefficients in last block. Last block with 2 observations is not included in model.")
})

test_that("mrbayes: test warningMessageObsPerVariable()", {
  expect_warning(mrbayes(dataMr = exampleData, obsPerBlock = 100,
                         intercept = TRUE, y = "V11", refresh = 0,
                         verbose = FALSE),
                 "Number of observations per regression coefficient <= 25 in all blocks. Models could be unreliable.")
  expect_warning(mrbayes(fileMr = fileMr, obsPerBlock = 100,
                         intercept = TRUE, y = y, refresh = 0,
                         verbose = FALSE),
                 "Number of observations per regression coefficient <= 25 in all blocks. Models could be unreliable.")
})
