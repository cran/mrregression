context("Tests for help functions.")

path = tempdir()
files = list.files(path = path, pattern = "dat\\^header")

if (length(files) == 0) {
  set.seed(214)
  makeTestData()
  files = list.files(path = path, pattern = "dat\\^header")
}

filename_noheader = list.files(path = path, pattern = "dat\\^headerFALSE\\^sep_\\^dec\\.\\^rownamesFALSE\\^skip0\\^nastringNA")
filepath_noheader = paste0(path, "/", filename_noheader)

dat = data.table::fread(filepath_noheader, sep = "_", header = FALSE, data.table = FALSE)

data(exampleData)

test_that("readBlock works correctly for dataMr", {
  
  j = 1
  commonheader2 = NULL
  
  while (j <= 2) {
    
    rB = list(currentBlock = NULL, dataMr = dat)
    rBMan = list(currentBlock = NULL, dataMr = dat)
    
    if (j == 1) {
      obsPerBlock = 500
    } else {
      obsPerBlock = 300
    }
    for (i in 1:ceiling(nrow(dat)/obsPerBlock)) {
      
      rB = readBlock(fileMr = NULL, dataMr = rB$dataMr, 
                     obsPerBlock = obsPerBlock)
      if (nrow(rBMan[["dataMr"]]) < obsPerBlock) {
        rBMan[["currentBlock"]] = rBMan[["dataMr"]][(1:nrow(rBMan[["dataMr"]])), ]
        rBMan[["dataMr"]] = rBMan[["dataMr"]][-(1:nrow(rBMan[["dataMr"]])), ]
      } else {
        rBMan[["currentBlock"]] = rBMan[["dataMr"]][(1:obsPerBlock), ]
        rBMan[["dataMr"]] = rBMan[["dataMr"]][-(1:obsPerBlock), ]
      }
      
      expect_equal(rB[["currentBlock"]], rBMan[["currentBlock"]])
      expect_equal(rB[["dataMr"]], rBMan[["dataMr"]])
      
    }
    j = j + 1
  }
})

test_that("readBlock works correctly for fileMr" , {
  
  j = 1
  
  while (j <= 2) {
    
    if (j == 1) {
      obsPerBlock = 500
    } else {
      obsPerBlock = 300
    }
    
    if (j == 1) {
      rB = list(currentBlock = NULL, dataMr = NULL)
      rBMan = list(currentBlock = NULL, dataMr = dat)
      con = file(filepath_noheader, open = "r")
      on.exit(close(con))
      
      commonheader2 = colnames(dat)
      while (length({txt = readLines(con = con, n = obsPerBlock)} ) > 0) {
        
        rB = readBlock(fileMr = filepath_noheader, dataMr = NULL, obsPerBlock = obsPerBlock, 
                       commonheader2 = commonheader2, dec = ".", naStrings = "NA", 
                       txt = txt, sep = "_")
        if (length(txt) < obsPerBlock) {
          rBMan[["currentBlock"]] = rBMan[["dataMr"]][(1:nrow(rBMan[["dataMr"]])), ]
          rBMan[["dataMr"]] = rBMan[["dataMr"]][-(1:nrow(rBMan[["dataMr"]])), ]
        } else {
          rBMan[["currentBlock"]] = rBMan[["dataMr"]][(1:obsPerBlock), ]
          rBMan[["dataMr"]] = rBMan[["dataMr"]][-(1:obsPerBlock), ]
        }
        
        expect_equivalent(rB[["currentBlock"]], rBMan[["currentBlock"]])
      }
      close(con)
    } else { # this is without colnames (commonheader2)
      rB = list(currentBlock = NULL, dataMr = NULL)
      rBMan = list(currentBlock = NULL, dataMr = dat)
      con = file(filepath_noheader, open = "r")
      on.exit(close(con))
      
      if (!is.null("commonheader2")) {
        commonheader2 = NULL
        while (length({txt = readLines(con = con, n = obsPerBlock)} ) > 0) {
          
          rB = readBlock(fileMr = filepath_noheader, dataMr = NULL, obsPerBlock = obsPerBlock, 
                         dec = ".", naStrings = "NA", commonheader2 = commonheader2,
                         txt = txt, sep = "_")
          
          if (is.null(rBMan[["currentBlock"]])) {
            colnames(rBMan[["dataMr"]]) = colnames(rB[["currentBlock"]])
          }
          if (length(txt) < obsPerBlock) {
            rBMan[["currentBlock"]] = rBMan[["dataMr"]][(1:nrow(rBMan[["dataMr"]])), ]
            rBMan[["dataMr"]] = rBMan[["dataMr"]][-(1:nrow(rBMan[["dataMr"]])), ]
          } else {
            rBMan[["currentBlock"]] = rBMan[["dataMr"]][(1:obsPerBlock), ]
            rBMan[["dataMr"]] = rBMan[["dataMr"]][-(1:obsPerBlock), ]
          }
          
          expect_equivalent(rB[["currentBlock"]], rBMan[["currentBlock"]])
          
        }
      }
    }
    j = j + 1
  }
  
})

test_that("Test that outputs of mergeFullLevels() and makeFinalModel() do not change, 
          ap 1.", {
            
  oldopt = options(max.print = 99999)
  on.exit(options(oldopt))
            
  approach = "1"
  l = 1
  fit1 = list(mrfrequentist(dataMr = exampleData[1:750, ], approach = approach, 
                            obsPerBlock = 750, formula = V11 ~ .))
  fit2 = mrfrequentist(dataMr = exampleData[751:1500, ], approach = approach, 
                       obsPerBlock = 1000, formula = V11 ~ .) 
  test1 = mergeFullLevels(models2 = fit1, l2 = l, tempM2 = fit2, 
                                    approach = approach)
  expect_equal(test1[["l"]], 2)
  expect_equal(test1[["models"]], list())
  expect_equal(test1[["tempM"]][["level"]], 2)
  expect_equal(test1[["tempM"]][["numberObs"]], 1500)
  expect_known_output(test1[["tempM"]][["summaryStats"]], 
                      file = "resTestHelpFunctionsNoChange1.rds", 
                      print = TRUE, update = FALSE)
            
  fit3 = mrfrequentist(dataMr = exampleData, approach = approach, obsPerBlock = 750, 
                       formula = V11 ~ .)
  fit3$dataHead = NULL
  fit3$formula = NULL
  test1b = makeFinalModel(mod = test1[["tempM"]], approach = approach, obsPerBlock = 750)
  expect_equal(fit3$approach, test1b$approach)
  expect_equal(fit3$level, test1b$level)
  expect_equal(fit3$numberObs, test1b$numberObs)
  expect_equal(fit3$summaryStats, test1b$summaryStats)
})        

test_that("Test that outputs of mergeFullLevels() and makeFinalModel() do not change, 
          ap 3.", {
            
  oldopt = options(max.print = 99999)
  on.exit(options(oldopt))        
  
  approach = "3"
  l = 1
  fit1 = list(mrfrequentist(dataMr = exampleData[1:750, ], approach = approach, 
                            obsPerBlock = 750, formula = V11 ~ .))
  fit2 = mrfrequentist(dataMr = exampleData[751:1500, ], approach = approach, 
                       obsPerBlock = 750, formula = V11 ~ .) 
  test3 = mergeFullLevels(models2 = fit1, l2 = l, tempM2 = fit2, 
                        approach = approach)
  expect_equal(test3[["l"]], 2)
  expect_equal(test3[["models"]], list())
  expect_equal(test3[["tempM"]][["level"]], 2)
  expect_equal(test3[["tempM"]][["numberObs"]], 1500)
  expect_known_output(test3[["tempM"]][["XTX"]],
                      file = "resTestHelpFunctionsNoChange3XTX.rds", 
                      print = TRUE, update = FALSE)
  expect_known_output(test3[["tempM"]][["yTX"]], 
                      file = "resTestHelpFunctionsNoChange3yTX.rds", 
                      print = TRUE, update = FALSE)
  expect_known_output(test3[["tempM"]][["yTy"]], 
                      file = "resTestHelpFunctionsNoChange3yTy.rds", 
                      print = TRUE, update = FALSE)
            
  fit3 = mrfrequentist(dataMr = exampleData, approach = approach, obsPerBlock = 750, 
                       formula = V11 ~ .)
  fit3$dataHead = NULL
  fit3$formula = NULL
  test3b = makeFinalModel(mod = test3[["tempM"]], approach = approach, 
                          obsPerBlock = 750, p = 11)
  test3b$betaHat = NULL
  test3b$seUnbiased = NULL
  test3b$seBiased = NULL
  expect_equal(fit3$approach, test3b$approach)
  expect_equal(fit3$level, test3b$level)
  expect_equal(fit3$numberObs, test3b$numberObs)
  expect_equal(fit3$XTX, test3b$XTX)
  expect_equal(fit3$yTX, test3b$yTX)
  expect_equal(fit3$yTy, test3b$yTy)
  expect_equal(fit3$summaryStats, test3b$summaryStats)
})


skip_on_cran()
skip_if_not_installed(pkg = "rstan")

test_that("Test that outputs of mergeFullLevels() and makeFinalModel() do not change, 
          ap 2.", {
            
  oldopt = options(max.print = 99999)
  on.exit(options(oldopt))
            
  approach = "2"
  l = 1
  fit1 = list(mrbayes(dataMr = exampleData[1:750, ], obsPerBlock = 750, y = "V11",
                      seed = 123, refresh = 0, verbose = FALSE))
  fit2 = mrbayes(dataMr = exampleData[751:1500, ], obsPerBlock = 750, y = "V11",
                 seed = 456, refresh = 0, verbose = FALSE)
  test2 = mergeFullLevels(models2 = fit1, l2 = l, tempM2 = fit2,
                          approach = approach)
  expect_length(test2, 3)
  expect_equal(test2[["l"]], 2)
  expect_equal(test2[["models"]], list())
  expect_length(test2[["tempM"]], 4)
  expect_equal(test2[["tempM"]][["level"]], 2)
  expect_equal(test2[["tempM"]][["numberObs"]], 1500)
  expect_equal(min(test2[["tempM"]][["diagnostics"]][, "n_eff"]) > 1500, TRUE)
  expect_equal(max(test2[["tempM"]][["diagnostics"]][, "Rhat"]) < 1.05, TRUE)
  
  test2Reference = readRDS(file = "resTestHelpFunctionsNoChange2.rds")
  expect_equal(test2, test2Reference, tolerance = 0.1)
  expect_equal(test2$models, test2Reference$models)
  expect_equal(test2$l, test2Reference$l)
  expect_equal(test2$tempM$level, test2Reference$tempM$level)
  expect_equal(test2$tempM$numberObs, test2Reference$tempM$numberObs)
  expect_equal(test2$tempM$summaryStats, test2Reference$tempM$summaryStats, tolerance = 0.001)
  expect_equal(test2$tempM$diagnostics, test2Reference$tempM$diagnostics, tolerance = 0.1)
            
  fit3 = mrbayes(dataMr = exampleData, obsPerBlock = 750, y = "V11", seed = 123,
                 refresh = 0, verbose = FALSE)
            
  fit3$dataHead = NULL
  fit3$modelCode = NULL
  test2b = makeFinalModel(mod = test2[["tempM"]], approach = approach, 
                          obsPerBlock = 750)
  expect_equal(fit3$approach, test2b$approach)
  expect_equal(fit3$level, test2b$level)
  expect_equal(fit3$numberObs, test2b$numberObs)
  expect_equal(fit3$summaryStats, test2b$summaryStats, tolerance = 0.0002)
})
