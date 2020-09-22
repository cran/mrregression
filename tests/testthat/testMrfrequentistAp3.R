context("Test mrfrequentist approach 3")

approach = "3"

path = tempdir()
files = list.files(path = path, pattern = "dat\\^header")
if (length(files) == 0) {
  set.seed(214)
  makeTestData()
  files = list.files(path = path, pattern = "dat\\^header")
}

filename_header = list.files(path = path, pattern = "dat\\^headerTRUE\\^sep_\\^dec\\.\\^rownamesFALSE\\^skip0\\^nastringNA")
filename_noheader = list.files(path = path, pattern = "dat\\^headerFALSE\\^sep_\\^dec\\.\\^rownamesFALSE\\^skip0\\^nastringNA")

filepath_header = paste0(path, "/", filename_header)
filepath_noheader = paste0(path, "/", filename_noheader)

dataframe_header = data.table::fread(filepath_header, sep = "_", data.table = FALSE)
datatable_header = data.table::as.data.table(dataframe_header)

data(exampleData)

test_that("Test class, known output and data.table/data.frame as input.", {
  
  modDataframe = mrfrequentist(formula = y ~ ., dataMr = dataframe_header, 
                               obsPerBlock = 300, approach = approach)
  modDatatable = mrfrequentist(formula = y ~ ., dataMr = datatable_header, 
                               obsPerBlock = 300, approach = approach)
  
  fitExampleData = mrfrequentist(formula = V11 ~ ., dataMr = exampleData, 
                                 obsPerBlock = 500, approach = approach)
  
  expect_known_output(object = fitExampleData, file = "resTestMrfrequentistAp3.rds",
                      print = TRUE, update = FALSE)
  
  expect_is(object = modDataframe, class = "mrfrequentist")
  
  expect_equivalent(object = modDataframe, expected = modDatatable)
  
})

test_that("Reading in: same results for all data sets (separators, header, rownames,
          dataMr, fileMr, obsPerBlock = 300).", {
            
            obsPerBlock = 300
            
            mod1 = mrfrequentist(dataMr = dataframe_header,
                                 approach = approach,
                                 obsPerBlock = obsPerBlock, formula = y ~ .)          
            
            mod1dataHead = mod1$dataHead
            mod1$dataHead = NULL
            
            mod1$formula = NULL
            mod1$terms = NULL                    
            
            mod_dat = vector("list", 40)
            mod_file = vector("list", 40)
            
            for(i in 1:(length(files))){
              
              posHeader = regexpr("header", files[i])[1]+6
              header = as.logical(unlist(strsplit(files[i], "^"))[posHeader])
              
              posDec = regexpr("dec", files[i])[1]+3
              dec = unlist(strsplit(files[i], "^"))[posDec]
              
              posSep = regexpr("sep", files[i])[1]+3
              sep = unlist(strsplit(files[i], "^"))[posSep]
              
              posRownames = regexpr("rownames", files[i])[1]+8
              rownames = as.logical(unlist(strsplit(files[i], "^"))[posRownames])
              
              if (header && !rownames) {
                formula = y ~ .
              } else if (header && rownames) {
                colnam = c("del", "A", "B", "C", "D", "y")
                formula = y ~ A + B + C + D
              } else if (!header && !rownames) {
                formula = V5 ~ .
              } else if (!header && rownames) {
                formula = V6 ~ V2 + V3 + V4 + V5
              }
              
              if(sep == "t"){
                sep = "\t"
              }
              
              if (header && rownames) {
                dat = data.table::fread(paste(path, "/", files[i], sep = ""),
                                        fill = TRUE, col.names = colnam,
                                        dec = dec, header = header,
                                        sep = sep, data.table = FALSE)
              } else {
                dat = data.table::fread(paste(path, "/", files[i], sep = ""),
                                        dec = dec, header = header,
                                        sep = sep, data.table = FALSE)
              }
              
              mod_dat[[i]] = mrfrequentist(dataMr = dat, approach = approach, obsPerBlock = obsPerBlock,
                                           formula = formula)
              if (header && rownames) {
                mod_file[[i]] = mrfrequentist(fileMr = paste(path, "/", files[i], sep = ""),
                                              approach = approach, header = header,
                                              colNames = colnam, dec = dec, sep = sep,
                                              obsPerBlock = obsPerBlock, formula = formula)
                
              } else {
                mod_file[[i]] = mrfrequentist(fileMr = paste(path, "/", files[i], sep = ""),
                                              approach = approach, header = header, dec = dec, sep = sep,
                                              obsPerBlock = obsPerBlock, formula = formula)
              }
              
              if (rownames == TRUE) {
                expect_equivalent(mod_dat[[i]]$dataHead[, (2:6)], mod1dataHead)
                expect_equivalent(mod_file[[i]]$dataHead[, (2:6)], mod1dataHead)
              } else if (rownames == FALSE) {
                expect_equivalent(mod_dat[[i]]$dataHead, mod1dataHead)
                expect_equivalent(mod_file[[i]]$dataHead, mod1dataHead)
              }
              
              mod_dat[[i]]$dataHead = NULL
              mod_file[[i]]$dataHead = NULL
              
              mod_dat[[i]]$formula = NULL
              mod_file[[i]]$formula = NULL
              
              mod_dat[[i]]$terms = NULL
              mod_file[[i]]$terms = NULL
              
              expect_equivalent(mod_dat[[i]], mod1)
              expect_equivalent(mod_file[[i]], mod1)
            }
            
          })

test_that("Reading in: same results for all data sets (separators, header, rownames,
          dataMr, fileMr, obsPerBlock = 501).", {
            
            obsPerBlock = 501
            
            mod2 = mrfrequentist(fileMr = paste(path, "/", files[1], sep = ""),
                                 approach = approach, header = FALSE, dec = ",", sep = " ",
                                 obsPerBlock = obsPerBlock, formula = V5 ~ .)
            
            mod2dataHead = mod2$dataHead
            mod2$dataHead = NULL
            
            mod2$formula = NULL
            mod2$terms = NULL
            
            mod_dat = vector("list", 40)
            mod_file = vector("list", 40)
            
            for(i in 1:(length(files))){
              
              posHeader = regexpr("header", files[i])[1]+6
              header = as.logical(unlist(strsplit(files[i], "^"))[posHeader])
              
              posDec = regexpr("dec", files[i])[1]+3
              dec = unlist(strsplit(files[i], "^"))[posDec]
              
              posSep = regexpr("sep", files[i])[1]+3
              sep = unlist(strsplit(files[i], "^"))[posSep]
              
              posRownames = regexpr("rownames", files[i])[1]+8
              rownames = as.logical(unlist(strsplit(files[i], "^"))[posRownames])
              
              if (header && !rownames) {
                formula = y ~ .
              } else if (header && rownames) {
                colnam = c("del", "A", "B", "C", "D", "y")
                formula = y ~ A + B + C + D
              } else if (!header && !rownames) {
                formula = V5 ~ .
              } else if (!header && rownames) {
                formula = V6 ~ V2 + V3 + V4 + V5
              }
              
              if(sep == "t"){
                sep = "\t"
              }
              
              if (header && rownames) {
                dat = data.table::fread(paste(path, "/", files[i], sep = ""),
                                        fill = TRUE, col.names = colnam,
                                        dec = dec, header = header,
                                        sep = sep, data.table = FALSE)
              } else {
                dat = data.table::fread(paste(path, "/", files[i], sep = ""),
                                        dec = dec, header = header,
                                        sep = sep, data.table = FALSE)
              }
              
              mod_dat[[i]] = mrfrequentist(dataMr = dat, approach = approach, obsPerBlock = obsPerBlock,
                                           formula = formula)
              if (header && rownames) {
                mod_file[[i]] = mrfrequentist(fileMr = paste(path, "/", files[i], sep = ""),
                                              approach = approach, header = header,
                                              colNames = colnam, dec = dec, sep = sep,
                                              obsPerBlock = obsPerBlock, formula = formula)
                
              } else {
                mod_file[[i]] = mrfrequentist(fileMr = paste(path, "/", files[i], sep = ""),
                                              approach = approach, header = header, dec = dec, sep = sep,
                                              obsPerBlock = obsPerBlock, formula = formula)
              }
              
              if (rownames == TRUE) {
                expect_equivalent(mod_dat[[i]]$dataHead[, (2:6)], mod2dataHead)
                expect_equivalent(mod_file[[i]]$dataHead[, (2:6)], mod2dataHead)
              } else if (rownames == FALSE) {
                expect_equivalent(mod_dat[[i]]$dataHead, mod2dataHead)
                expect_equivalent(mod_file[[i]]$dataHead, mod2dataHead)
              }
              
              mod_dat[[i]]$dataHead = NULL
              mod_file[[i]]$dataHead = NULL
              
              mod_dat[[i]]$formula = NULL
              mod_file[[i]]$formula = NULL
              
              mod_dat[[i]]$terms = NULL
              mod_file[[i]]$terms = NULL
              
              expect_equivalent(mod_dat[[i]], mod2)
              expect_equivalent(mod_file[[i]], mod2)
            }
            
          })

test_that("Random transformations, intercept.", {
  
  obsPerBlock = 300
  
  mod1 = mrfrequentist(dataMr = dataframe_header,
                       approach = approach, header = FALSE,
                       obsPerBlock = obsPerBlock,
                       formula = y ~ A + I(B^3) + log(abs(C)) + D + I(A*D))
  
  mod1dataHead = mod1$dataHead
  mod1$dataHead = NULL
  mod1$formula = NULL
  mod1$terms = NULL
  
  mod_dat = vector("list", 40)
  mod_file = vector("list", 40)
  
  for(i in 1:(length(files))){
    
    posHeader = regexpr("header", files[i])[1]+6
    header = as.logical(unlist(strsplit(files[i], "^"))[posHeader])
    
    posDec = regexpr("dec", files[i])[1]+3
    dec = unlist(strsplit(files[i], "^"))[posDec]
    
    posSep = regexpr("sep", files[i])[1]+3
    sep = unlist(strsplit(files[i], "^"))[posSep]
    
    posRownames = regexpr("rownames", files[i])[1]+8
    rownames = as.logical(unlist(strsplit(files[i], "^"))[posRownames])
    
    if (header && !rownames) {
      formula = y ~ A + I(B^3) + log(abs(C)) + D + I(A*D)
    } else if (header && rownames) {
      colnam = c("del", "A", "B", "C", "D", "y")
      formula = y ~ A + I(B^3) + log(abs(C)) + D + I(A*D)
    } else if (!header && !rownames) {
      formula = V5 ~ V1 + I(V2^3) + log(abs(V3)) + V4 + I(V1*V4)
    } else if (!header && rownames) {
      formula = V6 ~ V2 + I(V3^3) + log(abs(V4)) + V5 + I(V2*V5)
    }
    
    if(sep == "t"){
      sep = "\t"
    }
    
    if (header && rownames) {
      dat = data.table::fread(paste(path, "/", files[i], sep = ""),
                              fill = TRUE, col.names = colnam,
                              dec = dec, header = header,
                              sep = sep, data.table = FALSE)
    } else {
      dat = data.table::fread(paste(path, "/", files[i], sep = ""),
                              dec = dec, header = header,
                              sep = sep, data.table = FALSE)
    }
    
    mod_dat[[i]] = mrfrequentist(dataMr = dat, approach = approach, obsPerBlock = obsPerBlock,
                                 formula = formula)
    if (header && rownames) {
      mod_file[[i]] = mrfrequentist(fileMr = paste(path, "/", files[i], sep = ""),
                                    approach = approach, header = header,
                                    colNames = colnam, dec = dec, sep = sep,
                                    obsPerBlock = obsPerBlock, formula = formula)
      
    } else {
      mod_file[[i]] = mrfrequentist(fileMr = paste(path, "/", files[i], sep = ""),
                                    approach = approach, header = header, dec = dec, sep = sep,
                                    obsPerBlock = obsPerBlock, formula = formula)
    }
    
    if (rownames == TRUE) {
      expect_equivalent(mod_dat[[i]]$dataHead[, (2:6)], mod1dataHead)
      expect_equivalent(mod_file[[i]]$dataHead[, (2:6)], mod1dataHead)
    } else if (rownames == FALSE) {
      expect_equivalent(mod_dat[[i]]$dataHead, mod1dataHead)
      expect_equivalent(mod_file[[i]]$dataHead, mod1dataHead)
    }
    
    mod_dat[[i]]$dataHead = NULL
    mod_file[[i]]$dataHead = NULL
    
    mod_dat[[i]]$formula = NULL
    mod_file[[i]]$formula = NULL
    
    mod_dat[[i]]$terms = NULL
    mod_file[[i]]$terms = NULL
    
    expect_equivalent(mod_dat[[i]], mod1)
    expect_equivalent(mod_file[[i]], mod1)
  }
  
})

test_that("Random transformations, no intercept.", {
  
  obsPerBlock = 300
  
  mod2 = mrfrequentist(dataMr = dataframe_header,
                       approach = approach, obsPerBlock = obsPerBlock,
                       formula = y ~ A + I(B^3) + log(abs(C)) + D + I(A*D) + 0)
  
  mod2dataHead = mod2$dataHead
  mod2$dataHead = NULL
  mod2$formula = NULL
  mod2$terms = NULL
  
  mod_dat = vector("list", 40)
  mod_file = vector("list", 40)
  
  for(i in 1:(length(files))){
    
    posHeader = regexpr("header", files[i])[1]+6
    header = as.logical(unlist(strsplit(files[i], "^"))[posHeader])
    
    posDec = regexpr("dec", files[i])[1]+3
    dec = unlist(strsplit(files[i], "^"))[posDec]
    
    posSep = regexpr("sep", files[i])[1]+3
    sep = unlist(strsplit(files[i], "^"))[posSep]
    
    posRownames = regexpr("rownames", files[i])[1]+8
    rownames = as.logical(unlist(strsplit(files[i], "^"))[posRownames])
    
    if (header && !rownames) {
      formula = y ~ A + I(B^3) + log(abs(C)) + D + I(A*D) + 0
    } else if (header && rownames) {
      colnam = c("del", "A", "B", "C", "D", "y")
      formula = y ~ A + I(B^3) + log(abs(C)) + D + I(A*D) + 0
    } else if (!header && !rownames) {
      formula = V5 ~ V1 + I(V2^3) + log(abs(V3)) + V4 + I(V1*V4) + 0
    } else if (!header && rownames) {
      formula = V6 ~ V2 + I(V3^3) + log(abs(V4)) + V5 + I(V2*V5) + 0
    }
    
    if(sep == "t"){
      sep = "\t"
    }
    
    if (header && rownames) {
      dat = data.table::fread(paste(path, "/", files[i], sep = ""),
                              fill = TRUE, col.names = colnam,
                              dec = dec, header = header,
                              sep = sep, data.table = FALSE)
    } else {
      dat = data.table::fread(paste(path, "/", files[i], sep = ""),
                              dec = dec, header = header,
                              sep = sep, data.table = FALSE)
    }
    
    mod_dat[[i]] = mrfrequentist(dataMr = dat, approach = approach, obsPerBlock = obsPerBlock,
                                 formula = formula)
    if (header && rownames) {
      mod_file[[i]] = mrfrequentist(fileMr = paste(path, "/", files[i], sep = ""),
                                    approach = approach, header = header,
                                    colNames = colnam, dec = dec, sep = sep,
                                    obsPerBlock = obsPerBlock, formula = formula)
      
    } else {
      mod_file[[i]] = mrfrequentist(fileMr = paste(path, "/", files[i], sep = ""),
                                    approach = approach, header = header, dec = dec, sep = sep,
                                    obsPerBlock = obsPerBlock, formula = formula)
    }
    
    if (rownames == TRUE) {
      expect_equivalent(mod_dat[[i]]$dataHead[, (2:6)], mod2dataHead)
      expect_equivalent(mod_file[[i]]$dataHead[, (2:6)], mod2dataHead)
    } else if (rownames == FALSE) {
      expect_equivalent(mod_dat[[i]]$dataHead, mod2dataHead)
      expect_equivalent(mod_file[[i]]$dataHead, mod2dataHead)
    }
    
    mod_dat[[i]]$dataHead = NULL
    mod_file[[i]]$dataHead = NULL
    
    mod_dat[[i]]$formula = NULL
    mod_file[[i]]$formula = NULL
    
    mod_dat[[i]]$terms = NULL
    mod_file[[i]]$terms = NULL
    
    expect_equivalent(mod_dat[[i]], mod2)
    expect_equivalent(mod_file[[i]], mod2)
  }
  
})

test_that(desc = "Test column names.", {
  
  mod1 = mrfrequentist(dataMr = dataframe_header, approach = approach,
                       obsPerBlock = 300, formula = y ~ .)
  
  mod1$formula = NULL
  mod1$dataHead = NULL
  mod1$terms = NULL
  
  colnam = c("A1", "A2", "A3", "A4", "y1")
  
  mod2 = mrfrequentist(formula = y1 ~ ., fileMr = filepath_header, 
                       approach = approach,
                       obsPerBlock = 300, sep = "_", header = TRUE, colNames = colnam)
  
  mod2$formula = NULL
  mod2$dataHead = NULL
  mod2$terms = NULL
  
  mod3 = mrfrequentist(formula = y1 ~ ., fileMr = filepath_noheader, 
                       approach = approach,
                       obsPerBlock = 300, sep = "_", header = FALSE, colNames = colnam)
  
  mod3$formula = NULL
  mod3$dataHead = NULL
  mod3$terms = NULL
  
  expect_equivalent(mod2, mod1)
  expect_equivalent(mod3, mod1)
})
