context("Test mrbayes(): Class and reading in.")

skip_on_cran()

if (requireNamespace("rstan", quietly = TRUE)) {
  
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
  
  obsPerBlock = 500
  
  seed = 394
  chains = 4

  test_that("Test class, known output and data.table/data.frame as input.", {
    
    fitExampleDataDF = mrbayes(dataMr = exampleData,
                               obsPerBlock = obsPerBlock, y = "V11",
                               seed = seed, chains = chains,
                               refresh = 0, verbose = FALSE)
    
    exDatDF = data.table::as.data.table(exampleData)
    
    fitExampleDataDT = mrbayes(dataMr = exDatDF,
                               obsPerBlock = obsPerBlock, y = "V11",
                               seed = seed, chains = chains,
                               refresh = 0, verbose = FALSE)
    
    fitExampleDataDFReference = readRDS("resTestMrbayes.rds")

    expect_equal(fitExampleDataDF, fitExampleDataDFReference, tolerance = 0.1)
    expect_equal(fitExampleDataDF$level, fitExampleDataDFReference$level)
    expect_equal(fitExampleDataDF$numberObs, fitExampleDataDFReference$numberObs)
    expect_equal(fitExampleDataDF$summaryStats, fitExampleDataDFReference$summaryStats, tolerance = 0.001)
    expect_equal(fitExampleDataDF$diagnostics, fitExampleDataDFReference$diagnostics, tolerance = 0.1)
    expect_equal(fitExampleDataDF$approach, fitExampleDataDFReference$approach)
    expect_equal(fitExampleDataDF$dataHead, fitExampleDataDFReference$dataHead)
    
    expect_is(object = fitExampleDataDF, class = "mrbayes")
    
    expect_equivalent(object = fitExampleDataDF, expected = fitExampleDataDT)
    
  })
  
  
 test_that("Reading in: same results for all data sets (separators, header, rownames,
          dataMr, fileMr). Moreover, rownames and specifying dataStan.", {
   
   mod = mrbayes(dataMr = dataframe_header,
                 dec = ",", obsPerBlock = obsPerBlock, y = "y",
                 seed = seed, chains = chains,
                 refresh = 0, verbose = FALSE)
   
   modDataHead = mod$dataHead
   mod$dataHead = NULL
   
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
       y = "y"
     } else if (header && rownames) {
       colnam = c("del", "A", "B", "C", "D", "y")
       y = "y"
     } else if (!header && !rownames) {
       y = "V5"
     } else if (!header && rownames) {
       y = "V6"
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
      
      if (header && rownames) {
        dataStan = quote(list(n = nrow(currentBlock),
                              d = (ncol(currentBlock) - 2),
                              X = currentBlock[, c("A", "B", "C", "D")],
                              y = currentBlock[, "y"]))
      } else if (!header && rownames) {
        dataStan = quote(list(n = nrow(currentBlock),
                              d = (ncol(currentBlock) - 2),
                              X = currentBlock[, c("V2", "V3", "V4", "V5")],
                              y = currentBlock[, "V6"]))
      }
      
      if (header && rownames) {
        mod_file[[i]] = mrbayes(fileMr = paste(path, "/", files[i], sep = ""),
                                sep = sep, header = header,
                                dataStan = dataStan,
                                dec = dec, obsPerBlock = obsPerBlock, y = y,
                                colNames = colnam, seed = seed,
                                chains = chains,
                                refresh = 0, verbose = FALSE)
        mod_dat[[i]] = mrbayes(dataMr = dat, dataStan = dataStan,
                               obsPerBlock = obsPerBlock, y = y,
                               seed = seed,
                               chains = chains,
                               refresh = 0, verbose = FALSE)
      } else if (!header && rownames) {
        mod_file[[i]] = mrbayes(fileMr = paste(path, "/", files[i], sep = ""),
                                sep = sep, header = header,
                                dataStan = dataStan,
                                dec = dec, obsPerBlock = obsPerBlock, y = y,
                                seed = seed,
                                chains = chains,
                                refresh = 0, verbose = FALSE)
        mod_dat[[i]] = mrbayes(dataMr = dat,
                                 obsPerBlock = obsPerBlock, y = y,
                                 seed = seed, dataStan = dataStan,
                                 chains = chains,
                                 refresh = 0, verbose = FALSE)
      } else {
        mod_file[[i]] = mrbayes(fileMr = paste(path, "/", files[i], sep = ""),
                                sep = sep, header = header,
                                dec = dec, obsPerBlock = obsPerBlock, y = y,
                                seed = seed,
                                chains = chains,
                                refresh = 0, verbose = FALSE)
        mod_dat[[i]] = mrbayes(dataMr = dat,
                               obsPerBlock = obsPerBlock, y = y,
                               seed = seed,
                               chains = chains,
                               refresh = 0, verbose = FALSE)
      }
      
      if (rownames == TRUE) {
        expect_equivalent(mod_dat[[i]]$dataHead[, (2:6)], modDataHead)
        expect_equivalent(mod_file[[i]]$dataHead[, (2:6)], modDataHead)
      } else if (rownames == FALSE) {
        expect_equivalent(mod_dat[[i]]$dataHead, modDataHead)
        expect_equivalent(mod_file[[i]]$dataHead, modDataHead)
      }
      
      mod_dat[[i]]$dataHead = NULL
      mod_file[[i]]$dataHead = NULL
      
      expect_equivalent(mod_dat[[i]], mod)
      expect_equivalent(mod_file[[i]], mod)
   }
   
   filename = list.files(path = path, pattern = "dat\\^headerFALSE\\^sep_\\^dec\\.\\^rownamesTRUE\\^skip0\\^nastringNA")
   filepath = paste0(path, "/", filename)
   
   mod2 = mrbayes(fileMr = filepath,
                  header = FALSE,
                  dataStan = quote(list(n = nrow(currentBlock),
                                        d = (ncol(currentBlock) - 2),
                                        X = currentBlock[, c("V2", "V3", "V4", "V5")],
                                        y = currentBlock[, "V6"])),
                  sep = "_", obsPerBlock = obsPerBlock, y = "V6",
                  seed = seed,
                  chains = chains,
                  refresh = 0, verbose = FALSE)
   
   mod2$dataHead = NULL
   
   expect_equivalent(mod2, mod)
   
  })
  
}
