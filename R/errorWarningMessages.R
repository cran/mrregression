argumentChecks = function (dataMr, fileMr, header, colNames, obsPerBlock) {
  if (is.null(fileMr) && is.null(dataMr)) {
    stop("Please specify either fileMr or dataMr.",
         call. = FALSE)
  }
  
  if (!is.null(fileMr) && !is.null(dataMr)) {
    stop("Both fileMr and dataMr given. Please specify only one of them.",
         call. = FALSE)
  }
  
  if (!is.null(dataMr) && !is.data.frame(dataMr) ) {
    stop("dataMr must inherit from class data.frame.", call. = FALSE)
  }
  
  if (!is.logical(header)) {
    stop("Argument header must be logical.", call. = FALSE)
  }
  
  if (!is.null(colNames)) {
    if (!is.character(colNames)) {
      stop("Argument colNames must be a character vector of strings.",
           call. = FALSE)
    }
  }
  
  if (!is.numeric(obsPerBlock)) {
    stop("Argument obsPerBlock must be of class numeric or integer.", call. = FALSE)
  }
  
  if (obsPerBlock %% 1 != 0) {
    stop("Argument obsPerBlock must be a whole number.", call. = FALSE) 
  }
  
}

errorMessageObsPerBlock = function (p, obsPerBlock) {
  if (p >= obsPerBlock) {
    stop("Number of regression coefficients is larger than or equal to the number of observations in each block.",
         call. = FALSE)
  }
}

warningMessageObsPerBlock = function (p, n, approach) {
  if (p > n) {
  	warntext = ifelse(approach == '2',
  	  sprintf("Fewer observations than coefficients in last block. Last block with %s observations is not included in model.",
                    n),
      sprintf("Singular fit encountered in last block. Last block with %s observations is not included in model.",
                    n))
    warning(warntext, call. = FALSE)
    return(TRUE)
  }
  if (p == n) {
    warning(sprintf("Number of regression coefficients is equal to number of observations in last block. Last block with %s observations is not included in model.",
                    n), call. = FALSE)
    return(TRUE)
  }

  return(FALSE)
}

warningMessageObsPerVariable = function (p, obsPerBlock, approach) {
  if ((obsPerBlock / p <= 25) && approach != "3") {
    warning("Number of observations per regression coefficient <= 25 in all blocks. Models could be unreliable.",
            call. = FALSE)
  }
}