#' @title Fitting frequentist linear models using Merge and Reduce
#' @description \code{mrfrequentist} is used to conduct frequentist linear
#'   regression on very large data sets using Merge and Reduce as
#'   described in Geppert et al. (2020).
#' @references Geppert, L.N., Ickstadt, K., Munteanu, A., & Sohler, C. (2020).
#'   Streaming statistical models via Merge & Reduce. International Journal
#'   of Data Science and Analytics, 1-17, \cr
#'   doi: https://doi.org/10.1007/s41060-020-00226-0
#' @inheritParams mrbayes

#' @param approach \code{(character)} \cr Approach specifying the merge
#'   technique. One of either "1" or "3". Approach "1" is based on a weighted
#'   mean procedure whereas approach "3" is an exact method based on blockwise 
#'   calculations of X'X, y'X and y'y. See Geppert et al. (2020) for details on 
#'   the approaches and section Details below for comments on approach "3".
#' @param formula \code{(formula)} \cr
#'   See \code{\link[stats]{formula}}. Note that \code{mrfrequentist} currently
#'   supports numeric predictors only.
#' @param obsPerBlock \code{(numeric)} \cr Value specifying the number of
#'   observations in each block. This number has to be larger than the number of 
#'   regression coefficients. Moreover, for approach 1 the recommended ratio of 
#'   observations per regression coefficient is larger than 25 (Geppert et al., 2020). 
#'   Note that the last block may contain less observations than specified 
#'   depending on the sample size. If the number of observations in this last 
#'   block is too small it is not included in the model and a warning is 
#'   issued. 
#' @return Returns an object of class \code{"mrfrequentist"} which is a list
#'   containing the following components \strong{for both approaches "1" and "3":}
#'   \item{approach}{The approach used for merging the models. Either "1" or "3".}
#'   \item{formula}{The model's \code{formula}.}
#'   \item{level}{Number of level of the final model in Merge and Reduce. This is equal
#'     to \eqn{\lceil \log_2{(\code{numberObs}/\code{obsPerBlock})} \rceil + 1}{
#'     log2(ceiling(numberObs/obsPerBlock))+1} 
#'     and corresponds to the number of buckets in Figure 1 of Geppert et al. (2020).} 
#'   \item{numberObs}{The total number of observations.}
#'   \item{summaryStats}{Summary statistics reporting the estimated regression coefficients 
#'   and their unbiased standard errors. Estimates are based
#'   on the merge technique as specified in the argument \code{approach}.
#'   For approach "1" the estimates of the standard errors are corrected
#'   dividing by \eqn{ \sqrt \lceil \code{numberObs /  obsPerBlock} \rceil
#'   }{sqrt(ceiling(numberObs/obsPerBlock))}. For further details see Geppert et al. (2020).
#'   For approach "3" the unbiased estimates of the standard errors are given.}
#'   \item{dataHead}{First six rows of the data in the first block. This serves
#'   as a sanity check, especially when using the argument \code{fileMr}.}
#'   \item{terms}{Terms object.}
#'   \strong{Additionally for approach "3" only:}
#'   \item{XTX}{The final model's \code{crossprod(X, X)}.}
#'   \item{yTX}{The final model's \code{crossprod(y, X)}.}
#'   \item{yTy}{The final model's \code{crossprod(y, y)}.}
#' @section Details:
#'   In approach "3" the estimated regression coefficients and their unbiased standard errors
#'   are calculated via qr decompositions on X'X (as in \code{\link[speedglm]{speedlm}} 
#'   with argument \code{method = "qr"}). Moreover, the merge step uses the same 
#'   idea of blockwise addition for X'X, y'y and y'X as \code{speedglm}'s updating 
#'   procedure \code{\link[speedglm]{updateWithMoreData}}. Conceptually though, 
#'   Merge and Reduce is not an updating algorithm as it merges models based on 
#'   a comparable amount of data along a tree structure to obtain a final model.
#'   
#' @examples
#' ## run mrfrequentist() with dataMr
#' data(exampleData)
#' fit1 = mrfrequentist(dataMr = exampleData, approach = "1", obsPerBlock = 300,
#' formula = V11 ~ .)
#'
#' ## run mrfrequentist() with fileMr
#' filepath = system.file("extdata", "exampleFile.txt", package = "mrregression")
#' fit2 = mrfrequentist(fileMr = filepath, approach = "3", header = TRUE,
#' obsPerBlock = 100, formula = y ~ .)
#' @importFrom stats model.matrix
#' @importFrom utils head
#' @export

mrfrequentist = function(
  formula,
  fileMr = NULL,
  dataMr = NULL,
  obsPerBlock,
  approach = c("1", "3"),
  sep = "auto",
  dec = ".",
  header = TRUE,
  naStrings = "NA",
  colNames = NULL,
  naAction = na.fail
  ){
  
  # argument checks; no checks for sep, dec, naStrings as these are passed on to fread
  argumentChecks(dataMr = dataMr, fileMr = fileMr, header = header, 
                 colNames = colNames, obsPerBlock = obsPerBlock)
  approach = match.arg(approach, c("1", "3"))
  if (!inherits(formula, "formula")) {
    stop("Argument formula must inherit from formula.", call. = FALSE)
  }

  models = list()
  
  p = NULL
  
  if (!is.null(fileMr)) {
    con = file(fileMr, open="r")
    on.exit(close(con))
  }
  
  if (header && is.null(colNames) && !is.null(fileMr)) {
    commonheader = readLines(con = con, n = 1)
    commonheader2 = names(data.table::fread(input = paste0(commonheader, "\n"),
                                            data.table = FALSE, sep = sep, 
                                            dec = dec))
    rm(commonheader)
  } else if (header && !is.null(colNames) && !is.null(fileMr)) {
    commonheader2 = colNames
    readLines(con = con, n = 1) # remove header
  } else if (!header && !is.null(colNames) && !is.null(fileMr)) {
    commonheader2 = colNames
  } else {
    commonheader2 = NULL
  }
  
  while (
    if ( !is.null(fileMr) ) {
      (length({txt = readLines(con = con, n = obsPerBlock)} ) > 0)
    } else if (!is.null(dataMr)) {
      (nrow(dataMr) > 0)
    }) {
    
    l = 1
    
    rB = readBlock(fileMr = fileMr, dataMr = dataMr,
                   obsPerBlock = obsPerBlock,
                   commonheader2 = commonheader2,
                   dec = dec, sep = sep,
                   naStrings = naStrings, txt = txt)
    
    currentBlock = rB$currentBlock
    dataMr = rB$dataMr
    rm(rB)
    
    naAction = na.fail
    currentBlock = naAction(currentBlock)

    if (is.null(p) || (!is.null(p) && !warningMessageObsPerBlock(p = p,
                                                  n = nrow(currentBlock),
                                                  approach = approach))) {
      
      tempM = saveRelChar(approach = approach, currentBlock = currentBlock, 
                          formula = formula, l = l, p = p)
      
      if (is.null(p)) {
        p = tempM$p
        errorMessageObsPerBlock(p = p, obsPerBlock = obsPerBlock)
        dataHead = head(currentBlock)
      }
      tempM$p = NULL
      
      rm(currentBlock)
      
      tmp = mergeFullLevels(models2 = models, l2 = l, tempM2 = tempM, 
                            approach = approach)
      models = tmp[["models"]]
      l = tmp[["l"]]
      tempM = tmp[["tempM"]]
      rm(tmp)
      
      # if no model with level l in models save tempM
      if (all(sapply(models, function(x) x$l) != l)) {
        
        models[[length(models) + 1]] = tempM
        
        rm(tempM)
        
      } # end of if
    } # end of     if (is.null(p) || (!is.null(p) && !warningMessageObsPerBlock(p = p, n = nrow(currentBlock), approach = approach)))
  } # end of while readLines
  
  while (length(models) > 1) {
    
    currentMod = which(sapply(models, function(x) x$l) %in%
                         sort(sapply(models, function(x) x$l))[c(1,2)])
    
    l = max(c(models[[currentMod[1]]]$l, models[[currentMod[2]]]$l))
    
    tempM = mergeModels(model1 = models[[currentMod[1]]],
                        model2 = models[[currentMod[2]]],
                        l = l,
                        approach = approach)
    
    models[[currentMod[2]]] = NULL
    models[[currentMod[1]]] = NULL
    
    l = l + 1
    
    tmp = mergeFullLevels(models2 = models, l2 = l, tempM2 = tempM, 
                          approach = approach)
    models = tmp[["models"]]
    l = tmp[["l"]]
    tempM = tmp[["tempM"]]
    rm(tmp)
    
    models[[length(models) + 1]] = tempM
    rm(tempM)
    
  } # end of while
  
  models[[1]] = makeFinalModel(mod = models[[1]], approach = approach,
                                  p = p, obsPerBlock = obsPerBlock)
  
  if (approach == "1") {
    res = list(approach = models[[1]]$approach,
               formula = formula,
               level = models[[1]]$level,
               numberObs = models[[1]]$numberObs,
               summaryStats = models[[1]]$summaryStats,
               dataHead = dataHead,
               terms = models[[1]]$terms)
  } else if (approach == "3") {
    res = list(approach = models[[1]]$approach,
               formula = formula,
               level = models[[1]]$level,
               numberObs = models[[1]]$numberObs,
               summaryStats = models[[1]]$summaryStats,
               dataHead = dataHead,
               terms = models[[1]]$terms,
               XTX = models[[1]]$XTX,
               yTX = models[[1]]$yTX,
               yTy = models[[1]]$yTy)
  }
  
  class(res) = "mrfrequentist"
  
  warningMessageObsPerVariable(p = p, obsPerBlock = obsPerBlock,
                               approach = approach)
  
  return(res)
  
}
