#' @title Bayesian linear regression using Merge and Reduce
#'
#' @description \code{mrbayes} is used to conduct Bayesian linear regression on
#'   very large data sets using Merge and Reduce as described in Geppert et al. (2020).
#'   Package \code{rstan} needs to be installed. When calling the function this
#'   is checked using \code{requireNamespace} as suggested by
#'   Hadley Wickham in "R packages" (section Dependencies, 
#'   http://r-pkgs.had.co.nz/description.html, accessed 2020-07-31).
#' @references Geppert, L.N., Ickstadt, K., Munteanu, A., & Sohler, C. (2020).
#'   Streaming statistical models via Merge & Reduce. International Journal
#'   of Data Science and Analytics, 1-17, \cr
#'   doi: https://doi.org/10.1007/s41060-020-00226-0
#' @param fileMr (\code{character}) \cr The name of a file, including the
#'   filepath, to be read in blockwise. Either \code{fileMr} or \code{dataMr}
#'   needs to be specified. When using this argument, the arguments \code{sep}, 
#'   \code{dec}, \code{header}, \code{naStrings}, \code{colNames} (as in \code{\link[data.table]{fread}}) 
#'   are of relevance. Further options from \code{fread} are currently not supported.
#'   Also note that defaults might differ. In case the data to be read in has row names,
#'   note that these will be read in as regular column. This may need
#'   special treatment.
#' @param dataMr (\code{data.frame}) \cr The data to be used for the regression
#'   analysis. Either \code{fileMr} or \code{dataMr} needs to be specified.
#'   Note that the arguments \code{sep}, \code{dec}, \code{header}, \code{naStrings}, 
#'   and \code{colNames} are ignored when \code{dataMr} is specified.
#' @param dataStan (\code{list}) \cr Optional argument. This argument is
#'   equivalent to the argument \code{data} in \code{\link[rstan]{stan}}. If not
#'   specified the default \code{dataStan}, which makes use of all predictors is used.
#'   See section Details for the default \code{dataStan} and further notes on the syntax
#'   to be used when specifiying this argument.
#' @param sep See documentation of \code{\link[data.table]{fread}}. Default is
#'   "auto". Ignored when \code{dataMr} is specified.
#' @param dec See documentation of \code{\link[data.table]{fread}}. Default is 
#'   ".". Ignored when \code{dataMr} is specified.
#' @param header \code{(logical)} \cr See documentation of
#'   \code{\link[data.table]{fread}}. Defaults to \code{TRUE}. Ignored when
#'   \code{dataMr} is specified. If \code{header} is set to \code{FALSE} and no 
#'   \code{colNames} are given, then column names default to "V" followed by the 
#'   column number.
#' @param naStrings \code{(character)} \cr Optional argument.
#'   See argument na.strings of \code{\link[data.table]{fread}}.
#'   Default is "NA". Ignored when \code{dataMr} is specified and optional
#'   when \code{fileMr} is used.
#' @param colNames \code{(character vector)} \cr Same as argument
#'   col.names of \code{\link[data.table]{fread}}. Ignored when \code{dataMr} is
#'   specified and optional when \code{fileMr} is used.
#' @param obsPerBlock \code{(numeric)} \cr Value specifying the number of
#'   observations in each block. This number has to be larger than the number of 
#'   regression coefficients. Moreover, the recommended ratio of 
#'   observations per regression coefficient is larger than 25 (Geppert et al., 2020). 
#'   Note that the last block may contain less observations than specified 
#'   depending on the sample size. If the number of observations in this last 
#'   block is too small it is not included in the model and a warning is 
#'   issued. 
#' @param intercept \code{(logical)} \cr Argument specifying whether the model
#'   should have an intercept term or not. Defaults to \code{TRUE}.
#' @param y \code{(character)} \cr Column name of the dependent variable.
#' @param naAction \code{(function)} \cr Action to be taken when missing values
#' are present in the data. Currently only \code{\link[stats]{na.fail}} is 
#' supported.
#' @param ... Further optional arguments to be passed on to
#'   \code{\link[rstan]{stan}}, especially \code{pars} and arguments that
#'   control the behaviour of the sampling in \code{rstan} such as \code{chains},
#'   \code{iter}, \code{warmup}, and \code{thin}. Please refer to 
#'   \code{\link[rstan]{rstan}}.
#' @return Returns an object of class \code{"mrbayes"} which is a list
#'   containing the following components: 
#'   \item{level}{Number of level of the final model in Merge and Reduce. This is equal
#'     to \eqn{\lceil \log_2{(\code{numberObs}/\code{obsPerBlock})} \rceil + 1}{
#'     log2(ceiling(numberObs/obsPerBlock))+1} 
#'     and corresponds to the number of buckets in Figure 1 of Geppert et al. (2020).} 
#'   \item{numberObs}{The total number of observations.}
#'   \item{summaryStats}{Summary statistics including the mean, median,
#'   quartiles, 2.5\% and 97.5\% quantiles of the posterior distributions for each
#'   regression coefficient and the error term's standard deviation sigma.} 
#'   \item{diagnostics}{Effective sample size (n_eff) and potential
#'   scale reduction factor on split chains (Rhat) calculated from the output of
#'   \link[rstan]{summary,stanfit-method}. Note that, using Merge and Reduce,
#'   for each regression coefficient only one value is reported: For n_eff the
#'   minimum observed value on level 1 is reported and for Rhat the maximum
#'   observed value on level 1 is reported.} 
#'   \item{modelCode}{The model. Syntax
#'   as in argument \code{model_code} of \link[rstan]{stan}.}
#'   \item{dataHead}{First six rows of the data in the first block. This serves
#'   as a sanity check, especially when using the argument \code{fileMr}.}
#' @section Details:
#'   Code of default \code{dataStan} makes use of all predictors: \cr
#'   \code{dataStan = list(n = nrow(currentBlock),} \cr \code{d = (ncol(currentBlock) -
#'   1),} \cr \code{X = currentBlock[, -colNumY],} \cr \code{y = currentBlock[, colNumY])} \cr
#'   where \code{currentBlock}
#'   is the current block of data to be evaluated, \code{n} the number of observations, 
#'   \code{d} the number of variables (without intercept), \code{X} contains the predictors,
#'   and \code{y} the dependent variable. \code{colNumY} is the column number of the 
#'   dependent variable that the function finds internally. \cr
#'   \cr
#'   When specifying the argument \code{dataStan}, note two things: \cr 
#'   1. Please use the syntax of the default \code{dataStan}, i.e. the object 
#'   containing the data of the block to be evaluated is called 
#'   \code{currentBlock}, the number of observations must be set to
#'   \code{n = nrow(currentBlock)}, \code{d} needs to be set to the number of
#'   variables without intercept, the dependent variable must be named \code{y},
#'   and the independent variables must be named \code{X}. \cr 
#'   2. The expressions
#'   within the list must be unevaluated: Therefore, use the function
#'   \code{\link[base]{quote}}. \cr
#'   
#' @examples
#' # Package rstan needs to be installed for running this example.
#' \donttest{ 
#' if (requireNamespace("rstan", quietly = TRUE)) {
#'   n = 2000
#'   p = 4
#'   set.seed(34)
#'   x1 = rnorm(n, 10, 2)
#'   x2 = rnorm(n, 5, 3)
#'   x3 = rnorm(n, -2, 1)
#'   x4 = rnorm(n, 0, 5)
#'   y = 2.4 - 0.6 * x1 + 5.5 * x2 - 7.2 * x3 + 5.7 * x4 + rnorm(n)
#'   data = data.frame(x1, x2, x3, x4, y)
#' 
#'   normalmodell = '
#'   data {
#'   int<lower=0> n;
#'   int<lower=0> d;
#'   matrix[n,d] X; // predictor matrix
#'   vector[n] y; // outcome vector
#'   }
#'   parameters {
#'   real alpha; // intercept
#'   vector[d] beta; // coefficients for predictors
#'   real<lower=0> sigma; // error scale
#'   }
#'   model {
#'   y ~ normal(alpha + X * beta, sigma); // likelihood
#'   }
#'   ' 
#' 
#'   datas = list(n = nrow(data), d = ncol(data)-1,
#'                y = data[, dim(data)[2]], X = data[, 1:(dim(data)[2]-1)])
#'   fit0 = rstan::stan(model_code = normalmodell, data = datas, chains = 4, iter = 1000)
#'   fit1 = mrbayes(dataMr = data, obsPerBlock = 500, y = 'y')
#' }
#' }
#' @importFrom utils head
#' @import Rcpp
#' @export

mrbayes = function (
  y,
  intercept = TRUE,
  fileMr = NULL,
  dataMr = NULL,
  obsPerBlock,
  dataStan = NULL,
  sep = "auto",
  dec = ".",
  header = TRUE,
  naStrings = "NA",
  colNames = NULL,
  naAction = na.fail,
  ...) {
  
  if (!requireNamespace("rstan", quietly = TRUE)) {
    message("Please install rstan and its dependencies in order to use this function.")
  } else {
    
    # argument checks; no checks for sep, dec, naStrings as these are passed on to fread
    argumentChecks(dataMr = dataMr, fileMr = fileMr, header = header, 
                   colNames = colNames, obsPerBlock = obsPerBlock)
    stopifnot(is.logical(intercept), is.character(y))
    if (!is.null(dataStan)) {
      if (!is.call(dataStan)) {
        stop("dataStan must be specified as a list within the function quote. Evaluation of 
        the expression will take place at a later point in time.", call. = FALSE)
      }
    }
    
    approach = "2"
    
    models = list()
    
    p = NULL
    
    fit_stan = NULL
    
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
    
    if (intercept) {
      modelCode = modelIntercept
    } else {
      modelCode = modelNoIntercept
    }
    
    while (
      if (!is.null(fileMr)) {
        (length({txt = readLines(con = con, n = obsPerBlock)}) > 0)
      } else if (!is.null(dataMr)) {
        (nrow(dataMr) > 0)
      }  ) {
      
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
      
      # dataStan: create data or use the one given by the user
      if (is.null(dataStan)) {
        colNumY = which(colnames(currentBlock) == y)
        dataStanInt = list(n = nrow(currentBlock),
                           d = (ncol(currentBlock) - 1),
                           X = currentBlock[, -colNumY],
                           y = currentBlock[, colNumY])
      } else if (!is.null(dataStan)) {
        dataStanInt = eval(dataStan)
      }
      
      if (dataStanInt$n != nrow(currentBlock)) {
        stop("dataStan not specified correctly. The number of observations n
             must be set to nrow(currentBlock).", call. = FALSE)
      }
      
      if (is.null(p)) {
        p = ifelse(test = intercept, yes = dataStanInt$d + 1, no = dataStanInt$d)
        errorMessageObsPerBlock(p = p,  obsPerBlock = obsPerBlock)
        dataHead = head(currentBlock)
      }
        
      if (!warningMessageObsPerBlock(p = p, n = dataStanInt$n, approach = approach)) {
        
        # model compilation - do this only once
        if (is.null(fit_stan)) {
          suppressMessages({fit_stan = rstan::stan(model_code = modelCode,
                                                   data = dataStanInt, chains = 0)})
        }

        fit1 = rstan::stan(fit = fit_stan, data = dataStanInt, ...)

        tempM = saveRelChar(approach = approach, currentBlock = currentBlock, 
                            fit = fit1, l = l, p = p)
        
        rm(fit1)
        dataStanInt = NULL
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
        
      } # end of if if (!warningMessageObsPerBlock(p = p, n = dataStanInt$n, approach = approach))
    } # end of while readLines
    
    # check if more than one model left and if yes merge
    while (length(models) > 1) {
      
      currentMod = which(sapply(models, function(x) x$l) %in%
                           sort(sapply(models, function(x) x$l))[c(1,2)])
      
      l = max(c(models[[currentMod[1]]]$l, models[[currentMod[2]]]$l))
      
      tempM = mergeModels(model1 = models[[currentMod[1]]],
                          model2 = models[[currentMod[2]]],
                          l = l,
                          approach = approach)
      
      # delete merged models
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
                                    obsPerBlock = obsPerBlock)
    
    models[[1]]$dataHead = dataHead
    
    class(models[[1]]) = "mrbayes"
    
    warningMessageObsPerVariable(p = p, obsPerBlock = obsPerBlock, 
                                 approach = approach)
    
    return(models[[1]])
    
  }
}
