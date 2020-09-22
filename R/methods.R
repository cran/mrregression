#' @importFrom stats .checkMFClasses coef delete.response model.frame na.fail printCoefmat pt terms

#' @importFrom stats nobs
#' @export
print.mrfrequentist = function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("Approach: \n", x$approach, "\n", sep = "")

  cat("\nFormula: \n", deparse(x$formula), "\n", sep = "")

  cat("\nCoefficients: \n")
  print.default(x$summaryStats[, "Estimate"], digits = digits , print.gap = 2L,
                quote = FALSE)
}

#' @export
print.mrbayes = function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nCoefficients: \n")
  print.default(x$summaryStats)
}

#' @export
#' @rdname mrregression
#' @name mrregression
coef.mrfrequentist = function(object, ...)
  object$summaryStats[, "Estimate"]

coef.summary.mrfrequentist = function(object, ...)
  coef.mrfrequentist(object)

#' @export
#' @rdname mrregression
nobs.mrfrequentist = function(object, ...)
  object$numberObs

#' @export
#' @rdname mrregression
predict.mrfrequentist = function(object, data, ...) {
  if (missing(data)) stop("data needs to be specified.")
  
  t1 = terms(object$formula, data = object$dataHead)
  t2 = delete.response(t1)
  
  mf = model.frame(t2, data = data, na.action = na.fail)
  
  .checkMFClasses(attr(object$terms, "dataClasses"), mf)
  
  designm = model.matrix(t2, mf)
  
  pred = drop(designm %*% matrix(coef(object), ncol = 1))
  
  return(pred)
}

#' @export
#' @rdname mrregression
summary.mrfrequentist = function(object, ...) {
  class(object) = "summary.mrfrequentist"
  return(object)
}

#' @export
#' @rdname mrregression
print.summary.mrfrequentist = function(x, ...) {
  cat("Approach: \n", x$approach, "\n", sep = "")
  cat("\nFormula: \n", deparse(x$formula), "\n", sep = "")
  cat("\nNumber of Observations: \n",x$numberObs, "\n", sep = "")
  cat("\nLevel: \n", x$level, "\n", sep = "")
  cat("\nCoefficients: \n")
  if (x$approach == '3') printCoefmat(x$summaryStats, ...) else
    printCoefmat(x$summaryStats, has.Pvalue = FALSE, cs.ind = 1:2, tst.ind = NULL)
  invisible(x)
}

#' @export
#' @rdname mrregression
nobs.mrbayes = function(object, ...)
  object$numberObs

#' @export
#' @rdname mrregression
summary.mrbayes = function(object, ...) {
  class(object) = "summary.mrbayes"
  return(object)
}

#' @export
#' @rdname mrregression
print.summary.mrbayes = function(x, ...) {
  cat("\nNumber of Observations: \n", x$numberObs, "\n", sep = "")
  cat("\nLevel: \n", x$level, "\n", sep = "")
  cat("\nCoefficients: \n")
  print.default(x$summaryStats)
  cat("\nDiagnostics: \n")
  print.default(x$diagnostics)
  invisible(x)
}
