#' mrregression: Frequentist and Bayesian linear regression using 
#' Merge and Reduce.
#'
#' Frequentist and Bayesian linear regression for large data sets. Useful when 
#' the data does not fit into memory (for both frequentist and Bayesian 
#' regression), to make running time manageable (mainly for Bayesian 
#' regression), and to reduce the total running time because of reduced or 
#' less severe memory-spillover into the virtual memory.
#' The package contains the two main functions
#' \code{\link[mrregression]{mrfrequentist}} and \code{\link[mrregression]{mrbayes}} 
#' as well as several S3 methods listed below. Note, that currently only
#' numerical predictors are supported. Factor variables can be included in the
#' model in dummy-coded form, e.g. using \code{\link[stats]{model.matrix}}.
#' However, this may lead to highly variable or even unreliable estimates /
#' posterior distributions if levels are not represented well in every single block.
#' It is solely the user's responsibility to check that this is not the case!
#'
#' @param object Object of class \code{"mrfrequentist"} or \code{"mrbayes"},
#'  respectively.
#' @param x Object of class \code{"summary.mrfrequentist"} or \code{"summary.mrbayes"},
#' respectively.
#' @param data A \code{data.frame} used to predict values of the
#' dependent variable. Data has to contain all variables in the model, additional
#' columns are ignored. Note that this is not an optional argument. 
#' @param ... Currently only useful for method \code{print.summary.mrfrequentist} 
#' and approach "3". See arguments to function \code{\link[stats]{printCoefmat}}, 
#' especially \code{digits} and \code{signif.stars}.
#' 
#' @references Geppert, L.N., Ickstadt, K., Munteanu, A., & Sohler, C. (2020).
#'   Streaming statistical models via Merge & Reduce. International Journal
#'   of Data Science and Analytics, 1-17, \cr
#'   doi: https://doi.org/10.1007/s41060-020-00226-0
#'
#' @docType package
#' @name mrregression
NULL