% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R, R/mrregression.R
\docType{package}
\name{mrregression}
\alias{mrregression}
\alias{coef.mrfrequentist}
\alias{nobs.mrfrequentist}
\alias{predict.mrfrequentist}
\alias{summary.mrfrequentist}
\alias{print.summary.mrfrequentist}
\alias{nobs.mrbayes}
\alias{summary.mrbayes}
\alias{print.summary.mrbayes}
\title{mrregression: Frequentist and Bayesian linear regression using 
Merge and Reduce.}
\usage{
\method{coef}{mrfrequentist}(object, ...)

\method{nobs}{mrfrequentist}(object, ...)

\method{predict}{mrfrequentist}(object, data, ...)

\method{summary}{mrfrequentist}(object, ...)

\method{print}{summary.mrfrequentist}(x, ...)

\method{nobs}{mrbayes}(object, ...)

\method{summary}{mrbayes}(object, ...)

\method{print}{summary.mrbayes}(x, ...)
}
\arguments{
\item{object}{Object of class \code{"mrfrequentist"} or \code{"mrbayes"},
respectively.}

\item{...}{Currently only useful for method \code{print.summary.mrfrequentist} 
and approach "3". See arguments to function \code{\link[stats]{printCoefmat}}, 
especially \code{digits} and \code{signif.stars}.}

\item{data}{A \code{data.frame} used to predict values of the
dependent variable. Data has to contain all variables in the model, additional
columns are ignored. Note that this is not an optional argument.}

\item{x}{Object of class \code{"summary.mrfrequentist"} or \code{"summary.mrbayes"},
respectively.}
}
\description{
Frequentist and Bayesian linear regression for large data sets. Useful when 
the data does not fit into memory (for both frequentist and Bayesian 
regression), to make running time manageable (mainly for Bayesian 
regression), and to reduce the total running time because of reduced or 
less severe memory-spillover into the virtual memory.
The package contains the two main functions
\code{\link[mrregression]{mrfrequentist}} and \code{\link[mrregression]{mrbayes}} 
as well as several S3 methods listed below. Note, that currently only
numerical predictors are supported. Factor variables can be included in the
model in dummy-coded form, e.g. using \code{\link[stats]{model.matrix}}.
However, this may lead to highly variable or even unreliable estimates /
posterior distributions if levels are not represented well in every single block.
It is solely the user's responsibility to check that this is not the case!
}
\references{
Geppert, L.N., Ickstadt, K., Munteanu, A., & Sohler, C. (2020).
  Streaming statistical models via Merge & Reduce. International Journal
  of Data Science and Analytics, 1-17, \cr
  doi: https://doi.org/10.1007/s41060-020-00226-0
}
