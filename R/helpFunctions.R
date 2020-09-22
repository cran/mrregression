readBlock = function (fileMr, dataMr, obsPerBlock,
                      commonheader2, dec,
                      naStrings, txt, sep) {
  if (!is.null(fileMr)) {
    if (!is.null(commonheader2)) {
      currentBlock = data.table::fread(input = paste(txt, collapse = "\n"),
                                       sep = sep,
                                       col.names = commonheader2, header = FALSE,
                                       dec = dec, na.strings = naStrings,
                                       data.table = FALSE)
    } else if (is.null(commonheader2)) {
      currentBlock = data.table::fread(input = paste(txt, collapse = "\n"),
                                       sep = sep,
                                       header = FALSE,
                                       dec = dec, na.strings = naStrings,
                                       data.table = FALSE)
    }
  } else if (!is.null(dataMr)) {
    if (nrow(dataMr) >= obsPerBlock) {
      currentBlock = dataMr[c(1:obsPerBlock), ]
      dataMr = dataMr[-c(1:obsPerBlock), ]
    } else if (nrow(dataMr) < obsPerBlock) {
      currentBlock = dataMr
      dataMr = dataMr[-c(1:nrow(dataMr)), ]
    }
  }
  
  return(list(currentBlock = currentBlock, dataMr = dataMr))
}

saveRelChar = function (approach, currentBlock, l, fit, formula = formula, 
                        p = p) {

  if (approach == "1") {
    
    if (is.null(p)) {
      fit = stats::lm(formula, data = currentBlock, model = FALSE, x = TRUE, 
                      singular.ok = FALSE)
      p = ncol(fit$x)
    } else {
      fit = stats::lm(formula, data = currentBlock, model = FALSE,
                      singular.ok = FALSE)
    }
    
    if (!all(attr(terms(fit), "dataClasses") == "numeric")) {
     stop("Predictors and target variable must be numeric.")
    }
    
    tempM = list(level = l,
                 numberObs = nobs(fit),
                 summaryStats = coef(summary(fit))[, c("Estimate", "Std. Error")],
                 terms = terms(fit),
                 p = p)
    
  } else if (approach == "3") {
    
    modframe = model.frame(formula = formula, data = currentBlock)
    termsModframe = terms(modframe)
     if (!all(attr(termsModframe, "dataClasses") == "numeric")) {
       stop("Predictors and target variable must be numeric.")
     }
    
    target = paste(formula)[2]
    y = as.matrix(subset(x = currentBlock, select = target))
    X = stats::model.matrix(modframe, data = currentBlock)
    
    if (is.null(p)) {
      p = ncol(X)
    }
    
    if (p > qr(X)$rank) {
      stop("Singular fit encountered.", call. = FALSE)
    }
    
    tempM = list(level = l,
                 numberObs = nrow(currentBlock),
                 betaHat = rep(NA, ncol(X)),
                 XTX = crossprod(X, X),
                 yTy = crossprod(y, y),
                 yTX = crossprod(y, X),
                 terms = termsModframe,
                 p = p)
  } else if (approach == "2") {
    summary_fit1 = rstan::summary(fit)$summary
    tempM = list(level = l,
                 numberObs = nrow(currentBlock),
                 summaryStats = summary_fit1[-nrow(summary_fit1),
                                             c("mean", "2.5%", "25%",
                                               "50%", "75%", "97.5%")],
                 diagnostics = summary_fit1[-nrow(summary_fit1),
                                            c("n_eff", "Rhat")])
  }
  
  return(tempM)
}

makeFinalModel = function(mod, approach, p = p, obsPerBlock) {
  
  if (approach == "1") {
    mod$summaryStats[, "Std. Error"] =
      mod$summaryStats[, "Std. Error"] / sqrt(ceiling(mod$numberObs / obsPerBlock))
  } else if (approach == "3") {
    betaHat = qr.solve(mod$XTX, t(mod$yTX))
    numerator = (mod$yTy + 
                   crossprod(t(crossprod(betaHat, mod$XTX)), betaHat) - 
                   (2*crossprod(t(mod$yTX), betaHat)))[1, 1, drop = TRUE]
    sSqUnbiased = numerator / (mod$numberObs - p)
    seUnbiased = sqrt(diag(qr.solve(mod$XTX) * sSqUnbiased))
    mod$summaryStats = cbind(betaHat, seUnbiased)
    colnames(mod$summaryStats) = c("Estimate", "Std. Error")
  } else if (approach == "2") {
    mod$summaryStats[, c("2.5%", "25%", "75%", "97.5%")] =
      ((mod$summaryStats[, c("2.5%", "25%", "75%", "97.5%")] -
          mod$summaryStats[, "mean"]) /
         sqrt(ceiling(mod$numberObs / obsPerBlock))) +
      mod$summaryStats[, "mean"]
  }
  if (approach == "3") {
    mod$summaryStats =
      cbind(mod$summaryStats, "t value" = mod$summaryStats[, "Estimate"] / mod$summaryStats[, "Std. Error"])
    mod$summaryStats =
      cbind(mod$summaryStats, `Pr(>|t|)` =
        2 * pt(abs(mod$summaryStats[, "t value"]), mod$numberObs - p, lower.tail = FALSE))
  }

  mod$approach = approach  
  
  return(mod)
  
}


mergeFullLevels = function(models2, l2, tempM2, approach) {
  
  # while level is full mergeModels
  while (any(sapply(models2, function(x) x$l == l2))) {
    
    fullLevel = which(sapply(models2, function(x) x$l == l2))
    
    tempM2 = mergeModels(model1 = tempM2,
                         model2 = models2[[fullLevel]],
                         l = l2,
                         approach = approach)
    
    # remove model from level l
    models2[[fullLevel]] = NULL
    l2 = l2 + 1
  } # end of while
  
  return(list(tempM = tempM2, models = models2, l = l2))
  
}
