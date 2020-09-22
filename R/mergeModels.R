mergeModels = function (model1,
                        model2,
                        l = l,
                        approach = approach) {
  
  # calculate weights for mrbayes and approach "1"
  if (!approach == "3") {
    w1 = model1$numberObs / (model1$numberObs + model2$numberObs)
    w2 = model2$numberObs / (model1$numberObs + model2$numberObs)
  }
  
  # make new active model
  if (approach != "2") {
    if (all(model1$terms == model2$terms)) {
      if (approach == "1") {
        model3 = list(level = l + 1,
                      numberObs = model1$numberObs + model2$numberObs,
                      summaryStats = w1*model1$summaryStats + w2*model2$summaryStats,
                      terms = model1$terms)
      } else if (approach == "3") {
        XTX = model1$XTX + model2$XTX
        yTX = model1$yTX + model2$yTX
        model3 = list(level = l + 1,
                      numberObs = model1$numberObs + model2$numberObs,
                      XTX = XTX,
                      yTX = yTX,
                      yTy = model1$yTy + model2$yTy,
                      terms = model1$terms)
      }
    } else {
      stop("Types of variables in the models to be merged are not the same.", 
           .call = FALSE)
    }
  } else if (approach == "2") {
    n_eff = pmin(model1$diagnostics[, "n_eff"], model2$diagnostics[, "n_eff"])
    Rhat = pmax(model1$diagnostics[, "Rhat"], model2$diagnostics[, "Rhat"])
    model3 = list(level = l + 1,
                  numberObs = model1$numberObs + model2$numberObs,
                  summaryStats = w1 * model1$summaryStats + w2 * model2$summaryStats,
                  diagnostics = cbind(n_eff, Rhat))
  }
  
  return(model3)
  
}