mysummary <- function(modelobject) {
    # returns coefficients and pvalue of rsm() or lm() model object
    coef <- summary(modelobject)$coefficients
    pvalue <- pvalue(modelobject)
    return(list(coef=coef, pvalue=pvalue))
}
