pvalue <- function (modelobject) {
    # returns pvalue of rsm() or lm() model object
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}
