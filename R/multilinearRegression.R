`multilinearRegression` <-
function (phen, gen = NULL, reference = "noia", genZ = NULL, 
    max.level = NULL, max.dom = NULL, e.unique = FALSE, start.algo = "linear", 
    start.values = NULL, ...) 
{
    if (is.null(max.level) || max.level > 2) {
        max.level <- 2
    }
    if (is.null(max.dom) || max.dom > max.level) {
        max.dom <- max.level
    }
    linear <- linearRegression(phen = phen, gen = gen, reference = reference, 
        genZ = genZ, max.level = max.level, max.dom = max.dom)
    smat <- linear$S
    zmat <- linear$Z
    nloc <- linear$nloc
    phen <- linear$phen
    nn <- effectsNamesMultilinear(nloc, max.level, max.dom)
    form <- formulaMultilinear(nloc, max.level, max.dom, e.unique)
    X <- matrix2list(zmat %*% smat)
    if (is.null(start.values)) {
        if (start.algo == "linear") {
            start.values <- startingValues(linear, max.level, 
                max.dom, e.unique)
        }
        else if (start.algo == "multilinear") {
            mlin <- multilinearRegression(phen = phen, gen = gen, 
                reference = reference, genZ = genZ, max.level = 2, 
                max.dom = 0, e.unique = e.unique, start.algo = "linear", 
                start.values = NULL, ...)
            start.values <- startingValues(mlin, max.level, max.dom, 
                e.unique)
        }
        else if (start.algo == "subset") {
            subset.size <- 1000
            subsample <- sample(1:length(phen), subset.size)
            mlin <- multilinearRegression(phen = phen[subsample], 
                gen = gen[subsample, ], reference = reference, 
                genZ = genZ[subsample, ], max.level = max.level, 
                max.dom = max.dom, e.unique = e.unique, start.algo = "linear", 
                start.values = NULL, ...)
            start.values <- startingValues(mlin, max.level, max.dom, 
                e.unique)
        }
    }
    regression <- nls(formula = as.formula(form), start = start.values, 
        ...)
    ans <- list()
    ans$nloc <- nloc
    ans$phen <- phen
    ans$gen <- gen
    ans$S <- smat
    ans$Z <- zmat
    ans$E <- coef(regression)
    ans$std.dev <- summary(regression)$coef[, 2]
    ans$pvalues <- summary(regression)$coef[, 4]
    if (e.unique) {
        ee <- ans$E["ee"]
        ss <- ans$std.dev["ee"]
        pp <- ans$pvalues["ee"]
        ans$E <- ans$E[names(ans$E) != "ee"]
        ans$std.dev <- ans$std.dev[names(ans$std.dev) != "ee"]
        ans$pvalues <- ans$pvalues[names(ans$pvalues) != "ee"]
        for (i in 1:(nloc - 1)) {
            for (j in (i + 1):nloc) {
                ans$E[paste("e", i, j, sep = "")] <- ee
                ans$std.dev[paste("e", i, j, sep = "")] <- ss
                ans$pvalues[paste("e", i, j, sep = "")] <- pp
            }
        }
    }
    names(ans$E) <- nn
    names(ans$std.dev) <- nn
    ans$resvar <- var(residuals(regression))
    names(ans$pvalues) <- nn
    ans$variances <- rep(NA, length(nn))
    names(ans$variances) <- nn
    ans$regression <- regression
    class(ans) <- "noia.multilinear"
    return(ans)
}
