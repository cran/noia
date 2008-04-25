`linearRegression` <-
function (phen, gen = NULL, genZ = NULL, reference = "noia", 
    max.level = NULL, max.dom = NULL) 
{
    phen <- as.vector(phen)
    if (!is.null(gen) && is.vector(gen)) {
        gen <- as.matrix(gen)
    }
    if (!is.null(gen)) {
        gen <- gen[!is.na(phen), ]
    }
    if (!is.null(genZ)) {
        genZ <- genZ[!is.na(phen), ]
    }
    phen <- phen[!is.na(phen)]
    if (is.null(genZ)) {
        if (is.null(gen)) {
            stop("No genotype provided.")
        }
        else {
            genZ <- gen2genZ(gen)
        }
    }
    nloc <- ncol(genZ)/3
    nn <- effectsNamesGeneral(nloc)
    nn <- nn[nn %in% effectsSelect(nloc, max.level, max.dom)]
    smat <- genZ2S(reference, genZ, nloc)
    smat <- smat[, nn]
    zmat <- genZ2Z(genZ)
    regression <- lm(phen ~ (zmat %*% smat) + 0)
    ans <- list()
    class(ans) <- "noia.linear"
    ans$nloc <- nloc
    ans$phen <- phen
    if (!is.null(gen)) {
        ans$gen <- gen
    }
    ans$genZ <- genZ
    ans$S <- smat
    ans$Z <- zmat
    ans$E <- coef(regression)
    names(ans$E) <- nn
    ans$variances <- effectsVariances(ans)
    ans$std.dev <- summary(regression)$coef[, 2]
    ans$pvalues <- effectsPvalues(regression)
    ans$resvar <- var(residuals(regression))
    ans$regression <- regression
    return(ans)
}
