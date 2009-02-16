`effectsNamesMultilinear` <-
function (nloc = 2, max.level = 2, max.dom = 2) 
{
    if (!exists("effectsNames")) {
        effectsNames <- NULL
        rm(effectsNames)
        data(effectsNames, package = "noia")
    }
    a <- effectsNames[2]
    d <- effectsNames[3]
    e <- effectsNames[4]
    ans <- NULL
    ans <- c(ans, effNames(nloc = nloc))
    if ((nloc > 0) && (max.level > 0)) {
        for (l1 in 1:nloc) {
            ans <- c(ans, effNames(c(a), c(l1), nloc))
            if (max.dom > 0) {
                ans <- c(ans, effNames(c(d), c(l1), nloc))
            }
        }
    }
    if ((nloc > 1) && (max.level > 1)) {
        for (l1 in 1:(nloc - 1)) {
            for (l2 in (l1 + 1):nloc) {
                ans <- c(ans, effNames(c(e, e), c(l1, l2), nloc))
            }
        }
    }
    return(ans)
}
