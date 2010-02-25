freqmat2Sgenofreq <-
function (nloc, reference = "F2", freqmat = NULL, sinv = TRUE) 
{
    "strrev" <- function(ss) {
        sapply(lapply(strsplit(ss, character(0)), rev), paste, 
            collapse = "")
    }
    if (!exists("genotypesNames")) {
        genotypesNames <- NULL
        rm(genotypesNames)
        data(genotypesNames, package = "noia")
    }
    if (!exists("effectsNames")) {
        effectsNames <- NULL
        rm(effectsNames)
        data(effectsNames, package = "noia")
    }
    ans <- list()
    ans$smat <- 1
    if (sinv) {
        ans$sinv <- 1
    }
    ans$genofreq <- 1
    ans$genofreqloc <- NULL
    for (l in 1:nloc) {
        eff <- colnames(ans$smat)
        geno <- rownames(ans$smat)
        loc <- freqmat2Sgenofreqloc(reference = reference, l, 
            freqmat)
        ans$smat <- kronecker(loc$smat, ans$smat)
        if (sinv) {
            ans$sinv <- kronecker(loc$sinv, ans$sinv)
        }
        if (is.null(eff)) {
            colnames(ans$smat) <- effectsNames[1:3]
        }
        else {
            colnames(ans$smat) <- strrev(kronecker(effectsNames[1:3], 
                strrev(eff), "paste", sep = ""))
        }
        if (is.null(geno)) {
            rownames(ans$smat) <- genotypesNames
        }
        else {
            rownames(ans$smat) <- strrev(kronecker(genotypesNames, 
                strrev(geno), "paste", sep = ""))
        }
        ans$genofreq <- kronecker(loc$genofreq, ans$genofreq)
        ans$genofreqloc <- rbind(ans$genofreqloc, loc$genofreq)
    }
    return(ans)
}
