`genZ2S` <-
function (type = "F2", genZ = NULL, nloc = NULL) 
{
    if (is.null(nloc)) {
        if (is.null(genZ)) {
            stop("Function Z2S: number of loci unknown; either zmat or nloc must be provided")
        }
        nloc <- ncol(genZ)/3
    }
    ans <- 1
    for (i in 1:nloc) {
        ans <- kronecker(Sloc(type, i, genZ), ans)
    }
    colnames(ans) <- effectsNamesGeneral(nloc)
    rownames(ans) <- genNames(nloc)
    return(ans)
}
