`effectsSelect` <-
function (nloc, max.level = NULL, max.dom = NULL) 
{
    if (!exists("effectsNames")) {
        data(effectsNames, package = "noia")
    }
    if ((is.null(max.level)) || (max.level > nloc)) {
        max.level <- nloc
    }
    if ((is.null(max.dom)) || (max.dom > max.level)) {
        max.dom <- max.level
    }
    n <- effectsNamesGeneral(nloc)
    if ((max.level == nloc) && (max.dom == nloc)) {
        return(n)
    }
    n.all <- apply(sapply(strsplit(n, ""), "c") != effectsNames[1], 
        2, "sum")
    n.d <- apply(sapply(strsplit(n, ""), "c") == effectsNames[3], 
        2, "sum")
    return(n[((n.d == 0) & (n.all <= max.level)) | ((n.d > 0) & 
        (n.all <= max.dom))])
}
