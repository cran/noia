`effectsVariances` <-
function (obj) 
{
    if (class(obj) != "noia.linear") {
        stop("Inexpected object of class \"", class(obj), "\"\n")
    }
    zmat <- obj$Z
    smat <- obj$S
    effects <- obj$E
    freq <- Z2freq(zmat)
    v <- apply(smat * smat * freq, 2, "sum") * effects * effects
    names(v) <- names(effects)
    return(v)
}
