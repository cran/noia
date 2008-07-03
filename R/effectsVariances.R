`effectsVariances` <-
function (obj) 
{
    if (class(obj) != "noia.linear") {
        stop("Unexpected object of class \"", class(obj), "\"\n")
    }
    v <- NULL
    if (is.null(obj$zmat) || is.null(obj$smat)) {
        v <- rep(NA, length(obj$E))
    }
    else {
        freq <- Z2freq(obj$zmat)
        v <- apply(obj$smat * obj$smat * freq, 2, "sum") * obj$E * 
            obj$E
    }
    names(v) <- names(obj$E)
    return(v)
}
