`GPmap` <-
function (obj) 
{
    if (class(obj) == "noia.linear") {
        if (is.null(obj$smat)) {
            stop("No GP map estimate with 'fast' algorith")
        }
        g <- cbind(obj$smat %*% obj$E, sqrt((obj$smat * obj$smat) %*% 
            (obj$std.dev * obj$std.dev)))
        colnames(g) <- c("G.val", "std.err")
        return(g)
    }
    else if (class(obj) == "noia.multilinear") {
        rec <- reconstructLinearEffects(obj)
        g <- cbind(obj$smat %*% rec[, 1], sqrt((obj$smat * obj$smat) %*% 
            (rec[, 2] * rec[, 2])))
        return(g)
    }
    else {
        stop("Class", class(obj), "unknown.\n")
    }
}
