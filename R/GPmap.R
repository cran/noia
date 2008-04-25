`GPmap` <-
function (obj) 
{
    if (class(obj) == "noia.linear") {
        g <- cbind(obj$S %*% obj$E, sqrt((obj$S * obj$S) %*% 
            (obj$std.dev * obj$std.dev)))
        colnames(g) <- c("G.val", "std.err")
        return(g)
    }
    else if (class(obj) == "noia.multilinear") {
        rec <- reconstructLinearEffects(obj)
        g <- cbind(obj$S %*% rec[, 1], sqrt((obj$S * obj$S) %*% 
            (rec[, 2] * rec[, 2])))
        return(g)
    }
    else {
        stop("Class", class(obj), "unknown.\n")
    }
}
