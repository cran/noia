`geneticEffects` <-
function (obj, ref.genotype = "P1") 
{
    if (class(obj) == "noia.linear") {
        new.smat <- genZ2S(obj$genZ, type = ref.genotype)
        new.smat <- solve(new.smat)
        new.smat <- new.smat[colnames(obj$S), ]
        T <- new.smat %*% obj$S
        effects <- T %*% obj$E
        std.err <- sqrt((T * T) %*% (obj$std.dev * obj$std.dev))
        ans <- cbind(effects, std.err)
        colnames(ans) <- c("Effects", "Std.dev")
        return(ans)
    }
    else if (class(obj) == "noia.multilinear") {
        stop("Change of reference for the multilinear model: not implemented.")
    }
    else {
        stop("Object of class ", class(obj), " unknown.")
    }
}
