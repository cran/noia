`varianceDecomposition` <-
function (obj) 
{
    if (!exists("effectsNames")) {
        effectsNames <- NULL; rm(effectsNames); data(effectsNames, package = "noia")
    }
    if (class(obj) == "noia.linear" || class(obj) == "noia.multilinear") {
        n <- names(obj$variances)
        if (obj$nloc == 1) {
            n.a <- c(0, 1, 0)
            n.d <- c(0, 0, 1)
            n.e <- c(0, 0, 0)
        }
        else {
            n.a <- apply(sapply(strsplit(n, ""), "c") == effectsNames[2], 
                2, "sum")
            n.d <- apply(sapply(strsplit(n, ""), "c") == effectsNames[3], 
                2, "sum")
            n.e <- apply(sapply(strsplit(n, ""), "c") == effectsNames[4], 
                2, "sum")
        }
        sum.total <- 0
        cat("\n")
        for (lev in 1:(obj$nloc)) {
            sum.level <- 0
            if (class(obj) == "noia.linear" || lev < 2) {
                for (nr.d in 0:lev) {
                  nr.a <- lev - nr.d
                  v <- sum(obj$variances[(n.a == nr.a) & (n.d == 
                    nr.d)])
                  if (is.na(v)) {
                    v <- 0
                  }
                  sum.level <- sum.level + v
                  if (v > 0) {
                    cat("\tVar(", paste(rep("A", nr.a), rep("D", 
                      nr.d), sep = "", coll = ""), "):\t", format(v, 
                      digits = 5), "\n", sep = "")
                  }
                }
            }
            else {
                v <- sum(obj$variances[n.e == lev])
                if (is.na(v)) {
                  v <- 0
                }
                sum.level <- sum.level + v
                if (v > 0) {
                  cat("\tVar(", paste(rep("E", lev), sep = "", 
                    coll = ""), "):\t", format(v, digits = 5), 
                    "\n")
                }
            }
            sum.total <- sum.total + sum.level
            if (sum.level > 0) {
                cat("\tTotal level ", lev, ":\t", format(sum.level, 
                  digits = 5), "\n\n")
            }
        }
        cat("\tTotal:\t", format(sum.total, digits = 5), "\n\n")
    }
    else {
        stop("Class", class(obj), "unknown.\n")
    }
}
