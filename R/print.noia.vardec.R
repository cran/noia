print.noia.vardec <-
function (x, ...) 
{
    cat("\nVariance decomposition:\n")
    if (!is.null(x$V_G)) {
        totvar <- x$V_G
    }
    else {
        totvar <- sum(unlist(x))
    }
    cat(paste("\tTotal genetic variance:", format(totvar, digits = 5), 
        "\n"))
    print(names(x))
    for (level in setdiff(names(x), "V_G")) {
        cat(paste("\tOrder", level, "\tTotal:\t", format(sum(x[[level]]), 
            digits = 5, nsmall = 5), "\t(", format(100 * sum(x[[level]])/totvar, 
            digits = 3, nsmall = 1), "%)", "\n"))
        for (i in 1:(length(x[[level]]))) {
            cat(paste("\t\t\t", names(x[[level]])[i], "\t", format(x[[level]][i], 
                digits = 5, nsmall = 5), "\t(", format(100 * 
                x[[level]][i]/totvar, digits = 3, nsmall = 1), 
                "%)", "\n"))
        }
    }
}
