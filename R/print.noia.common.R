`print.noia.common` <-
function (x, ...) 
{
    cat("\nPhenotype:\n")
    cat(paste("\tn=", length(x$phen), " min: ", format(min(x$phen), 
        digits = 5), " max: ", format(max(x$phen), digits = 5), 
        " mean: ", format(mean(x$phen), digits = 5), "\n"))
    cat("Genotype:\n")
    cat(paste("\tn=", nrow(x$genL), ",", ncol(x$genL), "loci\n"))
    for (i in colnames(x$genL)) {
        cat("\t\t", i, ":  \tn=", (length(as.character(x$genL[, 
            i])) - sum(as.character(x$genL[, i]) == "NA")), "\t", 
            paste(levels(factor(as.character(x$genL[, i]), exclude = c("NA", 
                "<NA>"))), collapse = "  "), "\n")
    }
    cat("\n")
    coef <- cbind(x$E, x$variances, x$std.dev, x$pvalues)
    colnames(coef) <- c("Effects", "Variances", "Std.dev", "Pr(>|t|)")
    printCoefmat(coef, P.values = TRUE, signif.stars = TRUE, 
        has.Pvalue = TRUE)
    variance <- var(x$phen, na.rm = TRUE)
    cat("\nVariances\n\tTotal (phen)\t", format(variance, digits = 5), 
        "\n\tResidual\t", format(x$resvar, digits = 5), "\n\tExplained\t", 
        format(variance - x$resvar, digits = 5), "\t(", format(100 * 
            (variance - x$resvar)/variance, digits = 3), "%)\n", 
        sep = "")
}
