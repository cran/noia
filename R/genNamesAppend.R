genNamesAppend <-
function (name) 
{
    if (!exists("genotypesNames")) {
        genotypesNames <- NULL
        rm(genotypesNames)
        data(genotypesNames, package = "noia")
    }
    ans <- NULL
    for (n in name) {
        for (g in genotypesNames[1:3]) {
            ans <- c(ans, paste(g, n, sep = ""))
        }
    }
    return(ans)
}
