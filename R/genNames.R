`genNames` <-
function (nloc = 2) 
{
    if (!exists("genotypesNames")) {
        genotypesNames <- NULL; rm(genotypesNames);data(genotypesNames, package = "noia")
    }
    names <- genotypesNames[1:3]
    if (nloc > 1) {
        for (i in 2:nloc) {
            names <- genNamesAppend(names)
        }
    }
    return(names)
}
