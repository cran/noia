`startingValues` <-
function (reg, max.level = 2, max.dom = 2, e.unique = FALSE) 
{
    if (class(reg) == "noia.linear") {
        return(startingValuesLinear(reg, max.level, max.dom, 
            e.unique))
    }
    else if (class(reg) == "noia.multilinear") {
        return(startingValuesMultilinear(reg, max.level, max.dom, 
            e.unique))
    }
    else {
        stop("Object of class \"noia.linear\" or \"noia.multilinear\" expected\n")
    }
}
