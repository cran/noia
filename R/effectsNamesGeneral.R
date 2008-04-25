`effectsNamesGeneral` <-
function (nloc = 2) 
{
    if (!exists("effectsNames")) {
        data(effectsNames, package = "noia")
    }
    ebase <- effectsNames[1:3]
    enames <- ebase
    if (nloc > 1) {
        for (i in 1:(nloc - 1)) {
            enames <- kronecker(ebase, enames, FUN = "paste", 
                sep = "")
        }
    }
    return(strrev(enames))
}
