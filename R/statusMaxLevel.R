`statusMaxLevel` <-
function (effect, max.level = NULL) 
{
    if (!exists("effectsNames")) {
        data(effectsNames, package = "noia")
    }
    if (!is.null(max.level) && nchar(effect) > max.level) {
        if (sum(strsplit(effect, "")[[1]] != effectsNames[1]) > 
            max.level) 
            return(FALSE)
    }
    return(TRUE)
}
