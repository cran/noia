`statusMaxDom` <-
function (effect, max.dom = NULL) 
{
    if (!exists("effectsNames")) {
        data(effectsNames, package = "noia")
    }
    if (!is.null(max.dom) && nchar(effect) > max.dom) {
        if (sum(strsplit(effect, "")[[1]] == effectsNames[3]) > 
            max.dom) 
            return(FALSE)
    }
    return(TRUE)
}
