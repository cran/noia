`strrev` <-
function (ss) 
{
    sapply(lapply(strsplit(ss, character(0)), rev), paste, collapse = "")
}
