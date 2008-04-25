`matrix2list` <-
function (mat) 
{
    ans <- list()
    for (c in 1:ncol(mat)) {
        label <- colnames(mat)[c]
        ans[[label]] <- c(mat[, c])
    }
    return(ans)
}
