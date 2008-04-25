`drawGenotype` <-
function (nloc = 1, type = "F2") 
{
    ans <- ""
    for (l in 1:nloc) {
        r <- runif(1)
        g <- ""
        if (type == "F2") {
            if (r < 1/4) {
                g <- "1"
            }
            else if (r < 3/4) {
                g <- "2"
            }
            else {
                g <- "3"
            }
        }
        else if (type == "Finf") {
            if (r < 1/2) {
                g <- "1"
            }
            else {
                g <- "3"
            }
        }
        else if (type == "F1") {
            g <- "2"
        }
        else {
            stop("Population type ", type, " unknown.")
        }
        ans <- paste(ans, g, sep = "")
    }
    return(ans)
}
