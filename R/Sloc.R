`Sloc` <-
function (type = "F2", i = NULL, genZ = NULL) 
{
    ans <- NULL
    if (type == "F2") {
        ans <- matrix(c(1, -1, -0.5, 1, 0, 0.5, 1, 1, -0.5), 
            byrow = TRUE, ncol = 3)
    }
    else if (type == "P1") {
        ans <- matrix(c(1, 0, 0, 1, 1, 1, 1, 2, 0), byrow = TRUE, 
            ncol = 3)
    }
    else if (type == "P2") {
        ans <- matrix(c(1, -2, 0, 1, -1, 1, 1, 0, 0), byrow = TRUE, 
            ncol = 3)
    }
    else if (type == "F1") {
        ans <- matrix(c(1, -1, -1, 1, 0, 0, 1, 1, -1), byrow = TRUE, 
            ncol = 3)
    }
    else if (type == "Finf") {
        ans <- matrix(c(1, -1, 0, 1, 0, 1, 1, 1, 0), byrow = TRUE, 
            ncol = 3)
    }
    else if (type == "UWR") {
        ans <- matrix(c(1, -1, -1/3, 1, 0, 2/3, 1, 1, -1/3), 
            byrow = TRUE, ncol = 3)
    }
    else if (type == "noia") {
        if (is.null(genZ) || is.null(i)) {
            stop("zmat is necessary for statistical model computation")
        }
        f <- Z2freq(genZ[, (3 * i - 2):(3 * i)])
        ans <- matrix(c(1, -f[2] - 2 * f[3], (-2 * f[2] * f[3])/(f[1] + 
            f[3] - (f[1] - f[3])^2), 1, 1 - f[2] - 2 * f[3], 
            (4 * f[1] * f[3])/(f[1] + f[3] - (f[1] - f[3])^2), 
            1, 2 - f[2] - 2 * f[3], (-2 * f[1] * f[2])/(f[1] + 
                f[3] - (f[1] - f[3])^2)), byrow = TRUE, ncol = 3)
    }
    else if (type == "G2A") {
        if (is.null(genZ) || is.null(i)) {
            stop("zmat is necessary for statistical model computation")
        }
        f <- Z2freq(genZ[, (3 * i - 2):(3 * i)])
        p <- f[1] + f[2]/2
        ans <- matrix(c(1, -2 * (1 - p), -2 * (1 - p) * (1 - 
            p), 1, -1 + 2 * p, 2 * p * (1 - p), 1, 2 * p, -2 * 
            p * p), byrow = TRUE, ncol = 3)
    }
    else {
        stop("unknown type ", type, ".")
    }
    return(ans)
}
