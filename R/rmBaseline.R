############## Baseline Correction Modiefied #####################
###### Function Inherited from PROcess package, but we modified it according to our need.

rmBaseline <- function (fldr, bseoffrda = NULL, breaks = 200, qntl = 0, method = "loess",
    bw = 0.1, SpecNames = list.files(fldr, pattern = "\\.*csv\\.*"))
{
    fs <- list.files(fldr, pattern = "\\.*csv\\.*", full.names = T)
    n <- length(fs)
    par(mfrow=c(3,3))
    for (j in 1:n) {
        f1 <- read.files(fs[j])
        fcut <- f1[f1[, 1] > 0, ]
        bseoff <- PROcess::bslnoff(fcut, breaks = breaks, qntl = qntl,
            method = method, plot=TRUE,bw = bw)

        if (j > 1)
            bseoffM <- cbind(bseoffM, bseoff[, 2])
        else bseoffM <- bseoff[, 2]
    }
    dimnames(bseoffM) <- list(signif(bseoff[, 1], 6), SpecNames)
    if (!is.null(bseoffrda))
        save(list = bseoffM, file = bseoffrda)
    bseoffM
}

