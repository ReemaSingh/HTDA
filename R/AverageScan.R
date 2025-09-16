#################################################
# Written by Dr. Reema Singh #
#################################################
AverageScan <- function(x){
        y <- data.frame(x$b)
        colnames(y) <- c("mass","absolute","relative")
        aveMZ <- mean(as.numeric(as.character(y$mass)))
        aveAbs <- mean(as.numeric(as.character(y$absolute)))
        aveRel <- mean(as.numeric(as.character(y$relative)))
        averageSpectrum <- data.frame(MZ = aveMZ, Absolute = aveAbs, Relative = aveRel)
        return(averageSpectrum)
        }
