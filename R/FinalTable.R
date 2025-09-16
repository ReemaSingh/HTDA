#################################################
# Written by Dr. Reema Singh #
#################################################
FinalTable <- function(x){
	peaks <- x$a[[2]][2]
        BaseMZ <- x$a[[3]][4]
        BaseMZ <- gsub("m/z","",BaseMZ)
        tic1 <- sum(as.numeric(x$b[,2]))
        total <- c(peaks,BaseMZ,tic1)
        return(total)
        }
