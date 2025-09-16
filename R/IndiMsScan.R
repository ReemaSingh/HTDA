#################################################
# Written by Dr. Reema Singh #
#################################################
IndiMsScan <- function(x,i){
        y <- data.frame(x[[i]]$b)
        colnames(y) <- c("mass","absolute","relative")
        return(y)
        }

