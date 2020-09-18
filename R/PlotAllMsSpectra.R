plotAllMsSpectra <- function(x){
        no <- x$a[[2]]
        no <- gsub("# of Peaks", "Number of Peaks = ",no)
        y <- data.frame(x$b)
        colnames(y) <- c("mass","absolute","relative")
        plot(as.character(y$mass),as.character(y$relative),type="h",xlab ="Mass/Charge", ylab = "Relative Intensity",col=ifelse(y$relative=="100.00", "red","black"), main = no)
        }


