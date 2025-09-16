#################################################
# Written by Dr. Reema Singh #
#################################################
PlotChrom <- function(x){
        plot(as.character(x$RT),as.character(x$MZ),type="h",xlab="Retention Time", ylab = "Mass/Charge Ration")
        identify(as.character(x$RT),as.character(x$MZ),x$MSPeaks)
        }


