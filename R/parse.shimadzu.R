#################################################
# Written by Dr. Reema Singh #
#################################################
######## Define S4 object ############

setClass("parse.Shimadzu",representation(McPeakTable="data.frame",Annotation="matrix",GasChromatogram="data.frame",MassSpectrum = "list",FinalTable="data.frame"))

#### Implement Show generic function on S4 Class ######

setMethod("show","parse.Shimadzu",function(object){
	cat('Object of class "parse.Shimadzu"',"\n",
	"The object contains all parse information from Shimadzu pre-process .txt file", "\n",
	"User can access the structure of the object for more information", "\n")
	})

######## Constructor for object parse.Shimadzu #############


	parse.shimadzu <- function(fileName){
	conn=file(fileName,open="r")
	linn=readLines(conn)
	data <- vector()
	for (i in 1:length(linn)){
	data <- rbind(data, linn[i])
	}

	read.shimadzu <- Parse(data)
	return(read.shimadzu)
	close(conn)
	}


	
	
