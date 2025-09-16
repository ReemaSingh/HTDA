#################################################
# Written by Dr. Reema Singh #
#################################################
####### Shimadzu .txt file parsing function 

Parse <- function(x){
	pos1 <- which(x=="[Header]")
	pos2 <- which(x=="[File Information]")
	pos3 <- which(x =="[Sample Information]")
	pos4 <- which(x=="[MC Peak Table]")
	pos5 <- which(x=="[Spectrum Process Table]")
	pos6 <- which(x=="[MS Similarity Search Results for Spectrum Process Table]")
	pos7 <- which(x== "[MS Chromatogram]")
	pos8 <- which(x == "[MS Spectrum]")

####### Header Information

	header = vector()
	for(i in 1:length(x)){
	if(i < pos2){
	header <- rbind(header, x[i])
	 }
}

####### Sample Information

	SamInfo <- vector()
	for(i in 1:length(x)){
	if((i >= pos3) &&  (i < pos4)){
	SamInfo <-rbind(SamInfo, x[i])
	}
}

######## MC Peak table

	PeakTable <- vector()
	for(i in 1:length(x)){
	if((i >= pos4) &&  (i < pos5)){
	PeakTable <-rbind(PeakTable, x[i])
	PeakTable <- gsub("\t","   ",PeakTable)
	}
}

	TotalPeak <- PeakTable[2,]
	peaks<- PeakTable[-c(1:3),]
	peaks <- strsplit(peaks,"  ",fixed=FALSE)
	peaks <- t(sapply(peaks,'[', 1:max(sapply(peaks,length))))
	colnames(peaks) <- peaks[1,]
	peaks <- peaks[-1,]
	peaks <- peaks[,-c(12:13)]
	peaks <- data.frame(peaks)

###### MS Spectrum

	Spectrum <- vector()
	for(i in 1:length(x)){
	if((i >= pos5) &&  (i < pos6)){
	Spectrum <-rbind(Spectrum, x[i])
	}
}

####### MS Annotation

	Annotation <- vector()
	for(i in 1:length(x)){
	if((i >= pos6) &&  (i < pos7)){
	Annotation <-rbind(Annotation, x[i])
	Annotation <- gsub("\t","   ",Annotation)

	}
}

###### Gas Chromatogram

	Chromatogram <- vector()
	for(i in 1:length(x)){
	if((i >= pos7) && (i < pos8[1])){
	Chromatogram <-rbind(Chromatogram, x[i])
	}
}
	y <- Chromatogram[-c(1:6),]
	y <- gsub("\t","  ",y)
	y <- strsplit(y,"  ",fixed=FALSE)
	y <- t(sapply(y,'[',1:max(sapply(y,length))))
	colnames(y) <- y[1,]
	y <- y[-1,]
	y <- data.frame(y)

###### Mass Spectrum 
######## Extract all the spectrum into one

	MsSpectrum <- vector()
	for(i in 1:length(x)){
	if(i >= pos8){
	MsSpectrum <- rbind(MsSpectrum,x[i])
	}
}

###### spliting these spectrum into differnt lists

	AllSpectrum <- split(MsSpectrum,cumsum(MsSpectrum[,1] == "[MS Spectrum]"))
	zz <- lapply(AllSpectrum,extract) ###### Contains all the formatted spectrum in list form

########### Extracting Data matrix from MC Peak Table and individual Mass Spectrum
########## Peak,RT,Start-RT, End-RT
######### Peak base m/z  extract and sum of relative intensity (TIC)

        data <- peaks[,1:4]
        data <- na.omit(data)

        total <- lapply(zz,FinalTable)
        total <- t(sapply(total,'[',1:max(sapply(total,length))))
        tt <- cbind(data,total)
	tt <- as.matrix(tt)
        colnames(tt) <- c("Peaks","RT","StartRT","EndRT","MSPeaks","MZ","TIC")
        tt <- gsub("m/z","",tt)
        tt <- gsub("\\(.*)","",tt)
	tt <- data.frame(tt)
       	
	new("parse.Shimadzu",McPeakTable=peaks,Annotation=Annotation,GasChromatogram=y,MassSpectrum = zz,FinalTable=tt)
		
	
}


