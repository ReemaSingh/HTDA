#################################################
# Written by Dr. Reema Singh #
#################################################
###### Define S4 object ######

setClass("NcmsProcessData",representation(qData="matrix",phenoData="data.frame",BsData="matrix",normSpectra="matrix",diffExp = "data.frame",gcIntensity="matrix",retenTime="matrix"))

#### Implement Show generic function on S4 Class ######

setMethod("show","NcmsProcessData",function(object){
	cat('Object of class "NcmsProcessData"',"\n",
	"The object contains all pre-processed information", "\n",
	"User can access the structure of the object for more information", "\n")
	})

######### Pre-processing Function(Constructor for PreProcessData class  ###############

	preprocess <- function(file,phenoData,method="loess",cutoff=100,plot=TRUE,...){
	library(PROcess)
	if(plot==TRUE){
	pdf("BaselineCorrection.pdf")
	bscorr <- rmBaseline(file,method=method)
	dev.off()
	normSpect <- PROcess::renorm(bscorr,cutoff=cutoff)
	t <- as.matrix(rowMeans(normSpect))
	tt <- cbind(rownames(t),t[,1])
	mode(tt) <- "double"
	pdf("PeakIdentified.pdf")
	Peaks <- PROcess::isPeak(tt[tt[,1] > cutoff,],plot=plot,zerothrsh=1,ratio=0.1)	
	dev.off()
	grandpvec <- round(Peaks[Peaks$peak, "mz"])
	Quanti <- PROcess::getPeaks2(normSpect, grandpvec)
	pData <- data.frame(phenoData)
	new("NcmsProcessData",qData=Quanti,phenoData=pData,BsData=bscorr,normSpectra=normSpect)

}}

####### GCMS pre-processing ########

	gcProcess <- function(object,...){
	cdffiles <- list.files(object, recursive=TRUE, full.names=TRUE)
	xset <- xcms::xcmsSet(cdffiles)  ############## filter and identify peaks
	xset <- xcms::group(xset,bw=20, mzwid=0.01, minfrac=1)  ############ match peaks across samples 
	xset2 <- xcms::retcor(xset, family = "symmetric", plottype = "mdevden") ###############  retention time correction 
	xset2 <- xcms::group(xset2, bw = 20)  
	xset3 <- xcms::fillPeaks(xset2)    ############# fill in missing peak data 
	an <- CAMERA::xsAnnotate(xset3) ############ extracts the peaktable from a provided xcmsSet
	a <- CAMERA::getPeaklist(an)
	dat <- as.matrix(a[,10:16])
	colnames(dat) <- rownames(xset@phenoData)
	new("NcmsProcessData",qData=dat,phenoData=phenodata1)
        }

