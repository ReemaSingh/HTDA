#################################################
# Written by Dr. Reema Singh #
#################################################
###### Define S4 object ######

setClass("PreProcessData",representation(qData="matrix",phenoData="data.frame",diffExp = "data.frame",DFsummary = "data.frame",enrichment="data.frame"))

#### Implement Show generic function on S4 Class ######

setMethod("show","PreProcessData",function(object){
        cat('Object of class "PreProcessData"',"\n",
        "The object contains all pre-processed information", "\n",
        "User can access the structure of the object for more information", "\n")
        })


########## Constructor for PreProcessData Class ###########

rnaseqProcess <- function(Gfasta,targets,reference,gff1,type,plot=TRUE){
        command <- paste("bowtie2-build",Gfasta,Gfasta)
        command1 <- paste("bowtie2 -x",Gfasta)
        system(command)
	if(type == "single"){
	for(i in seq(along=targets$Fastq)) {
        input <-  paste("./", targets$Fastq, sep="")
        output <-  paste("./", targets$Fastq, ".sam",sep="")
        command2 <- paste(command1, input[i], "-S", output[i])
        system(command2)
	Rsamtools::asBam(file=output[i], destination=gsub(".sam", "", output[i]), overwrite=TRUE, indexDestination=TRUE)
        unlink(output[i])
        }}

	#if(type=="paired"){
	else{
        for(i in seq(along=targets$Fastq)) {
        input <-  paste("./", targets$file1, sep="")
	input1 <- paste("./",targets$file2, sep="")
        output <-  paste("./", targets$Fastq, ".sam",sep="")
        command2 <- paste(command1, "-1",input[i],"-2",input1[i],"-S", output[i])
        system(command2)
	Rsamtools::asBam(file=output[i], destination=gsub(".sam", "", output[i]), overwrite=TRUE, indexDestination=TRUE)
        unlink(output[i])
	}}
        
	#Rsamtools::asBam(file=output[i], destination=gsub(".sam", "", output[i]), overwrite=TRUE, indexDestination=TRUE)
        #unlink(output[i])
        
	library(Biostrings);
	library(IRanges)
	library(GenomicRanges)

	gtf2GRangesList <- function(myfile="my.gff") {
	gtf <- read.delim(myfile, header=FALSE)
	colnames(gtf) <- c("seqname", "source", "feature", "start", "end", "score", "strand", "frame","attributes")
	chronly <- c(1:22, "X", "Y", "MT")
	gtf <- gtf[as.character(gtf$seqname) %in% chronly, ]
	gene_ids <-  gsub(".*gene_id (.*?);.*", "\\1", gtf$attributes)
	transcript_ids<- gsub(".*transcript_id (.*?);.*", "\\1", gtf$attributes)
	index<-gene_ids!=""
	index2<-transcript_ids!=""

	gene.gr<-GRanges(seqnames=gtf$seqname[index],ranges=IRanges(gtf$start[index],gtf$end[index]),strand=gtf$strand[index],tx_id=transcript_ids[index],gene_id=gene_ids[index])
	gene.gr.list<-split(gene.gr,gene_ids[index])

	transcript.gr<-GRanges(seqnames=gtf$seqname[index2],ranges=IRanges(gtf$start[index2],gtf$end[index2]),strand=gtf$strand[index2],tx_id=transcript_ids[index2],gene_id=gene_ids[index2])
  
	transcript.gr.list<-split(transcript.gr,transcript_ids[index2])

	r<-list()
	r$gene<-gene.gr.list
	r$transcript<-transcript.gr.list
	return(r)
	}

	#source("/homes/rsingh/PersonalWork/HTDA_Revised/DE/TestData/test.R")
	gffsub <- gtf2GRangesList(gff1)

	samples <- as.character(targets$Fastq)
	samplespath <- paste("./", samples, ".bam", sep="")
        names(samplespath) <- samples
	bfl <- Rsamtools::BamFileList(samplespath, index=character())
	countDF2 <- GenomicRanges::summarizeOverlaps(gffsub$gene, bfl, mode="Union", ignore.strand=TRUE)
	countDF2<- as.matrix(assays(countDF2)$counts)

 	if(plot==TRUE){
        pdf("BarPlot.pdf")
        barplot(colSums(countDF2)*1e-6,ylab="Library size (millions)")
        dev.off()
        }

	g <- gffsub$gene
        returnRPKM <- function(counts, g) {
        geneLengthsInKB <- sum(width(g))/1000
        millionsMapped <- sum(counts)/1e+06
        rpm <- counts/millionsMapped
        rpkm <- rpm/geneLengthsInKB
        return(rpkm)
        }
        countDFrpkm <- apply(countDF2, 2, function(x) returnRPKM(counts=x, g=g))

        new("PreProcessData", qData=as.matrix(countDFrpkm), phenoData=phenodata)
	}
