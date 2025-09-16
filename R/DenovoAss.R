#################################################
# Written by Dr. Reema Singh #
#################################################
### Creating S4 object
setClass("SequnecAnalysisData",representation(AssembStat="data.frame",Homologus="data.frame",signalPeptide="data.frame",transRegion="data.frame",HMM="data.frame"))

setMethod("show","SequnecAnalysisData",function(object){
        cat('Object of class "SequnecAnalysisData"',"\n",
        "The object contains all information about denovo assembly sequence analysis", "\n",
        "User can access the structure of the object for more information", "\n")
        })

###### Transcriptomics Assembly using Trinity of both single as well paired data
DenovoAsmAnnoT <- function(command,file1,file2,type,seqtype,ram,lib,cpu..){
        command1 <- paste(command,"Trinity.pl",sep="")
        if(type=="single"){
        command2 <- paste(command1,"--seqType",seqtype,"--JM",ram,"--single",file1,"--SS_lib_type",lib,"--CPU",cpu,"--no_cleanup","--monitoring")
        }
	if(type=="single" & lib==""){
	command2 <- paste(command1,"--seqType",seqtype,"--JM",ram,"--single",file1,"--CPU",cpu,"--no_cleanup","--monitoring")
	}
        if(type=="paired"){
        command2 <- paste(command1,"--seqType",seqtype,"--JM",ram,"--left",file1,"--right",file2,"--SS_lib_type",lib,"--CPU",cpu,"--no_cleanup","--monitoring")
        }
	if(type=="paired" & lib==""){
	command2 <- paste(command1,"--seqType",seqtype,"--JM",ram,"--left",file1,"--right",file2,"--CPU",cpu,"--no_cleanup","--monitoring")
	}

        system(command2)

####### After alignment next step is find out the quality of the newly assembled transcript
	command3 <- paste(command,"util/TrinityStats.pl",sep="")
	fasta <- "trinity_out_dir/Trinity.fasta"
	command4 <- paste(command3,fasta)
	AssembledStat <- system(command4,intern=TRUE)
	AssembledStat <-  gsub("\t","",AssembledStat)
	AssembledStat <- strsplit(AssembledStat,"\n")
	AssembledStat <- do.call(rbind.data.frame,AssembledStat)
	colnames(AssembledStat) <- "AssembledStat"

	com <- paste(command,"trinity-plugins/transdecoder/TransDecoder",sep="")
        com1 <- paste(com,"-t",fasta)
        system(com1)

	new("SequnecAnalysisData",AssembStat=AssembledStat)

	}



