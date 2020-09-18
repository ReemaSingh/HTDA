### Check the quality of the newly assembled transcriptomes by aligning it with the existing genome(if available)
#using BLAT as well as aligning reads with the newly assembled transcript and the resulting alignment can visualize by using IGV/IGB

QualityDenovo <- function(path,file,blastdb,lib,..){
out <- paste(file,"_blat",sep="")
command2 <- paste("blat",blastdb,file,"-t=dna -q=dna -out=blast8",out)
system(command2)
command3 <- paste(path,"util/analyze_blastPlus_topHit_coverage.pl",sep="")
command4 <- paste(command3,out,file,blastdb)
AssemblesStat <- system(command4,intern=TRUE)

command5 <- paste(path,"util/alignReads.pl",sep="")

command6 <- paste(command5,"--left",file1,"--right",file2,"--seqType",seqtype,"--target",file,"--SS_lib_type",lib, "--aligner bowtie --retain_intermediate_files")
system(command6)

command7 <- paste(path,"/sw/opt/trinity/util/SAM_nameSorted_to_uniq_count_stats.pl",sep="")
command8 <- paste(command7,"bowtie_out/bowtie_out.nameSorted.sam")
alignSummar <- system(command8,intern=TRUE)
}


