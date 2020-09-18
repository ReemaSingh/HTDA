\name{DenovoAsmAnnoT}
\alias{DenovoAsmAnnoT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Function for denovo Transcriptomics Assembly
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DenovoAsmAnnoT(command, file1, file2, type, seqtype, ram, lib, cpu..)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{command}{
	Full path where trinity installed
}
  \item{file1}{
	In case of paired end file, this file will be the file which has all left read
}
  \item{file2}{
	File with right reads
}
  \item{type}{
	Paired or Single end reads
}
  \item{seqtype}{
	Read format i.e. Paired or Single
}
  \item{ram}{	
	Availble ram
}
  \item{lib}{
	Library used if strand specific reads
}
  \item{cpu..}{
	number of processor
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (command, file1, file2, type, seqtype, ram, lib, cpu..) 
{
    command1 <- paste(command, "Trinity.pl", sep = "")
    if (type == "single") {
        command2 <- paste(command1, "--seqType", seqtype, "--JM", 
            ram, "--single", file1, "--SS_lib_type", lib, "--CPU", 
            cpu, "--no_cleanup", "--monitoring")
    }
    if (type == "single" & lib == "") {
        command2 <- paste(command1, "--seqType", seqtype, "--JM", 
            ram, "--single", file1, "--CPU", cpu, "--no_cleanup", 
            "--monitoring")
    }
    if (type == "paired") {
        command2 <- paste(command1, "--seqType", seqtype, "--JM", 
            ram, "--left", file1, "--right", file2, "--SS_lib_type", 
            lib, "--CPU", cpu, "--no_cleanup", "--monitoring")
    }
    if (type == "paired" & lib == "") {
        command2 <- paste(command1, "--seqType", seqtype, "--JM", 
            ram, "--left", file1, "--right", file2, "--CPU", 
            cpu, "--no_cleanup", "--monitoring")
    }
    system(command2)
    command3 <- paste(command, "util/TrinityStats.pl", sep = "")
    fasta <- "trinity_out_dir/Trinity.fasta"
    command4 <- paste(command3, fasta)
    AssembledStat <- system(command4, intern = TRUE)
    AssembledStat <- gsub("\\t", "", AssembledStat)
    AssembledStat <- strsplit(AssembledStat, "\\n")
    AssembledStat <- do.call(rbind.data.frame, AssembledStat)
    colnames(AssembledStat) <- "AssembledStat"
    com <- paste(command, "trinity-plugins/transdecoder/TransDecoder", 
        sep = "")
    com1 <- paste(com, "-t", fasta)
    system(com1)
    new("SequnecAnalysisData", AssembStat = AssembledStat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
