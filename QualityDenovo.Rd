\name{QualityDenovo}
\alias{QualityDenovo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Function to check the quality of the assembled Transcriptome
}
\description{
	This function Align the assemble transcript with the genome(if available) by using BLAT. Also, align the reads with the newly assembled transcriptome, the resulting BAM files can be visualize by using IGB/IGB}
\usage{
QualityDenovo(path, file, blastdb, lib, ..)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
	Full path where Trinity installed
}
  \item{file}{
	Newly assembled transcriptome i.e. Trinity.fasta
}
  \item{blastdb}{
	blast databse :- Genome database
}
  \item{lib}{
%%     ~~Describe \code{lib} here~~
}
  \item{..}{
%%     ~~Describe \code{..} here~~
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
function (path, file, blastdb, lib, ..) 
{
    out <- paste(file, "_blat", sep = "")
    command2 <- paste("blat", blastdb, file, "-t=dna -q=dna -out=blast8", 
        out)
    system(command2)
    command3 <- paste(path, "util/analyze_blastPlus_topHit_coverage.pl", 
        sep = "")
    command4 <- paste(command3, out, file, blastdb)
    AssemblesStat <- system(command4, intern = TRUE)
    command5 <- paste(path, "util/alignReads.pl", sep = "")
    command6 <- paste(command5, "--left", file1, "--right", file2, 
        "--seqType", seqtype, "--target", file, "--SS_lib_type", 
        lib, "--aligner bowtie --retain_intermediate_files")
    system(command6)
    command7 <- paste(path, "/sw/opt/trinity/util/SAM_nameSorted_to_uniq_count_stats.pl", 
        sep = "")
    command8 <- paste(command7, "bowtie_out/bowtie_out.nameSorted.sam")
    alignSummar <- system(command8, intern = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
