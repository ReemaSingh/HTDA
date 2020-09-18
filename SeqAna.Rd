\name{SeqAna}
\alias{SeqAna}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for the annotation of assembled Transcriptome
}
\description{
	This function used to annotate the newly assemble transcriptome by using blast, SignalP to predict signal peptide and Hmm to identify protein domain. 
}
\usage{
SeqAna(file, blastdb, nthread = "8", mtras = "1", ..)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
	transdecore pep file generated after assembly 
}
  \item{blastdb}{
	uniprot database
}
  \item{nthread}{
	number of thread used
}
  \item{mtras}{
	maximum target
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
function (file, blastdb, nthread = "8", mtras = "1", ..) 
{
    command1 <- paste("makeblastdb -in", blastdb, "-dbtype prot")
    system(command1)
    command2 <- paste("blastp -query", file, "-db", blastdb, 
        "-num_threads", nthread, "-max_target_seqs", mtras, "-outfmt 6")
    output <- system(command2, intern = TRUE)
    output <- strsplit(output, "\\t")
    output <- do.call(rbind.data.frame, output)
    colnames(output) <- c("QueryID", "SubjectID", "Pidentity", 
        "AligLength", "Mismatch", "GapOpening", "Q-Start", "Q-End", 
        "s-Start", "s-end", "Evalue", "bit")
    command3 <- paste("signalp -f short", file)
    sigpep <- system(command3, intern = TRUE)
    sigpep1 <- strsplit(sigpep, " +")
    sigpep1 <- do.call(rbind.data.frame, sigpep1)
    colnames(sigpep1) <- c("name", "Cmax", "pos", "Ymax", "pos", 
        "Smax", "pos", "Smean", "D", "?", "Dmaxcut", "Networks-used")
    sigpep1 <- sigpep1[-c(1:2), ]
    sigpep1 <- sigpep1[, -13]
    comnd5 <- paste("hmmpress", db)
    system(comnd5)
    command5 <- paste("hmmscan --cpu 2 --domtblout TrinotatePFAM.out", 
        db, file)
    hmout <- system(command5, intern = TRUE)
    hmout1 <- strsplit(hmout, "\\t")
    hmout1 <- do.call(rbind.data.frame, hmout1)
    new("SequnecAnalysisData", Homologus = output, signalPeptide = sigpep1, 
        HMM = hmout1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
