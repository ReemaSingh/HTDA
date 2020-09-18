\name{parse.shimadzu}
\alias{parse.shimadzu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for parsing Shimadzy pre-processed .txt file
}
\description{
Function for parsing shimadzu preprocessed .txt file
}
\usage{
parse.shimadzu(fileName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileName}{
	name of .txt file
}
}
\details{
	This function depend on another three function for parsing .txt preprocessed data file i.e Parse.R, extract and FinalTable. This function stores data after parsing into five different clot of class parse.Shimadzu i.e McPeakTable, annotation, Gas Chromatography, mass spectrum and Finaltable .  Finaltable slot contains a table represnting  GC peaks, Retention time, mass spectrum ion fragmentation peaks for each GC peaks and Total Ion current (TIC).
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
function (fileName) 
{
    conn = file(fileName, open = "r")
    linn = readLines(conn)
    data <- vector()
    for (i in 1:length(linn)) {
        data <- rbind(data, linn[i])
    }
    read.shimadzu <- Parse(data)
    return(read.shimadzu)
    close(conn)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
