\name{preprocess}
\alias{preprocess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Function for processing raw LCMS data
}
\description{
	The preprocess function denoise, baseline correct, align, identified peaks and normalize them. The results can be visualize by thr baseline corrected and peak identification plots.
}
\usage{
preprocess(file, phenoData, method = "loess", cutoff = 100, plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
	name and location  of the folder having raw LCMS data
}
  \item{phenoData}{
	text file contains name and condition of each file
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{cutoff}{
%%     ~~Describe \code{cutoff} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (file, phenoData, method = "loess", cutoff = 100, plot = TRUE, 
    ...) 
{
    library(PROcess)
    if (plot == TRUE) {
        pdf("BaselineCorrection.pdf")
        bscorr <- rmBaseline(file, method = method)
        dev.off()
        normSpect <- PROcess::renorm(bscorr, cutoff = cutoff)
        t <- as.matrix(rowMeans(normSpect))
        tt <- cbind(rownames(t), t[, 1])
        mode(tt) <- "double"
        pdf("PeakIdentified.pdf")
        Peaks <- PROcess::isPeak(tt[tt[, 1] > cutoff, ], plot = plot, 
            zerothrsh = 1, ratio = 0.1)
        dev.off()
        grandpvec <- round(Peaks[Peaks$peak, "mz"])
        Quanti <- PROcess::getPeaks2(normSpect, grandpvec)
        pData <- data.frame(phenoData)
        new("NcmsProcessData", qData = Quanti, phenoData = pData, 
            BsData = bscorr, normSpectra = normSpect)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
