\name{gcProcess}
\alias{gcProcess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for GCMS data preprocessing
}
\description{
In gcProcess function we have inherited function for filteration, peak i
dentification, group, retentiontime correction from XCMS and annotation from CAM
ERA package.
}
\usage{
gcProcess(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
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
function (object, ...) 
{
    cdffiles <- list.files(object, recursive = TRUE, full.names = TRUE)
    xset <- xcms::xcmsSet(cdffiles)
    xset <- xcms::group(xset, bw = 20, mzwid = 0.01, minfrac = 1)
    xset2 <- xcms::retcor(xset, family = "symmetric", plottype = "mdevden")
    xset2 <- xcms::group(xset2, bw = 20)
    xset3 <- xcms::fillPeaks(xset2)
    an <- CAMERA::xsAnnotate(xset3)
    a <- CAMERA::getPeaklist(an)
    dat <- as.matrix(a[, 10:16])
    colnames(dat) <- rownames(xset@phenoData)
    new("NcmsProcessData", qData = dat, phenoData = phenodata1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
