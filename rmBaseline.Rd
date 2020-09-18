\name{rmBaseline}
\alias{rmBaseline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Baseline Correction
}
\description{
This function we have exported from PROcess package with a little modification.
}
\usage{
rmBaseline(fldr, bseoffrda = NULL, breaks = 200, qntl = 0, method = "loess", bw = 0.1, SpecNames = list.files(fldr, pattern = "\\.*csv\\.*"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fldr}{
%%     ~~Describe \code{fldr} here~~
}
  \item{bseoffrda}{
%%     ~~Describe \code{bseoffrda} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{qntl}{
%%     ~~Describe \code{qntl} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{bw}{
%%     ~~Describe \code{bw} here~~
}
  \item{SpecNames}{
%%     ~~Describe \code{SpecNames} here~~
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
function (fldr, bseoffrda = NULL, breaks = 200, qntl = 0, method = "loess", 
    bw = 0.1, SpecNames = list.files(fldr, pattern = "\\\.*csv\\\.*")) 
{
    fs <- list.files(fldr, pattern = "\\\.*csv\\\.*", full.names = T)
    n <- length(fs)
    par(mfrow = c(3, 3))
    for (j in 1:n) {
        f1 <- read.files(fs[j])
        fcut <- f1[f1[, 1] > 0, ]
        bseoff <- PROcess::bslnoff(fcut, breaks = breaks, qntl = qntl, 
            method = method, plot = TRUE, bw = bw)
        if (j > 1) 
            bseoffM <- cbind(bseoffM, bseoff[, 2])
        else bseoffM <- bseoff[, 2]
    }
    dimnames(bseoffM) <- list(signif(bseoff[, 1], 6), SpecNames)
    if (!is.null(bseoffrda)) 
        save(list = bseoffM, file = bseoffrda)
    bseoffM
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
