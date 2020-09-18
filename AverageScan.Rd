\name{AverageScan}
\alias{AverageScan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to extract average mass/charge and intensity value for every scan
}
\description{
Return mass/charge ration, intensity for the given peak
}
\usage{
	lapply(Sample1@MassSpectrum, AverageScan)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	All Mass Spectrum
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Return a list of average mass/charge ration and relative intensity for e
very scan
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
function (x) 
{
    y <- data.frame(x$b)
    colnames(y) <- c("mass", "absolute", "relative")
    aveMZ <- mean(as.numeric(as.character(y$mass)))
    aveAbs <- mean(as.numeric(as.character(y$absolute)))
    aveRel <- mean(as.numeric(as.character(y$relative)))
    averageSpectrum <- data.frame(MZ = aveMZ, Absolute = aveAbs, 
        Relative = aveRel)
    return(averageSpectrum)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
