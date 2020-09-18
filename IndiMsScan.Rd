\name{IndiMsScan}
\alias{IndiMsScan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to extract m/z and intensity from each scan of Mass Spectrum
}
\description{
Return mass/charge ration, absolute intensity and relative intensity for the given peak.
}
\usage{
IndiMsScan(x, i)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	 MassSpectrm slot of S4 class parse.Shimadzu.
}
  \item{i}{
	series number of peaks which you want to plot
}
}
\details{
	 Function to extract m/z and intensity from each scan of Mass Spectrum
}
\value{
	 Return a data.frame of mass/charge ration, absolute intensity and relative intensity
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
function (x, i) 
{
    y <- data.frame(x[[i]]$b)
    colnames(y) <- c("mass", "absolute", "relative")
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
