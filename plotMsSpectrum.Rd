\name{plotMsSpectrum}
\alias{plotMsSpectrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot individual mass spectrum
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotMsSpectrum(x, i)
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
function (x, i) 
{
    no <- x[[i]]$a[[2]]
    no <- gsub("# of Peaks", "Number of Peaks = ", no)
    y <- data.frame(x[[i]]$b)
    colnames(y) <- c("mass", "absolute", "relative")
    plot(as.character(y$mass), as.character(y$relative), type = "h", 
        xlab = "Mass/Charge", ylab = "Relative Intensity", col = ifelse(y$relative == 
            "100.00", "red", "black"), main = no)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
