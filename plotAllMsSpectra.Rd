\name{plotAllMsSpectra}
\alias{plotAllMsSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function for plotting all Mass Spectrum
}
\description{
Plot all Mass spectrum simultaneously and write them  in a pdf file
}
\usage{
lapply(x,plotAllMsSpectra)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	MassSpectrm slot of S4 class parse.Shimadzu
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
function (x) 
{
    no <- x$a[[2]]
    no <- gsub("# of Peaks", "Number of Peaks = ", no)
    y <- data.frame(x$b)
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
