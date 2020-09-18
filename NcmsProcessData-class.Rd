\name{NcmsProcessData-class}
\Rdversion{1.1}
\docType{class}
\alias{NcmsProcessData-class}
\alias{show,NcmsProcessData-method}

\title{Class \code{"NcmsProcessData"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NcmsProcessData", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{qData}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{phenoData}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{BsData}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{normSpectra}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{diffExp}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{gcIntensity}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{retenTime}:}{Object of class \code{"matrix"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "NcmsProcessData")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("NcmsProcessData")
}
\keyword{classes}
