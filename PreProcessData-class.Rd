\name{PreProcessData-class}
\Rdversion{1.1}
\docType{class}
\alias{PreProcessData-class}
\alias{show,PreProcessData-method}

\title{Class \code{"PreProcessData"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PreProcessData", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{qData}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{phenoData}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{diffExp}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{DFsummary}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{enrichment}:}{Object of class \code{"data.frame"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "PreProcessData")}: ... }
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
showClass("PreProcessData")
}
\keyword{classes}
