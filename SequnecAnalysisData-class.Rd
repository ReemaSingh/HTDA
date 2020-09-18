\name{SequnecAnalysisData-class}
\Rdversion{1.1}
\docType{class}
\alias{SequnecAnalysisData-class}
\alias{show,SequnecAnalysisData-method}

\title{Class \code{"SequnecAnalysisData"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SequnecAnalysisData", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{AssembStat}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{Homologus}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{signalPeptide}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{transRegion}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{HMM}:}{Object of class \code{"data.frame"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SequnecAnalysisData")}: ... }
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
showClass("SequnecAnalysisData")
}
\keyword{classes}
