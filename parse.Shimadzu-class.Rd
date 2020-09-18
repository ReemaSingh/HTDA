\name{parse.Shimadzu-class}
\Rdversion{1.1}
\docType{class}
\alias{parse.Shimadzu-class}
\alias{show,parse.Shimadzu-method}

\title{Class \code{"parse.Shimadzu"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("parse.Shimadzu", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{McPeakTable}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{Annotation}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{GasChromatogram}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{MassSpectrum}:}{Object of class \code{"list"} ~~ }
    \item{\code{FinalTable}:}{Object of class \code{"data.frame"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "parse.Shimadzu")}: ... }
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
showClass("parse.Shimadzu")
}
\keyword{classes}
