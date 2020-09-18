\name{extract}
\alias{extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Helper function for parse-shimadzu
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
    p <- gsub("\\t", "  ", unlist(x))
    p1 <- strsplit(p, "  ", fixed = FALSE)
    p2 <- p1[-c(1, 2, 3, 4)]
    f <- t(sapply(p2, "[", 1:max(sapply(p2, length))))
    colnames(f) <- f[1, ]
    f <- f[-1, ]
    tt <- c(t, list(a = p1[1:4], b = f))
    return(tt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
