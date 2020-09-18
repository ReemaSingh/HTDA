\name{MultiVariAnalysis}
\alias{MultiVariAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for Multivariant Analysis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MultiVariAnalysis(object, method = "clustering", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	preprocessing data from gcms, lcms and rnaseq

}
  \item{method}{
	clustering methods
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (object, method = "clustering", ...) 
{
    if (method == "clustering") {
        clust.genes <- amap::hcluster(x = object@qData, method = "pearson", 
            link = "average")
        clust.arrays <- amap::hcluster(x = t(object@qData), method = "pearson", 
            link = "average")
        heatcol <- colorRampPalette(c("Green", "Red"))(32)
        heatmap(x = object@qData, Rowv = as.dendrogram(clust.genes), 
            Colv = as.dendrogram(clust.arrays), col = heatcol)
    }
    if (method == "pca") {
        p <- amap::acp(object@qData)
        plot(p)
        return(p)
    }
    if (method == "kmeans") {
        cl <- amap::Kmeans(object@qData, 2)
        plot(object@qData, col = cl$cluster)
        points(cl$centers, col = 1:2, pch = 8, cex = 2)
        return(cl)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
