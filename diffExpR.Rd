\name{diffExpR}
\alias{diffExpR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for RNASeq Differential Expression
}
\description{
 Identified differentially expressed genes from two different condition
}
\usage{
diffExpR(object, comparison = comparison, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Quantified data obtained from  rnaseqProcess function
}
  \item{comparison}{
	 name of comparing conditions, currently working is "Control-Treated"
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
	The function based on the following flow

        1. First extrcat the quantified data from  PreProcessData S4 class objec
t.
        2. Extract the corresponding phenodata information.
        3. Use the Limma functionality for creating design and contrast matrix
        4. calculate differential expression

}
\value{
	Return an object of S4 class PreProcessData.
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
function (object, comparison = comparison, ...) 
{
    state <- object@phenoData$condition
    f <- factor(state)
    design <- model.matrix(~0 + f)
    colnames(design) <- levels(f)
    pdf("Mean-varianceTrend.pdf")
    linmod <- limma::voom(object@qData, design, plot = TRUE)
    dev.off()
    fit <- limma::lmFit(linmod, design)
    contrast.matrix <- limma::makeContrasts(comparison, levels = design)
    fit <- limma::contrasts.fit(fit, contrast.matrix)
    ebayes <- limma::eBayes(fit)
    object@diffExp <- limma::topTable(ebayes, number = Inf)
    object@DFsummary <- data.frame(summary(limma::decideTests(fit)))
    new("PreProcessData", object)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
