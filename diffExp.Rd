\name{diffExp}
\alias{diffExp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for Differential Expression
}
\description{
Identified differentially expressed genes from two different condition
}
\usage{
diffExp(object, comparison = NULL, volcanoPlot = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Quantified data obtained from gcProcess or preprocess function
}
  \item{comparison}{
	Condition to be compared, currently working is "control-Treated"
}
  \item{volcanoPlot}{
	by default False
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
This function identify differentially expressed genes between two condit
ion. This function is different from DiffExpR function implemented in RNASeq ana
lysis.
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
function (object, comparison = NULL, volcanoPlot = FALSE, ...) 
{
    state <- object@phenoData$Treatment
    f <- factor(state)
    design <- model.matrix(~0 + f)
    colnames(design) <- levels(f)
    contrast.matrix <- limma::makeContrasts(comparison, levels = design)
    fit <- limma::lmFit(object@qData, design)
    fit <- limma::contrasts.fit(fit, contrast.matrix)
    ebayes <- limma::eBayes(fit)
    if (volcanoPlot == TRUE) {
        jpeg("Valcanoplot.jpg")
        limma::volcanoplot(ebayes, coef = 1, highlight = 0, names = fit$genes$ID, 
            xlab = "Log Fold Change", ylab = "Log Odds", pch = 16, 
            cex = 0.35)
        dev.off()
    }
    object@diffExp <- limma::topTable(ebayes, number = Inf)
    new("NcmsProcessData", object)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
