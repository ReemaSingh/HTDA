\name{annotate}
\alias{annotate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for Gene-Set Enrichment Analysis
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
annotate(object, annotation = "ath1121501", ont = "MF", pvalue = 0.5, entrez, entrezid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	Differentially expressed data generated from diffExp function
}
  \item{annotation}{
	bioconductor annotation database default ath1121501
}
  \item{ont}{
	ontology i.e MF,BP,CC default MF
}
  \item{pvalue}{
	 Significant cutoff default 0.05
}
  \item{entrez}{
	yes or no
}
  \item{entrezid}{
	if entrez is yes, then the entrez id object
}
}
\details{
The function calculate Gene-Set Enrichment Analysis (GSEA) using hyperg
eometric t-test for both up and down regulated genes at a logfold cutoof 1 and -
1.
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
function (object, annotation = "ath1121501", ont = "MF", pvalue = 0.5, 
    entrez, entrezid) 
{
    up <- object@diffExp[which(object@diffExp$logFC >= 1), ]
    down <- object@diffExp[which(object@diffExp$logFC >= -1), 
        ]
    de <- rbind(up, down)
    geneUniverse <- rownames(object@qData)
    geneSample <- de$ID
    if (entrez == "no") {
        genesample1 <- entrezid[which(rownames(entrezid) \%in\% 
            geneSample), ]
        geneUniverse1 <- entrezid[which(rownames(entrezid) \%in\% 
            geneUniverse), ]
        params <- new("GOHyperGParams", geneIds = genesample1, 
            universeGeneIds = geneUniverse1, annotation = annotation, 
            ontology = ont, pvalueCutoff = pvalue, conditional = FALSE, 
            testDirection = "over")
        hgOver <- GOstats::hyperGTest(params)
    }
    else {
        params <- new("GOHyperGParams", geneIds = geneSample, 
            universeGeneIds = geneUniverse, annotation = annotation, 
            ontology = ont, pvalueCutoff = pvalue, conditional = FALSE, 
            testDirection = "over")
        hgOver <- GOstats::hyperGTest(params)
    }
    object@enrichment <- summary(hgOver)
    new("PreProcessData", object)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
