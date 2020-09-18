\name{Parse}
\alias{Parse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Another helper function for parse-Shimadzu
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Parse(x)
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
    pos1 <- which(x == "[Header]")
    pos2 <- which(x == "[File Information]")
    pos3 <- which(x == "[Sample Information]")
    pos4 <- which(x == "[MC Peak Table]")
    pos5 <- which(x == "[Spectrum Process Table]")
    pos6 <- which(x == "[MS Similarity Search Results for Spectrum Process Table]")
    pos7 <- which(x == "[MS Chromatogram]")
    pos8 <- which(x == "[MS Spectrum]")
    header = vector()
    for (i in 1:length(x)) {
        if (i < pos2) {
            header <- rbind(header, x[i])
        }
    }
    SamInfo <- vector()
    for (i in 1:length(x)) {
        if ((i >= pos3) && (i < pos4)) {
            SamInfo <- rbind(SamInfo, x[i])
        }
    }
    PeakTable <- vector()
    for (i in 1:length(x)) {
        if ((i >= pos4) && (i < pos5)) {
            PeakTable <- rbind(PeakTable, x[i])
            PeakTable <- gsub("\\t", "   ", PeakTable)
        }
    }
    TotalPeak <- PeakTable[2, ]
    peaks <- PeakTable[-c(1:3), ]
    peaks <- strsplit(peaks, "  ", fixed = FALSE)
    peaks <- t(sapply(peaks, "[", 1:max(sapply(peaks, length))))
    colnames(peaks) <- peaks[1, ]
    peaks <- peaks[-1, ]
    peaks <- peaks[, -c(12:13)]
    peaks <- data.frame(peaks)
    Spectrum <- vector()
    for (i in 1:length(x)) {
        if ((i >= pos5) && (i < pos6)) {
            Spectrum <- rbind(Spectrum, x[i])
        }
    }
    Annotation <- vector()
    for (i in 1:length(x)) {
        if ((i >= pos6) && (i < pos7)) {
            Annotation <- rbind(Annotation, x[i])
            Annotation <- gsub("\\t", "   ", Annotation)
        }
    }
    Chromatogram <- vector()
    for (i in 1:length(x)) {
        if ((i >= pos7) && (i < pos8[1])) {
            Chromatogram <- rbind(Chromatogram, x[i])
        }
    }
    y <- Chromatogram[-c(1:6), ]
    y <- gsub("\\t", "  ", y)
    y <- strsplit(y, "  ", fixed = FALSE)
    y <- t(sapply(y, "[", 1:max(sapply(y, length))))
    colnames(y) <- y[1, ]
    y <- y[-1, ]
    y <- data.frame(y)
    MsSpectrum <- vector()
    for (i in 1:length(x)) {
        if (i >= pos8) {
            MsSpectrum <- rbind(MsSpectrum, x[i])
        }
    }
    AllSpectrum <- split(MsSpectrum, cumsum(MsSpectrum[, 1] == 
        "[MS Spectrum]"))
    zz <- lapply(AllSpectrum, extract)
    data <- peaks[, 1:4]
    data <- na.omit(data)
    total <- lapply(zz, FinalTable)
    total <- t(sapply(total, "[", 1:max(sapply(total, length))))
    tt <- cbind(data, total)
    tt <- as.matrix(tt)
    colnames(tt) <- c("Peaks", "RT", "StartRT", "EndRT", "MSPeaks", 
        "MZ", "TIC")
    tt <- gsub("m/z", "", tt)
    tt <- gsub("\\\(.*)", "", tt)
    tt <- data.frame(tt)
    new("parse.Shimadzu", McPeakTable = peaks, Annotation = Annotation, 
        GasChromatogram = y, MassSpectrum = zz, FinalTable = tt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
