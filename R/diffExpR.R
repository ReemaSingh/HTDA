#################################################
# Written by Dr. Reema Singh #
#################################################
############### Differentiall Expression method for RNASeq Data ##########

diffExpR <- function(object,comparison=comparison,...){
        state <- object@phenoData$condition
        f <- factor(state)
        design <- model.matrix(~0+f)
        colnames(design) <- levels(f)
        pdf("Mean-varianceTrend.pdf")
        linmod <- limma::voom(object@qData,design,plot=TRUE)
        dev.off()
        fit <- limma::lmFit(linmod,design)
        contrast.matrix <- limma::makeContrasts(comparison,levels=design)
        fit <- limma::contrasts.fit(fit, contrast.matrix)
        ebayes <- limma::eBayes(fit)
        object@diffExp <- limma::topTable(ebayes,number = Inf)
        object@DFsummary <- data.frame(summary(limma::decideTests(fit)))
        new("PreProcessData",object)
        }

