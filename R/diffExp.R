#################################################
# Written by Dr. Reema Singh #
#################################################
############### Differentiall Expression method##########

        diffExp <- function(object,comparison=NULL,volcanoPlot=FALSE,...){
        state <- object@phenoData$Treatment
        f <- factor(state)
        design <- model.matrix(~0+f)
        colnames(design) <- levels(f)
        contrast.matrix <- limma::makeContrasts(comparison,levels=design)
        fit <- limma::lmFit(object@qData,design)
        fit <- limma::contrasts.fit(fit, contrast.matrix)
        ebayes <- limma::eBayes(fit)
        if(volcanoPlot==TRUE){
        jpeg("Valcanoplot.jpg")
        limma::volcanoplot(ebayes, coef=1, highlight=0, names=fit$genes$ID, xlab="Log Fold Change", ylab="Log Odds", pch=16, cex=0.35)
        dev.off()
        }
        object@diffExp <- limma::topTable(ebayes,number = Inf)
        new("NcmsProcessData",object)
        }

