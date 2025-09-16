#################################################
# Written by Dr. Reema Singh #
#################################################
######### Geneset Enrichment Analysis ###########
	#### Gene Ontology (GO) anaysis of Differential Expressed Genes
        annotate <- function(object,annotation="ath1121501",ont="MF",pvalue=0.5,entrez,entrezid){
        up <- object@diffExp[which(object@diffExp$logFC >= 1), ]
        down <- object@diffExp[which(object@diffExp$logFC >= -1), ]
        de <- rbind(up,down)
        geneUniverse <- rownames(object@qData)
        geneSample <- de$ID
	### DEG should have Entrez ID idenifier.In case identifier is different then first step is to extract the coorsponding entrez identifier 
	if(entrez=="no"){
	genesample1 <- entrezid[which(rownames(entrezid) %in% geneSample),]
	geneUniverse1 <- entrezid[which(rownames(entrezid) %in% geneUniverse),]
	params <- new("GOHyperGParams", geneIds =genesample1, universeGeneIds =geneUniverse1,annotation=annotation, ontology = ont, pvalueCutoff = pvalue,conditional = FALSE, testDirection = "over")	
	hgOver <- GOstats::hyperGTest(params)
	}
	else{
        params <- new("GOHyperGParams", geneIds = geneSample, universeGeneIds = geneUniverse,annotation=annotation, ontology = ont, pvalueCutoff = pvalue, conditional = FALSE, testDirection = "over")
        hgOver <- GOstats::hyperGTest(params) 
	}
	####  return(hgOver)	
	object@enrichment <- summary(hgOver) 
	new("PreProcessData",object)
}

