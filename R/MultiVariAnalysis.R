#################################################
# Written by Dr. Reema Singh #
#################################################
############### Machine Learning method ######################

        MultiVariAnalysis <- function(object,method="clustering",...){
        if(method=="clustering"){
        clust.genes <- amap::hcluster(x=object@qData,method="pearson",link="average")
        clust.arrays <- amap::hcluster(x=t(object@qData),method="pearson",link="average")
        heatcol <- colorRampPalette(c("Green","Red"))(32)
        heatmap(x=object@qData,Rowv=as.dendrogram(clust.genes),Colv=as.dendrogram(clust.arrays),col=heatcol)
        }
        if(method=="pca"){
        p <- amap::acp(object@qData)
        plot(p)
        return(p)
        }
        if(method=="kmeans"){
        cl <- amap::Kmeans(object@qData, 2)
        plot(object@qData, col = cl$cluster)
        points(cl$centers, col = 1:2, pch = 8, cex=2)
        return(cl)
        }
        }

