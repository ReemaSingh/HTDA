extract <- function(x){
        p <- gsub("\t","  ",unlist(x))
        p1 <- strsplit(p,"  ",fixed=FALSE)
        p2 <- p1[-c(1,2,3,4)]
        f <- t(sapply(p2,'[',1:max(sapply(p2,length))))
        colnames(f) <- f[1,]
        f <- f[-1,]
        tt <- c(t,list(a=p1[1:4],b=f))
        return(tt)
}


