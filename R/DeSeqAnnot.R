#################################################
# Written by Dr. Reema Singh #
#################################################

        SeqAna <- function(file,blastdb,nthread="8",mtras="1",..){
        ##### Blast     
        command1 <- paste("makeblastdb -in",blastdb,"-dbtype prot")
        system(command1)
        command2 <- paste("blastp -query",file,"-db",blastdb,"-num_threads",nthread,"-max_target_seqs",mtras,"-outfmt 6")
        output <- system(command2,intern=TRUE)
        output<- strsplit(output,"\t")
        output <- do.call(rbind.data.frame,output)
        colnames(output) <- c("QueryID","SubjectID","Pidentity","AligLength","Mismatch","GapOpening","Q-Start","Q-End","s-Start","s-end","Evalue","bit")

	##### SignalP to predict signal peptide
        command3 <- paste("signalp -f short",file)
        sigpep <- system(command3,intern=TRUE)
        sigpep1 <- strsplit(sigpep, " +")
        sigpep1 <- do.call(rbind.data.frame,sigpep1)
        colnames(sigpep1) <- c("name","Cmax","pos","Ymax","pos","Smax","pos","Smean","D","?","Dmaxcut","Networks-used")
        sigpep1 <- sigpep1[-c(1:2), ]
        sigpep1 <- sigpep1[, -13]
        #rownames(sigpep1) <- sigpep1$name
        ###tmHMM
        #command4 <- paste("tmhmm --short",file)
        #tmout <- system(command4,intern=TRUE)
        #tmout1<- strsplit(tmout,"\t")
        #tmout1 <- do.call(rbind.data.frame,tmout1)
        #colnames(tmout1) <- c("Id","Length","ExpectNoAA","First60","PredTranHelix","Topology")
        #rownames(tmout1) <- tmout1$Id
	### Hmm to identify protein domain
	comnd5 <- paste("hmmpress",db)
	system(comnd5)
	command5 <- paste("hmmscan --cpu 2 --domtblout TrinotatePFAM.out",db,file)
	hmout <- system(command5,intern=TRUE)
	hmout1<- strsplit(hmout,"\t")
        hmout1 <- do.call(rbind.data.frame,hmout1)
	new("SequnecAnalysisData",Homologus=output,signalPeptide=sigpep1,HMM=hmout1)
        }


	
