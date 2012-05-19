

neaMod <-function (ags, fgs, fgslib = NULL, network, pnet = NULL, nperm = 50,stat = "F", seed = NULL) 
{
	pb <- tkProgressBar(title = "Analysis progress bar", "Analysis is initialized, please wait...", 
			min = 0, max = nperm, width = 300)
	
	if (is.null(fgslib)) {fgslib<-"nolibrary"}
	if (fgslib!="reactome.db" && fgslib!="GO.db"&&  fgslib!="KEGG.db") {
		#if (is.null(fgslib)) {
		if (is.character(fgs)) {
			fgs.type = 5
			fgs.list <- list()
			fgs.list[[1]] <- fgs
			fgs <- fgs.list
		}
		if (!is.list(fgs) && length(fgs) == 1) {
			require(fgslib, character.only = TRUE) || stop("need data package:",fgslib)
			require("GOstats", character.only = TRUE) || stop("need data package:","GOstats")
		}
		if (!is.null(fgslib) && is.character(fgslib)) {
			fgslib <- substr(fgslib, 1, nchar(fgslib) - 3)
		}
	}
	
	if (any(duplicated(ags))) 
		print("Duplicated in AGS: This procedure makes the input(gene symbols) unique")
	ags <- unique(toupper(ags))
	if (is.list(fgs)) {
		fgs <- lapply(fgs, toupper)
		fgs <- lapply(fgs, unique)
	}
	if (is.character(network)) {
		network <- toupper(network)
	}
	if (is.list(network)) {
		network <- lapply(network, toupper)
	}
	
	if (fgslib=="KEGG.db"){
		require(KEGG.db)
		require(org.Hs.eg.db)
		mapped.genes<-AnnotationDbi::as.list(KEGGPATHID2EXTID)  
		fgs<-list()
		for (i in 1:length(mapped.genes)){
			mg.name<-mapped.genes[[i]]
			fgs[[i]] <- unique(unlist(AnnotationDbi::mget(c(mg.name), org.Hs.egSYMBOL, ifnotfound=NA)))  
		}	
		names(fgs)<-names(mapped.genes)
		fgs<-fgs[-which(lapply(fgs,length)==1)]
	}
	if (fgslib=="reactome.db"){
		require(reactome.db)
		require(org.Hs.eg.db)
		mapped.genes<-AnnotationDbi::as.list(reactomePATHID2EXTID)  
		fgs<-list()
		for (i in 1:length(mapped.genes)){
			mg.name<-mapped.genes[[i]]
			fgs[[i]] <- unique(unlist(AnnotationDbi::mget(c(mg.name), org.Hs.egSYMBOL, ifnotfound=NA)))  
		}	
		names(fgs)<-names(mapped.genes)
		fgs<-fgs[-which(lapply(fgs,length)==1)]
	}
	if (fgslib=="GO.db"){
		require(org.Hs.eg.db)
		xx<-AnnotationDbi::as.list(org.Hs.egGO2ALLEGS)
		fgs<-list()
		for (i in 1:length(xx)){
			fgs[[i]] <- unique(names(xx[[i]])) 
		}
		names(fgs)<-names(xx)	
		fgs<-fgs[-which(lapply(fgs,length)==1)]
	}
	if (is.null(pnet)) {
		doperm <- "yes"
	}
	if (is.list(pnet)) {
		doperm <- "no"
		nperm <- length(pnet)
	}
	if (is.character(fgs) && fgs == "MF") {
		fgs.type = 1
	}
	if (is.character(fgs) && fgs == "BP") {
		fgs.type = 2
	}
	if (is.character(fgs) && fgs == "CC") {
		fgs.type = 3
	}
	if (is.character(fgs) && fgs == "KEGG") {
		fgs.type = 4
		fgs.probe <- as.list(get(paste(fgslib, "PATH2PROBE", sep = "")))
	}
	if (is.list(fgs)) {
		fgs.type = 5
		fgs.list <- fgs
	}
	if (fgs.type < 4) {
		fgs.probe <- as.list(get(paste(fgslib, "GO2REACTOMEID", sep = "")))
		fgsname <- names(fgs.probe)
		ont <- Ontology(fgsname)
	}
	if (fgs.type < 5) {
		mapped.genes<-AnnotationDbi::as.list(reactomeGO2REACTOMEID)  
		fgs.list<-list()
		for (i in 1:length(mapped.genes)){
			mg.name<-mapped.genes[[i]]
			fgs.list[[i]] <- unique(unlist(AnnotationDbi::mget(c(mg.name), org.Hs.egSYMBOL, ifnotfound=NA)))  
		}	
		names(fgs.list)<-names(mapped.genes)
		fgs.list<-fgs.list[-which(lapply(fgs.list,length)==1)]
	}
	if (is.character(network)) {
		margin <- countcon(network)
		genes <- margin$genes
	}
	if (is.list(network)) {
		anet <- list2arr0(network)
		margin <- countcon(anet)
		genes <- margin$genes
	}
	if (is.character(network) && stat == "F") {
		netlist2 <- arr2list(network, gsym = genes)
		nlink <- linknum.arr(ags, fgs = fgs.list, netlist2, gnum = TRUE, 
				gsym = genes)
	}
	if (is.list(network) && stat == "F") {
		nlink <- linknum.list(ags, fgs = fgs.list, network, gnum = TRUE, 
				gsym = genes)
	}
	if (is.character(network) && stat == "M") {
		netlist2 <- arr2list(network, gsym = genes)
		nlink <- linknum.arr.csum(ags, fgs = fgs.list, netlist2, 
				gnum = TRUE, gsym = genes)
	}
	if (is.list(network) && stat == "M") {
		nlink <- linknum.list.csum(ags, fgs = fgs.list, network, 
				gnum = TRUE, gsym = genes)
	}
	if (is.null(seed)) {
		set.seed(Sys.time())
	}
	if (!is.null(seed)) {
		set.seed(seed)
	}
	if (stat == "F") {
		res.nlink <- matrix(0, length(fgs.list), nperm)
	}
	if (stat == "M") {
		res.nlink <- vector("list", nperm)
	}
	
	
	### new commands ###
	
	pnetout <- list ()
	geneinfgs <- list()
	numgene <- NULL
	for (i in 1:length(fgs.list) ) {
		geneinfgs [[i]] <- ags[which(ags %in% fgs.list[[i]])]
		numgene[i] <- length(geneinfgs [[i]])
	}
	
	ngenefgs <- sapply(fgs.list, length)
	
	#### 
	
	
	for (i in 1:nperm) {
		if (doperm == "yes" && stat == "F") {
			perm.ncon <- netperm(margin$ncon)
			pos1 <- perm.ncon$pos1
			res.nlink[, i] <- linknum.list(ags, fgs = fgs.list, 
					netlist = pos1, gnum = TRUE, gsym = genes)
			##
			pnetout[[i]] <-   pos1     
			info <- sprintf("%d%% done", round(i/nperm * 100, 0))
			setTkProgressBar(pb, i, title = paste("Permutations are in progress "), info)
			
		}
		
		if (doperm == "no" && stat == "F") {
			res.nlink[, i] <- linknum.list(ags, fgs = fgs.list, 
					netlist = pnet[[i]], gnum = TRUE, gsym = genes)
			##
			info <- sprintf("%d%% done", round(i/nperm * 100, 0))
			setTkProgressBar(pb, i, title = paste("Analysis is in progress "), info) 
		}
		
		if (doperm == "yes" && stat == "M") {
			perm.ncon <- netperm(margin$ncon)
			pos1 <- perm.ncon$pos1
			res.nlink[[i]] <- linknum.list.csum(ags, fgs = fgs.list, 
					netlist = pos1, gnum = TRUE, gsym = genes)
			##
			pnetout[[i]] <-   pos1     
			info <- sprintf("%d%% done", round(i/nperm * 100, 0))
			setTkProgressBar(pb, i, title = paste("Permutations are in progress "), info)
			
		}
		if (doperm == "no" && stat == "M") {
			res.nlink[[i]] <- linknum.list.csum(ags, fgs = fgs.list, 
					netlist = pnet[[i]], gnum = TRUE, gsym = genes)
			##
			info <- sprintf("%d%% done", round(i/nperm * 100, 0))
			setTkProgressBar(pb, i, title = paste("Analysis is in progress "), info) 
		}
	}
	
	##
	close(pb)
	if (doperm == "no") pnetout <- pnet   
	##
	
	
	if (stat == "F") {
		stac.nlink<-cbind(res.nlink,nlink)
		rownames(stac.nlink) <- names(fgs.list)
		mean <- apply(stac.nlink, 1, mean)
		se <- apply(stac.nlink, 1, sd)
		se <- ifelse(se == 0, 1, se)
		stac.zscore <- (stac.nlink - mean)/se
		zscore <- stac.zscore[,ncol(stac.nlink)]
		pvalue<-rep(0,nrow(stac.nlink))
		for (i in 1:nrow(stac.nlink)){
			allperm<-stac.nlink[i,]; nallperm<-length(allperm)  
			pvalue[i] <- min(2*min(sum(nlink[i]<=allperm)/nallperm,sum(nlink[i]>=allperm)/nallperm),1)
		}
		##
		FDR <- pval2FDR(pvalue)
		return(list(nlink = nlink, res.nlink = res.nlink, zscore = zscore, pvalue = pvalue, FDR =FDR,
						ngenefgs =ngenefgs ,numgene= numgene, geneinfgs=geneinfgs, fgs.list =fgs.list, exp.link=mean, pnetout = pnetout))
	}
	if (stat == "M") {
		BSTAR <- array(0, dim = c(nrow(nlink), ncol(nlink), nperm + 1))
		for (i in 1:nperm) {
			BSTAR[, , i] <- res.nlink[[i]]
		}
		BSTAR[, , nperm + 1] <- nlink
		zscore.mat <- zstat(BSTAR)
		pvalue<-rep(0,nrow(zscore.mat))
		for (i in 1:nrow(zscore.mat)){
			allz<-zscore.mat[i,]; nallz<-length(allz) 
			if (!is.nan(zscore.mat[i,nallz])) {pvalue[i]<- min(2*min(sum(zscore.mat[i,nallz]<=allz)/nallz, sum(zscore.mat[i,nallz]>=allz)/nallz),1)}
			if (is.nan(zscore.mat[i,nallz])) {pvalue[i]<-1}
		}
		FDR <- pval2FDR(pvalue)
		return(list(nlink = nlink, res.nlink = res.nlink, zscore.mat = zscore.mat, pvalue = pvalue, FDR =FDR,
						ngenefgs =ngenefgs ,numgene= numgene, geneinfgs=geneinfgs, fgs.list =fgs.list, exp.link=mean, pnetout = pnetout))
	}
}


