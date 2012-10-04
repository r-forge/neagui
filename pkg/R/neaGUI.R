

neaGUI <-
		function() {
	
	
	is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
	
	instbioc <- function (pkg) {
		source("http://www.bioconductor.org/biocLite.R")
		biocLite(pkg)
	}
	instcran <- function (pkg) {
		install.packages(pkg)
	}
	
	
	pkgcrnls <- c("tcltk","hwriter" )
	pkgbiocls <- c("AnnotationDbi","KEGG.db", "GO.db", "org.Hs.eg.db", "reactome.db")
	pkgls <- c(pkgcrnls ,pkgbiocls )
	allins <- sapply(pkgls,is.installed)
	
	if (all(allins) == F ) {
		
		if (is.installed("tcltk")) {
			require (tcltk)
			intl <- tkmessageBox(title="Required packages installation", message="Some required packages are not yet installed, would you like install to install it now? For installation internet connection is needed!",icon="question",type="yesno",default="yes")
			if (tclvalue(intl) == "no") stop("Required packages are not installed")
			else {
				notins <- names(allins[allins==F]) 
				for (i in notins) {
					if (i %in% pkgcrnls ) instcran (i)
					else {
						instbioc (i)
					}
				}
				
			}
			
		}
		else {
			
			stop ("tcltk package is not installed, please install it first!")
			
		}
		
	}
	
	checkObject <- function (x,del=TRUE) {
			if (del==F) {
				exists(x, envir=.GlobalEnv)
			} 
			else {
				if(exists(x, envir=.GlobalEnv) ) remove(list=x, envir=.GlobalEnv)
			}
			
		}
		
		## remove output from previous analysis ###
		checkObject ("AGS")
		checkObject ("FGS")
		checkObject ("NETWORK")
		checkObject ("AnnotationDB")
		checkObject ("NETWORK")
		
		require(tcltk)
#require(nea)
		require(AnnotationDbi)
		require(KEGG.db)
		require(GO.db)
		require(org.Hs.eg.db)
		require(hwriter)
		tclRequire("BWidget")
		require(reactome.db)
		
		tt <<- tktoplevel()
		tkwm.title(tt,"neaGUI") 
		tkfocus(tt)
#initdir <- "C:/Users/Administrator/Documents"
#assign("initdir",initdir, envir = .GlobalEnv)
		spec.frm <- tkframe(tt,borderwidth=2)
		frame1 <- tkframe(spec.frm, relief="groove", borderwidth=2)
#frame2 <- tkframe(spec.frm, relief="groove", borderwidth=2)
		
		frame11 <- tkframe(frame1, borderwidth=2)
		frame12 <- tkframe(frame1, borderwidth=2)
		frame13 <- tkframe(frame1, borderwidth=2)
		frame14 <- tkframe(frame1, borderwidth=2)
		frame15 <- tkframe(frame1, borderwidth=2)
		
#frame3 <- tkframe(spec.frm, relief="groove", borderwidth=2)
#frame4 <- tkframe(spec.frm, relief="groove", borderwidth=2)
		frame5 <- tkframe(spec.frm, relief="groove", borderwidth=2)
		frame51 <- tkframe(frame5, borderwidth=2)
		frame52 <- tkframe(frame5, borderwidth=2)
		frame53 <- tkframe(frame5, borderwidth=2)
		
		frame6 <- tkframe(spec.frm, relief="groove", borderwidth=2)
		
		buttonFrame <-tkframe(tt,borderwidth=2)
		
		fontHeading <- tkfont.create(size=10,weight="bold")
		
		fileAgs <<- tclVar("        ")
		Browse.but1 <-tkbutton(frame11,text="Browse",command=getAgs )
		AgsEntry <-tkentry(frame11,width="34",textvariable=fileAgs )
		
		lab0 <- tklabel(frame11,text="Specify the input: ",font=fontHeading)
		
		lab1 <- tklabel(frame11,text="Altered Gene Set:                         ")
		
		tkgrid(lab0)
		tkgrid.configure(lab0,sticky="w")
		tkgrid(lab1,AgsEntry , Browse.but1 )
		tkgrid.configure(frame11,sticky="w")
		
		
		## Specify statistics ###
		
		rbstat1 <- tkradiobutton( frame15 )
		rbstat2 <- tkradiobutton( frame15 )
		
		rbValueStat <- tclVar("F")
		tkconfigure(rbstat1 ,variable=rbValueStat ,value="F",text="FNEA")
		tkconfigure(rbstat2 ,variable=rbValueStat ,value="M", text="MNEA")
		
		labstat <- tklabel(frame15,text="Network enrichment statistic:")
		
		tkgrid(labstat, rbstat1 , rbstat2 ,sticky="w")
		
		tkgrid.configure(frame15,sticky="w")
		
		#####
		
		### Specify fgs ####
		
		fgslist <- c("GO Cellular Component","GO Biological Process","GO Molecular Function","KEGG Pathway", "Reactome","User Defined")
		comboBox <- tkwidget(frame12,"ComboBox",editable=FALSE,values=fgslist,width="34")
		
		
		Browse.but2 <-tkbutton(frame12,text="Load FGS file",command=getFGS )
		
		
		tkgrid(tklabel(frame12,text="Specify Functional Gene Set:     "),comboBox,tklabel(frame12,text="          "))
		tkgrid.configure(Browse.but2, sticky="w")
		
		tkgrid.configure(frame12,sticky="w")
		
		### Annotation Data Base ###
		
		labDb <- tklabel(frame13,text="Annotation Database:                 ")
		labDb2 <- tklabel(frame13,text="Specify other annotation:")
		
		annolist <- c("GO annotation","KEGG annotation", "Reactome annotation",  "Other annotation")
		comboBoxAnn <- tkwidget(frame13,"ComboBox",editable=FALSE,values=annolist,width="34")
		
		tkgrid(labDb,comboBoxAnn,tklabel(frame13,text="          "),sticky="w")
		
		pkgDb <<- tclVar("")
		dbEntry <<-tkentry(frame13,width="35",textvariable=pkgDb )
		
		tkgrid(labDb2, dbEntry, sticky="w")
		tkgrid.configure(frame13,sticky="w")
		
		
		### Network LInks ###
		
		labNt <- tklabel(frame14,text="Network Links:                             ")
		Ntwk <<- tclVar("        ")
		load.but4 <-tkbutton(frame14,text="Browse",command=getNtw )
		ntEntry <-tkentry(frame14,width="35",textvariable=Ntwk )
		tkgrid(labNt, ntEntry, load.but4 )
		tkgrid.configure(frame14,sticky="w")
		
		tkgrid.configure(frame1,sticky="w")
		
		### Permutations ####
		labPerm <- tklabel(frame5,text="Permutation", font=fontHeading)
		
		rb1 <- tkradiobutton( frame51 )
		rb2 <- tkradiobutton( frame52 )
		
		rbValue1 <- tclVar("new")
		tkconfigure(rb1,variable=rbValue1,value="new",text="New permutations")
		tkconfigure(rb2,variable=rbValue1,value="old", text="Load previously saved permutations")
		
		tkgrid(labPerm ,sticky="w")
		
		tkgrid(rb1,sticky="w")
		
		permNum <- tclVar("100")
		entry.perm <-tkentry(frame51 ,width="10",textvariable=permNum)
		tkgrid.configure(tklabel(frame51 ,text="Number of permutations:           "),entry.perm,sticky="w")
		
		Seednum <- tclVar("1234")
		entry.Seed <-tkentry(frame51 ,width="10",textvariable=Seednum)
		tkgrid.configure(tklabel(frame51 ,text="Random seed number:               "),entry.Seed, 
				tklabel(frame51 ,text="                                                                "),sticky="w")
		
		tkgrid(rb2,sticky="w")
		filepnet <<- tclVar("        ")
		Browse.butpnt <-tkbutton(frame53,text="  Load  ",command=getPnet )
		PnetEntry <-tkentry(frame53,width="35",textvariable=filepnet )
		labpnet <- tklabel(frame53,text="Specify the file: ")
		
		tkgrid(labpnet,PnetEntry , Browse.butpnt,sticky="w" )
		
		tkgrid.configure(frame51,sticky="w")
		tkgrid.configure(frame52,sticky="w")
		tkgrid.configure(frame53,sticky="w")
		
		tkgrid.configure(frame5,sticky="w")
		
		### save the result ##
		
		labSave <- tklabel(frame6,text="Save the result", font=fontHeading)
		labSave2 <- tklabel(frame6,text="Specify the file name: ")
		labSave3 <- tklabel(frame6,text="                                 ")
		
		SavefileName <- tclVar("nea result")
#SavefileName <<- tclVar("        ")
		
#Browse.but5 <-tkbutton(frame6,text="Browse",command=savingRwd )
		resEntry <-tkentry(frame6,width="35",textvariable=SavefileName )
		
		tkgrid(labSave, sticky="w")
		tkgrid(labSave2, resEntry,labSave3 ,sticky="w")
#tkgrid(labSave2, resEntry, Browse.but5,sticky="w")
		
		tkgrid.configure(frame6,sticky="w")
		
		
		
		#### OK and Exit Button ###
		
		OnOK <- function()
		{
			
			rbVal1 <- as.character(tclvalue(rbValue1))
			rbValstat <- as.character(tclvalue(rbValueStat ))
			
			fgslist2 <- c("CC","BP","MF","KEGG", "Reactome", "UsrDfn")
			
			
			fgsChoice <- fgslist2[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
			
			dbChoice <- annolist[as.numeric(tclvalue(tcl(comboBoxAnn,"getvalue")))+1]
			
			Seed <- as.numeric(tclvalue(Seednum))
			nperm <<- as.numeric(tclvalue(permNum))
			
			if (checkObject ("Seed", del=F)==F) Seed =1234
			if (checkObject ("nperm", del=F)==F) nperm = 100
			
			
			Missing <- NULL
			if (checkObject ("AGS", del=F)==F) Missing <- c(Missing ,"AGS")
			if (length(fgsChoice)==0 )   Missing <- c(Missing ,"FGS")
			if (checkObject ("NETWORK", del=F)==F) Missing <- c(Missing ,"NETWORK")
			if (length(dbChoice)==0 )  Missing <- c(Missing ,"ANNOTATION DB")
			
			misObjt <- paste(Missing, collapse = ", ") 
			
			
			if (is.null (Missing )==F)
				tkmessageBox(title="Errors have occurred!", message= paste("The Following inputs have not been specified correctly: ", misObjt , " !!! Please input them to perform the analysis")
						,icon="error",type="ok")
			
			if (is.null (Missing ))
			
			{
				
				
				tkconfigure(tt,cursor="watch")
				print (paste("The analysis is started at ", date() ))
				
				
				
				
				if (fgsChoice=="UsrDfn") 
				
				{
					tkmessageBox(title="Warning",message="You define your own Functional Gene Set !",icon="warning",type="ok")
					
				}
				else {
					FGS <<- fgsChoice
				}
				
				## get the annotation database ##
				
				if(dbChoice == "KEGG annotation" ) dbInput <- "KEGG.db"
				if(dbChoice == "GO annotation" ) dbInput <- "GO.db"
				if(dbChoice == "Reactome annotation" ) dbInput <- "reactome.db"
				
				if(dbChoice == "Other annotation" )
				{
					dbInput <- tclvalue(pkgDb)
					if (dbInput == "") dbInput <- NULL
					else {
						getDb(dbInput)
					}
				}
				
				FGS <- get("FGS", envir = .GlobalEnv )
				AGS <- get("AGS", envir = .GlobalEnv )
				NETWORK <- get("NETWORK", envir = .GlobalEnv )
				AnnotationDB <<- dbInput
				
				FGS <- toupper(FGS)
				AGS <- toupper(AGS)
				NETWORK <- toupper(NETWORK)
				
				## result file name ##
				
				resName <- tclvalue(SavefileName)
				if (!nchar(resName)) {
					tclvalue(SavefileName) <<- c("")
					tkmessageBox(message="No file name for the result")
					tkfocus(tt)
				}
				
				else {
					filename <- savingRes (resName )
					
					## checking if KEGG and GO are installed ###
					if (FGS == "CC"| FGS == "MF"| FGS == "BP" |FGS == "KEGG" ) checkFgs ("KEGG")
					
					if (rbVal1=="new")  pnet <- NULL
					
					if (rbVal1=="old") {
						pexist <- tryCatch(pnet <- get("Pnet", envir = .GlobalEnv ), error = function(e) NULL)
						
						if (is.null(pexist)) pnet <- NULL
					}
					
					#### Main Analysis ####
					
					stat <<- rbValstat
					
					res <-neaMod(ags=AGS, fgs = FGS, fgslib  = dbInput, network=NETWORK, nperm = nperm , seed = Seed, pnet=pnet, stat=stat )
					
					pb <- tkProgressBar(title = "Analysis progress bar", "Analysis is being finalized, please wait...", width = 300)
					
					if (length(res$nlink)<1 ) {
						tkmessageBox(message = "No result, please check your inputs!!!")
						
					}
					
					else {
						
						if (FGS == "KEGG")  { 
							
							getcode <- function (y) {
								ff <- substr(y,1,3)
								if( is.na(as.numeric(ff) )== T )  code <- substr(y,4,nchar(y) )
								else { code <- y
								}
							}
							
							
							if (stat=="F") {
								getPathID <- function (res) {
									PathID <- names(res$nlink)
									combres <- data.frame(Number_links= res$nlink,  Expected_links = res$exp.link, 
											Number_of_Genes=res$ngenefgs ,Number_of_AGS_genes=res$numgene, Z_score=res$zscore, P_value=res$pvalue,  FDR= res$FDR)
									options(warn=-1)
									PathID1 <- sapply(PathID , function (x) getcode(x)  )
									options(warn=1)
									combres <-   data.frame(PATH_ID = PathID1, combres, PathID  )
									pathId <- AnnotationDbi::as.list(KEGGPATHID2NAME)
									KEGGPath <- data.frame(PATH_ID=names(pathId ),PATH_NAME= unlist(pathId ))
									pathIDres <- data.frame(KEGGPath[which(KEGGPath[,1]  %in% PathID1 ),])
									finalres <- merge(pathIDres ,combres )
									finalres[,1]  <- finalres [,10]
									finalres <- finalres[,-10]
								}
								
								
								ResultNEAxls <- getPathID (res )
							}
							else {
								
								### get KEGG PathID for stat= 'M' ##
								
								PathID <- rownames(res$nlink)
								options(warn=-1)
								PathID1 <- sapply(PathID , function (x) getcode(x)  )
								options(warn=1)
								pathId <- AnnotationDbi::as.list(KEGGPATHID2NAME)
								KEGGPath <- data.frame(PATH_ID=names(pathId ),PATH_NAME= unlist(pathId ))
								pathIDres <- data.frame(KEGGPath[which(KEGGPath[,1]  %in% PathID1 ),])
								nlink.M <-   data.frame(PATH_ID = PathID1, PathID  ,res$nlink)
								nlink.M <- merge(pathIDres ,nlink.M)
								nlink.M <- nlink.M[,-1]
								Z.score <- data.frame(PATH_ID = PathID1,PathID  , res$zscore.mat)
								Z.score  <- merge(pathIDres ,Z.score )
								Z.score <- Z.score[,-1]
							}
							
						}
						
						if (FGS == "CC"| FGS == "MF"| FGS == "BP") {
							
							GoTerm <- function (x) { 
								term1 <- GOTERM[[x]]
								GoInput <- cbind ( GOID= GOID(term1 ) ,  TERM= Term(term1 ), Definition= Definition(term1 ),
										Ontology= Ontology(term1 ))
								return(GoInput )
							}
							
							if (stat=="F") {
								goID <-names(res[[1]])
								resGO <- t(sapply(goID ,GoTerm ))
								colnames(resGO) <- c("GOID", "Term", "Definition", "Ontology")
								
								ResultNEAxls <- data.frame(resGO ,Number_links= res$nlink, Expected_links = res$exp.link,
										Number_of_Genes=res$ngenefgs ,Number_of_AGS_genes=res$numgene, Z_score=res$zscore, P_value=res$pvalue,  FDR= res$FDR)
							}
							
							else {
								
								### get GO PathID for stat= 'M' ##
								goID <-rownames(res[[1]])
								resGO <- t(sapply(goID ,GoTerm ))
								colnames(resGO) <- c("GOID", "Term", "Definition", "Ontology")
								nlink.M <- data.frame(resGO ,res$nlink)
								Z.score <- data.frame(resGO , res$zscore.mat)
							}
							
						}
						
						
						if (FGS == "REACTOME") {
							
							if (stat=="F") {
								pathres <- names(res$nlink)
								pathId <- AnnotationDbi::as.list(reactomePATHID2NAME)
								pathIdres <- pathId [as.character(pathres) ]
								
								x <- lapply(pathIdres , FUN="[", 1)
								y <- lapply(x, FUN=unlist)
								ReactomePath <- data.frame(PATH_ID=names(y ),PATH_Desc= unlist(y))
								
								ResultNEAxls <- data.frame(ReactomePath , Number_links= res$nlink,  Expected_links = res$exp.link, 
										Number_of_Genes=res$ngenefgs ,Number_of_AGS_genes=res$numgene, Z_score=res$zscore, P_value=res$pvalue,  FDR= res$FDR)
								
							}
							else {
								
								### get Reactome PathID for stat= 'M' ##
								pathres <- rownames(res$nlink)
								pathId <- AnnotationDbi::as.list(reactomePATHID2NAME)
								pathIdres <- pathId [as.character(pathres) ]
								x <- lapply(pathIdres , FUN="[", 1)
								y <- lapply(x, FUN=unlist)
								ReactomePath <- data.frame(PATH_ID=names(y ),PATH_Desc= unlist(y))
								nlink.M <- data.frame(ReactomePath ,res$nlink)
								Z.score <- data.frame(ReactomePath , res$zscore.mat)
								
							}
							
							
							
							
						}
						
						if (fgsChoice=="UsrDfn") {
							
							ResultNEAxls <- data.frame(PathID=  names(res$nlink),Number_links= res$nlink, Expected_links = res$exp.link,
									Number_of_Genes=res$ngenefgs ,Number_of_AGS_genes=res$numgene, Z_score=res$zscore, P_value=res$pvalue,  FDR= res$FDR)
							
						}
						
						permutedNetwork <- res$pnetout 
						if (is.null(pnet) == F) permutedNetwork <- pnet
						network.link.num <- res$res.nlink
						
						
						if (stat=="F") {
							ResultNEA <- ResultNEAxls[order(abs(ResultNEAxls$Z_score), decreasing = T),]
							FGS.genes.list <- res$fgs.list
							Ags.In.Fgs <- res$geneinfgs
							
							save(list=c("ResultNEA","permutedNetwork","AGS","FGS","NETWORK","AnnotationDB","network.link.num", "FGS.genes.list","Ags.In.Fgs"),file =filename [1])
							
							#save(list=c("ResultNEA","permutedNetwork","AGS","FGS","NETWORK","AnnotationDB","network.link.num"),file =filename [1])
							
							write.csv2(ResultNEAxls , file=filename[2], row.names =F, quote = F)
							
							
							print (paste("The analysis is finished at ", date() ))
							print (paste("Results are saved in ", filename [1], " and ", filename[2]))
							close(pb)
							fl <- filename[3]
							saveHtml(ResultNEA,fl)
							
						}
						
						else {
							save(list=c("nlink.M","Z.score","permutedNetwork","network.link.num", "AGS","FGS","NETWORK","AnnotationDB"),file =filename [1])
							print (paste("The analysis is finished at ", date() ))
							print (paste("Results are saved in ", filename [1]))
							close(pb)
							
							
						}
						
						
						ReturnVal <- tkmessageBox(title="The analysis has been done!",message="The analysis has been done! The result has been saved",
								icon="info",type="ok")
						
						
						
						
						rm(list=c("fgsChoice","dbChoice"))
						
						tkconfigure(tt,cursor="arrow")
					}
				}
			}
		}
		
		
		
		
		onCancel <- function()
		{
			ReturnVal <<- 0
			
			## remove output from previous analysis ###
			checkObject ("AGS")
			checkObject ("FGS")
			checkObject ("NETWORK")
			checkObject ("AnnotationDB")
			checkObject ("Pnet")
			checkObject ("stat")
			checkObject ("nperm")
			checkObject ("res")
			
			tkgrab.release(tt )
			tkdestroy(tt )
		}
		
		onHelp <- function () 
		{
			tkgrab.release(window)
			helpIndex <- file.path(system.file(package = "neaGUI"), 
					"index.html")
			browseURL(helpIndex)
		}
		
		
		OK.but <-tkbutton(buttonFrame ,text="   Run NEA   ",command=OnOK)
		Cancel.but <-tkbutton(buttonFrame ,text="   Exit   ",command=onCancel)
		Help.but <-tkbutton(buttonFrame ,text="  Help  ",command=onHelp)
		tkgrid(OK.but, Cancel.but, Help.but)
		
		tkgrid(spec.frm)
		tkgrid(buttonFrame)
		
		tkwait.window(tt)
		
	}
