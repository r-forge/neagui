neaGUI <-
function() {

require(tcltk)
require(nea)
library(AnnotationDbi)
require(KEGG.db)
require(GO.db)
require(org.Hs.eg.db)
      require(hwriter)
tclRequire("BWidget")
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

fgslist <- c("GO Cellular Component","GO Biological Process","GO Molecular Function","KEGG Pathway", "User Defined")
comboBox <- tkwidget(frame12,"ComboBox",editable=FALSE,values=fgslist,width="34")


Browse.but2 <-tkbutton(frame12,text="Load FGS file",command=getFGS )


tkgrid(tklabel(frame12,text="Specify Functional Gene Set:     "),comboBox,tklabel(frame12,text="          "))
tkgrid.configure(Browse.but2, sticky="w")

tkgrid.configure(frame12,sticky="w")

### Annotation Data Base ###

labDb <- tklabel(frame13,text="Annotation Database:                 ")
labDb2 <- tklabel(frame13,text="Specify other annotation:")

annolist <- c("GO annotation","KEGG annotation", "Other annotation")
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

permNum <- tclVar("50")
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

## 
Seed <- 1234
Perm <- 50


#### OK and Exit Button ###

OnOK <- function()
{

tkconfigure(tt,cursor="watch")
print (paste("The analysis is started at ", date() ))

rbVal1 <- as.character(tclvalue(rbValue1))
rbValstat <- as.character(tclvalue(rbValueStat ))

fgslist2 <- c("CC","BP","MF","KEGG", "UsrDfn")


fgsChoice <- fgslist2[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]

if (fgsChoice=="UsrDfn") 

{
tkmessageBox(title="Warning",message="You define your own Functional Gene Set !",icon="warning",type="ok")

}
else {
FGS <<- fgsChoice
}

## get the annotation database ##
dbChoice <- annolist[as.numeric(tclvalue(tcl(comboBoxAnn,"getvalue")))+1]
#dbInput <- dbChoice


             if(dbChoice == "KEGG annotation" ) dbInput <- "KEGG.db"
             if(dbChoice == "GO annotation" ) dbInput <- "GO.db"


if(dbChoice == "Other annotation" )
{
dbInput <- tclvalue(pkgDb)
if (dbInput == "") dbInput <- NULL
else {
getDb(dbInput)
}
}

Seed <- as.numeric(tclvalue(Seednum))
Perm <- as.numeric(tclvalue(permNum))

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
                        nperm <<- Perm 

res <-neaMod(ags=AGS, fgs = FGS, fgslib  = dbInput, network=NETWORK, nperm = nperm , seed = Seed, pnet=pnet, stat=stat )

                        pb <- tkProgressBar(title = "Analysis progress bar", "Analysis is being finalized, please wait...", width = 300)

if (FGS == "KEGG")  { 


getPathID <- function (res) {

PathID <- names(res$nlink)
combres <- data.frame(Number_links= res$nlink,  Expected_links = res$exp.link, 
          Number_of_Genes=res$ngenefgs ,Number_of_AGS_genes=res$numgene, Z_score=res$zscore, P_value=res$pvalue,  FDR= res$FDR)
getcode <- function (y) {
ff <- substr(y,1,3)
if( is.na(as.numeric(ff) )== T )  code <- substr(y,4,nchar(y) )
else { code <- y
  }
}
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

#ResultNEAxls <- data.frame(KEGGPath[which(KEGGPath[,1]  %in% PathID ),],Number_Link= res$nlink, Expected_link = res$exp.link,
#Number_of_Genes=res$ngenefgs ,Number_of_AGSGenes=res$numgene,  ZScore=res$zscore, Pvalue=res$pvalue,  FDR= res$FDR)


}

if (FGS == "CC"| FGS == "MF"| FGS == "BP") {

GoTerm <- function (x) { 
term1 <- GOTERM[[x]]
GoInput <- cbind ( GOID= GOID(term1 ) ,  TERM= Term(term1 ), Definition= Definition(term1 ),
Ontology= Ontology(term1 ))
return(GoInput )
}

goID <-names(res[[1]])
resGO <- t(sapply(goID ,GoTerm ))
                              colnames(resGO) <- c("GOID", "Term", "Definition", "Ontology")

ResultNEAxls <- data.frame(resGO ,Number_links= res$nlink, Expected_links = res$exp.link,
Number_of_Genes=res$ngenefgs ,Number_of_AGS_genes=res$numgene, Z_score=res$zscore, P_value=res$pvalue,  FDR= res$FDR)

      }


permutedNetwork <- res$pnetout 
if (is.null(pnet) == F) permutedNetwork <- pnet

#ResultNEA <- data.frame(Network.links= res$nlink, Expected.link = res$exp.link, Z.score=res$zscore,p.value=res$pvalue,FDR=res$FDR)
ResultNEA <- ResultNEAxls 

network.link.num <- res$res.nlink
FGS.genes.list <- res$fgs.list
Ags.In.Fgs <- res$geneinfgs

save(list=c("ResultNEA","permutedNetwork","AGS","FGS","NETWORK","AnnotationDB","network.link.num", "FGS.genes.list","Ags.In.Fgs"),file =filename [1])

#save(list=c("ResultNEA","permutedNetwork","AGS","FGS","NETWORK","AnnotationDB","network.link.num"),file =filename [1])

write.csv2(ResultNEAxls , file=filename[2], row.names =F, quote = F)

print (paste("The analysis is finished at ", date() ))
print (paste("Results are saved in ", filename [1], " and ", filename[2]))
                        close(pb)
ReturnVal <- tkmessageBox(title="The analysis has been done!",message="The analysis has been done! The result has been saved",
icon="info",type="ok")
fl <- filename[3]
saveHtml(ResultNEA,fl)

tkconfigure(tt,cursor="arrow")
#tkdestroy(tt)
}
}




onCancel <- function()
{
ReturnVal <<- 0

 if(exists("AGS", envir=.GlobalEnv) ) remove(AGS, envir=.GlobalEnv)
     if(exists("FGS", envir=.GlobalEnv)) remove(FGS, envir=.GlobalEnv)
                  if(exists("NETWORK", envir=.GlobalEnv)) remove(NETWORK, envir=.GlobalEnv)
     if(exists("Pnet", envir=.GlobalEnv)) remove(Pnet, envir=.GlobalEnv)
     if(exists("AnnotationDB", envir=.GlobalEnv)) remove(AnnotationDB, envir=.GlobalEnv)
     if(exists("stat", envir=.GlobalEnv)) remove(stat, envir=.GlobalEnv)
     if(exists("nperm", envir=.GlobalEnv)) remove(nperm, envir=.GlobalEnv)

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

