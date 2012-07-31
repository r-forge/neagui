getNtw <-
function() {
      
initdir <- get("initdir", envir = .GlobalEnv )
fileName <- tclvalue(tkgetOpenFile(filetypes=
gettext('{"text" {".txt"}} {"Comma delimited" {".csv"}}
{"All Files" {"*"}}'),initialdir = initdir))
tkconfigure(tt ,cursor="watch")
      tclvalue(Ntwk) <<- fileName

if (!nchar(fileName)) 
{
tkmessageBox(message = "No file was selected!")
} 
else 
{
splitted <- strsplit(fileName,".",fixed=TRUE)
if(unlist(splitted)[2] == "txt")
{
 network <- scan(file=fileName, sep=",", what=character())
 assign("NETWORK",network,envir=.GlobalEnv)
}

if(unlist(splitted)[2] == "csv")
{
#network <- unlist(read.xls(fileName))
network <- as.character(unlist(read.csv(fileName, header =F)))


assign("NETWORK",network,envir=.GlobalEnv)
}
####
if (unlist(splitted)[2] == "RData" | unlist(splitted)[2] == "Rdata")
{
#.tempEnv <- new.env()
#dataSet <- load(fileName, envir = .tempEnv)
LoadRdata(fileName,"NETWORK")
#rm(.tempEnv)
}
#######
if (filetype %in% c( "RData", "Rdata","txt", "csv") ==F)
 {
tkmessageBox(message = "The files is not txt, csv, or RData file")
      tclvalue(Ntwk) <<- ""


}
}
    tkconfigure(tt ,cursor="arrow")

}

