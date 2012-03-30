getNtw <-
function() {
      
initdir <- get("initdir", envir = .GlobalEnv )
fileName <- tclvalue(tkgetOpenFile(filetypes=
gettext('{"text" {".txt"}} {"Excel Files" {".xls"}}
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
else
{ 
if(unlist(splitted)[2] == "xls")
{
network <- unlist(read.xls(fileName))
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
else {
tkmessageBox(message = "The files is not txt, excel, or RData file")
}
}
}
    tkconfigure(tt ,cursor="arrow")

}

