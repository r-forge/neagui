getNtw <-
function() {
      
initdir <- get("initdir", envir = .GlobalEnv )
fileName <- tclvalue(tkgetOpenFile(filetypes=
gettext('{"text" {".txt"}} {"Comma delimited" {".csv"}}
{"All Files" {"*"}}'),initialdir = initdir))
tkconfigure(tt ,cursor="watch")

if (!nchar(fileName)) 
{
tkmessageBox(message = "No file was selected!")
} 
else 
{
splitted <- strsplit(fileName,".",fixed=TRUE)

splitted2 <- unlist(splitted)
filetype  <<- splitted2 [length(splitted2 )]


if(filetype   == "txt")
{
 tclvalue(Ntwk) <<- fileName
 network <- scan(file=fileName, sep=",", what=character())
 assign("NETWORK",network,envir=.GlobalEnv)
}

if(filetype   == "csv")
{
tclvalue(Ntwk) <<- fileName
network <- as.character(unlist(read.csv(fileName, header =F)))
assign("NETWORK",network,envir=.GlobalEnv)
}
####
if (filetype   == "RData" | filetype   == "Rdata")
{

LoadRdata(fileName,"NETWORK")
  tclvalue(Ntwk) <<- paste(inptObjt, "->", fileName)
	checkObject ("inptObjt")

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

