getAgs <-
function() {
#initdir <- get("initdir", envir = .GlobalEnv )
initdir <- system.file(package="neaGUI", "extdata")    
fileName <- tclvalue(tkgetOpenFile(filetypes=
	gettext('{"text" {".txt"}} {"Comma delimited" {".csv"}}
	{"All Files" {"*"}}'), initialdir =initdir))

	tkconfigure(tt ,cursor="watch")
      initdir <- dirname(fileName)
	assign("initdir",initdir, envir = .GlobalEnv)

#tclvalue(fileAgs) <<- fileName
if (!nchar(fileName)) 
{
tkmessageBox(message = "No file was selected!")
} 
else 
{
splitted <- strsplit(fileName,".",fixed=TRUE)

splitted2 <- unlist(splitted)
            filetype  <<- splitted2 [length(splitted2 )]

if(filetype  == "txt")
{
tclvalue(fileAgs) <<- fileName
ags <- scan(file=fileName, sep=",", what=character())
assign("AGS",ags,envir=.GlobalEnv)
}
 
if(filetype   == "csv")
{
tclvalue(fileAgs) <<- fileName
#ags <- unlist(read.xls(fileName)
ags <- as.character(unlist(read.csv(fileName, header =F)))

assign("AGS",ags,envir=.GlobalEnv)
}

if (filetype  == "RData" | filetype    == "Rdata")
{
      LoadRdata(fileName,"AGS")
      tclvalue(fileAgs) <<- paste(inptObjt, "->", fileName)
	checkObject ("inptObjt")
    }

if (filetype %in% c( "RData", "Rdata","txt", "csv") ==F)
{
tkmessageBox(message = "The files is not txt, csv, or RData file")
tclvalue(fileAgs) <<- ""

 }
}

  tkconfigure(tt ,cursor="arrow")

}

