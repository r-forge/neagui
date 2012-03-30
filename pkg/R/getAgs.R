getAgs <-
function() {
#initdir <- get("initdir", envir = .GlobalEnv )
     
fileName <- tclvalue(tkgetOpenFile(filetypes=
gettext('{"text" {".txt"}} {"Excel Files" {".xls"}}
{"All Files" {"*"}}')))

tkconfigure(tt ,cursor="watch")
      initdir <- dirname(fileName)
assign("initdir",initdir, envir = .GlobalEnv)

tclvalue(fileAgs) <<- fileName
if (!nchar(fileName)) 
{
tkmessageBox(message = "No file was selected!")
} 
else 
{
splitted <- strsplit(fileName,".",fixed=TRUE)

splitted2 <- unlist(splitted)
            filetype  <- splitted2 [length(splitted2 )]
if(filetype  == "txt")
{
ags <- scan(file=fileName, sep=",", what=character())
assign("AGS",ags,envir=.GlobalEnv)
}
else
{ 
if(filetype   == "xls")
{
ags <- unlist(read.xls(fileName))
assign("AGS",ags,envir=.GlobalEnv)
}

            if (filetype  == "RData" | filetype    == "Rdata")
{
    #.tempEnv <- new.env()
    #dataSet <- load(fileName, envir = .tempEnv)
      LoadRdata(fileName,"AGS")
                      #rm(.tempEnv)
    }

else {
tkmessageBox(message = "The files is not txt, excel, or RData file")
}
}
}
  tkconfigure(tt ,cursor="arrow")

}

