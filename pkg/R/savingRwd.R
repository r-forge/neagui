savingRwd <-
function() {
initdir <- get("initdir", envir = .GlobalEnv )

fileName <- tclvalue(tkgetSaveFile(filetypes="{{R images} {*.RData}} {{All files} *}",initialdir = initdir ))
split <- strsplit(fileName,".",fixed=TRUE)
fileRwd <- paste(split[[1]][1],".Rdata",sep="")
filecsv <- paste(split[[1]][1],".csv",sep="")

assign("fileRwd",fileRwd,envir=.GlobalEnv)
assign("filecsv",filecsv,envir=.GlobalEnv)

tclvalue(SavefileName) <<- fileRwd 

if (!nchar(SavefileName)) {
tclvalue(SavefileName) <<- c("")
tkmessageBox(message="No file was selected!")
}
}

