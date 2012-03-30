savingRes <-
function(filename) {
initdir <- get("initdir", envir = .GlobalEnv )
fileRwd <- paste(initdir,"/",filename,".Rdata",sep="")
filecsv <- paste(initdir,"/",filename,".csv",sep="")
return(c(fileRwd ,filecsv ))

}

