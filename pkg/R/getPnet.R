getPnet <-
function () {
initdir <- get("initdir", envir = .GlobalEnv )

filename <- tclvalue(tkgetOpenFile(filetypes = gettext("{\"R Data Files\" {\".rda\" \".Rdata\" \".RDA\"}} {\"All Files\" {\"*\"}}"), 
initialdir = initdir))
      
tkconfigure(tt ,cursor="watch")
      tclvalue(filepnet) <<- filename

#dataSet <- load(filename, envir = .tempEnv )
LoadRdata(filename,"Pnet")
      tkconfigure(tt ,cursor="arrow")


}

