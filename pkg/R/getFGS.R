getFGS <-
function () {
initdir <- get("initdir", envir = .GlobalEnv )
filename <- tclvalue(tkgetOpenFile(filetypes = gettext("{\"R Data Files\" {\".rda\" \".Rdata\" \".RDA\"}} {\"All Files\" {\"*\"}}"), 
initialdir = initdir))

tkconfigure(tt ,cursor="watch")
      #dataSet <- load(filename, envir = .tempEnv)
LoadRdata(filename,"FGS")
      tkconfigure(tt ,cursor="arrow")

}

