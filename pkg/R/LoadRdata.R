LoadRdata <-
function (fileName,objt) {
.tempEnv <- new.env()
dataSet <- load(fileName, envir = .tempEnv)

ttload <- tktoplevel() 
buttonFrame <-tkframe(ttload,borderwidth=2)
tkwm.deiconify(ttload )
tkwm.title(ttload , paste("Load ", objt))
tkgrab.set(ttload )
tkfocus(ttload )

tkgrid(tklabel(ttload ,text=paste("Choose the ", objt, " object") ))

spec.frm <- tkframe(ttload , borderwidth = 2)
frame1 <- tkframe(spec.frm, relief = "groove", borderwidth = 2)
buttonFrameiso <- tkframe(ttload , borderwidth = 2)
scr <- tkscrollbar(spec.frm, repeatinterval = 2, command = function(...) tkyview(tl,...))
tl <- tklistbox(spec.frm, height = 10, selectmode = "single", 
yscrollcommand = function(...) tkset(scr, ...), background = "white", width=30)
for (i in (1:length(dataSet))) {
tkinsert(tl, "end", dataSet[i])
}
tkselection.set(tl, 0)

tkgrid(frame1, tl, scr)
tkgrid.configure(tl, sticky = "nse")
tkgrid.configure(scr, rowspan = 10, sticky = "nsw")

tkgrid(spec.frm)

OnOK <- function()
{
tkconfigure(tt ,cursor="watch")
      options(warn=-1)
      # indx <- as.integer(tkcurselection(tl))
      # .tempEnv <- new.env()
#dataSet <- load(fileName, envir = .tempEnv)
name <- dataSet [as.numeric(tkcurselection(tl)) + 1]
inpt <- get(name, envir = .tempEnv )
assign(paste(objt), inpt, envir = .GlobalEnv)
      if(exists("dataSet")) rm(dataSet )
      if(exists(".tempEnv")) rm(.tempEnv)
tkdestroy(ttload )
      options(warn=1)
assign("inptObjt", name, envir = .GlobalEnv)
#msg <- paste("You choose ",name ," as the ", objt, " input! ",sep="")
#tkmessageBox(title=paste(objt, "file input  "),message=msg)
tkconfigure(tt ,cursor="arrow")
      tkfocus(tt)

}

onCancel <- function()
{
ReturnVal <- 0
tkgrab.release(ttload )
tkdestroy(ttload )
tkfocus(tt)
}
OK.but <-tkbutton(buttonFrame,text="   OK   ",command=OnOK)

Cancel.but <-tkbutton(buttonFrame ,text=" Cancel ",command=onCancel)

tkgrid(OK.but, Cancel.but)
tkgrid(buttonFrame)
tkfocus(ttload)
tkwait.window(ttload )
tkconfigure(tt ,cursor="arrow")
}

