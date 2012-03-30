getDb <-
function (dbInput) {
      
      tkconfigure(tt ,cursor="watch")
#checkDb <- tryCatch(library(dbInput),error = function(e) e=FALSE)

is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 

if (is.installed(dbInput)  == FALSE) {
tkmessageBox(title="Warning",message="The specified DB doesn't exist!! To install it you must be connected to internet!",icon="warning",type="ok")
setRepositories()
    install.packages(dbInput)
     }
}

