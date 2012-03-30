checkFgs <-
function (db) {


#checkDb <- tryCatch(library(dbInput),error = function(e) e=FALSE)

if (db == "CC"| db == "MF"| db == "BP" ) dbName <- "GO.db"
if (db == "KEGG") dbName <- "KEGG.db"

is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 

if (is.installed(dbName)  == FALSE) {

if  (db == "CC") pthname <- "cellular component"
if  (db == "BP") pthname <- "biological process" 
 if  (db == "MF") pthname <- "molecular function"
if  (db == "KEGG") pthname <- "KEGG pathway"


tkmessageBox(title="Warning",message= 
paste (pthname, " database doesn't exist!! To install it you must be connected to internet!", sep=""),
icon="warning",type="ok")

if (db == "CC"| db == "MF"| db == "BP" ) {
    source("http://www.bioconductor.org/biocLite.R")
biocLite("GO.db")
}
if (db == "KEGG") {
source("http://www.bioconductor.org/biocLite.R")
biocLite("KEGG.db")
biocLite("AnnotationDbi")

}

}
}

