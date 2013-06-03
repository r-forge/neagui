
library(neaGUI)

test.countcon <- function() {
	netwrk <-c("DNAJC6 RGL1","C1ORF156 NCBP2","AHCYL1 RTN3","PLK4 SKIV2L2","C22ORF28 MESDC2")
 	checkTrue(is.numeric(neaGUI:::countcon(netwrk)$ncon))
 	checkEquals(length(neaGUI:::countcon(netwrk)$ncon ) , 10  )
 	checkEquals(neaGUI:::countcon(netwrk)$genes  , c("AHCYL1","C1ORF156","C22ORF28","DNAJC6",
		"MESDC2","NCBP2","PLK4", "RGL1", "RTN3", "SKIV2L2")  )
 	checkEqualsNumeric(neaGUI:::countcon(netwrk)$ncon, rep(1,10) )
 }




test.arr2list <- function() {
	netwrk <-c("DNAJC6 RGL1","C1ORF156 NCBP2","AHCYL1 RTN3","PLK4 SKIV2L2","C22ORF28 MESDC2")
      genes <- neaGUI:::countcon(netwrk)$genes 
 	checkTrue(is.numeric(neaGUI:::arr2list(netwrk , gsym = genes)$list1))
 	checkEquals(length(neaGUI:::arr2list(netwrk , gsym = genes)$list1 ) , 5  )
 	checkEqualsNumeric(neaGUI:::arr2list(netwrk , gsym = genes)$list1, c(9,6,5,8,10) )

}

test.arr2list <- function() {
	ags <- c("A","B","C","D","E")
	fgs <- list(FGS1=c("B","E","F","G","H"),FGS2=c("F","C"))
	netwrk <-c("A B","A F","E C","Q R","S T","U V")
      genes <- neaGUI:::countcon(netwrk)$genes 
	netlist2 <- neaGUI:::arr2list(netwrk, gsym = genes)
	nlink <- neaGUI:::linknum.arr(ags, fgs = fgs, netlist2, gnum = TRUE,  gsym = genes)
	checkTrue(is.numeric(nlink ))
 	checkEquals(length(nlink  ) , length(fgs)  )
 	checkEqualsNumeric(nlink , c(3,2) )

}

test.netperm <- function() {
	ags <- c("A","B","C","D","E")
	fgs <- list(FGS1=c("B","E","F","G","H"),FGS2=c("F","C"))
	netwrk <-c("A B","A F","E C","Q R","S T","U V")
      genes <- neaGUI:::countcon(netwrk)$genes 

	perm.ncon <- neaGUI:::netperm(neaGUI:::countcon(netwrk)$ncon)
	pos1 <- perm.ncon$pos1

	checkTrue(is.list(pos1 ))
 	checkEquals(length(pos1 ) , length(ags)  )
 	checkEqualsNumeric(pos1[[1]] , c(6,5) )

}



test.netperm <- function() {
	ags <- c("A","B","C","D","E")
	fgs <- list(FGS1=c("B","E","F","G","H"),FGS2=c("F","C"))
	netwrk <-c("A B","A F","E C","Q R","S T","U V")
      genes <- neaGUI:::countcon(netwrk)$genes 
	perm.ncon <- neaGUI:::netperm(neaGUI:::countcon(netwrk)$ncon)
	pos1 <- perm.ncon$pos1

      res.nlink <- neaGUI:::linknum.list(ags, fgs = fgs, 
					netlist = pos1, gnum = TRUE, gsym = genes)	
	checkTrue(is.numeric(res.nlink ))
 	checkEquals(length(res.nlink ) , length(fgs)  )
}



