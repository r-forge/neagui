saveHtml <-
function (ResultNEA,fl) {
      linkID  <- NULL

#if (FGS=="reactome.db") ResultNEAsrt  <- ResultNEA 
if (fgsChoice=="UsrDfn") ResultNEAsrt  <- ResultNEA 
else {
if (FGS=="KEGG" & AnnotationDB== "KEGG.db")  linkID <- paste("http://www.genome.jp/dbget-bin/www_bget?pathway:",ResultNEA$PATH_ID,sep="")
if ( FGS%in%c("CC","BP","MF") & AnnotationDB== "GO.db")  linkID <- paste("http://amigo.geneontology.org/cgi-bin/amigo/term_details?term=",ResultNEA$GOID,sep="")
if (FGS=="reactome.db") linkID <- paste("http://www.reactome.org/cgi-bin/eventbrowser?DB=gk_current&ID=",ResultNEA$ReactomePath, sep="") 

ResultNEAsrt <- ResultNEA[order(ResultNEA$P_value),]
linkIDsrt <- linkID  [order(ResultNEA$P_value)]
}



ResultNEAsrt$Expected_links <- round(ResultNEAsrt$Expected_links ,4)
ResultNEAsrt$Z_score <- round(ResultNEAsrt$Z_score ,6)
ResultNEAsrt$P_value <- round(ResultNEAsrt$P_value ,6)
ResultNEAsrt$FDR <- round(ResultNEAsrt$FDR ,6)

p=openPage(fl)
hwrite('The result of neaGUI', p, br=TRUE,heading=2)
hwrite(paste('AGS: length:',length(AGS)), p, br=TRUE)
hwrite(paste('FGS:', FGS), p, br=TRUE)
hwrite(paste('Annotation:',AnnotationDB), p, br=TRUE)
st <- ifelse(stat=="F", "FNEA", "MNEA")
hwrite(paste('Statistics: ',st), p, br=TRUE)

hwrite(paste('Number of network links:', length(NETWORK) ), p, br=TRUE)
hwrite(paste('Number of Permutations:',nperm), p, br=TRUE)
hwrite('', p, br=TRUE)
hwrite('', p, br=TRUE)

algn <- list(Number_links='text-align:center',Expected_links='text-align:center',Number_of_Genes='text-align:center',
  Number_of_AGS_genes='text-align:center',Z_score='text-align:center', P_value='text-align:center',FDR ='text-align:center')


if (FGS=="KEGG" & AnnotationDB== "KEGG.db") {
hwrite(ResultNEAsrt , p, center=TRUE, row.bgcolor='99CCFF',col.link=list(PATH_ID=linkIDsrt ),col.style= algn ,
 cellspacing=0,table.class='raw', col.width=c(PATH_ID='75px', P_value='100px', Z_score='100px', FDR ='100px'),row.names=FALSE)
}


if (AnnotationDB== "GO.db") {
hwrite(ResultNEAsrt , p, center=TRUE, row.bgcolor='99CCFF',col.link=list(GOID=linkIDsrt ),col.style= algn ,
 cellspacing=0,table.class='raw', col.width=c(GOID='75px', P_value='100px', Z_score='100px', FDR ='100px'),row.names=FALSE)

}

else {
hwrite(ResultNEAsrt , p, center=TRUE, row.bgcolor='99CCFF',col.style= algn ,
 cellspacing=0,table.class='raw', col.width=c( P_value='100px', Z_score='100px', FDR ='100px'),row.names=FALSE)

}


closePage(p)

browseURL(fl)
}

