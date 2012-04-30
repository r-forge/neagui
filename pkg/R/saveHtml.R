saveHtml <-
function (ResultNEA,fl) {
if (FGS=="KEGG" & AnnotationDB== "KEGG.db")  linkID <- paste("http://www.genome.jp/dbget-bin/www_bget?pathway:",ResultNEA$PATH_ID,sep="")

ResultNEAsrt <- ResultNEA[order(ResultNEA$P_value),]
linkIDsrt <- linkID  [order(ResultNEA$P_value)]

ResultNEAsrt[,4] <- round(ResultNEAsrt [,4] ,4)
ResultNEAsrt[,7:9] <- round(ResultNEAsrt[,7:9] ,6)

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


hwrite(ResultNEAsrt , p, center=TRUE, row.bgcolor='99CCFF',col.link=list(PATH_ID=linkIDsrt ),col.style= algn ,
 cellspacing=0,table.class='raw', col.width=c(PATH_ID='75px', P_value='100px', Z_score='100px', FDR ='100px'),row.names=FALSE)

closePage(p)

browseURL(fl)
}

