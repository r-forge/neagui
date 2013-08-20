\name{nea}
\alias{nea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Network Enrichment Analysis
}
\description{
nea performs network enrichment analysis.
The number of network links between a novel altered gene set (AGS) and a known functional gene set (FGS) is computed and 
assessed by the number of network links obtained from randomly permuted network where the degree distribution is kept. 
As a measure of activation of the FGS, we provide the z-score. 

manypnet generates a specified number of randomly permuted networks. 
}
\usage{
nea(ags, fgs, fgslib = NULL, network, pnet = NULL, nperm = 50, stat="F", seed = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{ags}{
A vector of altered genes. Gene symbols (upper case) are used as a default.
}
  \item{fgs}{
A list defined by user or a character to specify GO ontologies or KEGG pathway. 
User can provide their own functional 
gene sets as a list. 
Options to specify GO ontologies or KEGG pathway are
"CC","BP","MF" and "KEGG" (cellular component, biological process, moelcular function and KEGG pathway). 
}
  \item{fgslib}{
A character of the name of annotation data.
To use GO terms or KEGG pathways, a specific annotation data should be specified. 
}
  \item{network}{
A vector of gene pairs 
or a list representing the network link. 
For the vector case, each element has
a combined name of two gene symbols with separation ''. For example, 
"7SK ALDOA". The number of network links
is computed with respect to this network.
}
  \item{pnet}{
A list of randomly permuted networks. 
This is the output of manypnet. If you do not specify this,
the permutation procedure for the network automatically performed. 
}
  \item{nperm}{
The number of permutations
}
  \item{stat}{
Two types of network enrichment statistic: FNEA and MNEA. 
FNEA (stat="F") depends on the size of ags while MNEA (Stat="M") avoids the dependence by taking
the largest statistic across AGS. Default is FNEA.
}
  \item{seed}{
A seed number for the permutation procedure
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

 \item{MainResult}{A matrix (number of rows equal length of fgs) of numbers (stat="F") or a matrix (length of fgs by length of ags lying in the given network) of numbers (stat="M"). The numbers are the network links between AGS and FGS under the given network.}
 \item{res.nlink}{A matrix (the length of fgs by the number of permutation) of the imputed FGS (pathways) (fgs names are shown as rownames). TheFGS are shown (sorted by on te p-values) with corresponding number of observed and expected network links, number of genes, number of AGS genes, z-score, 
p-values based on network permutations, and the false discovery rate (FDR).
}
\item{geneinfgs}{A vector containing genes from AGS which are in the FGS}
\item{fgslist}{A list of gene symbols for each FGS}
\item{pnetout}{A list of permuted network matrix. It can be used for other analysis.} 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Woojoo Lee, Setia Pramana and Yudi Pawitan
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{## Example 1: artifical data.

AGS<-c("A","B","C","D","E")
FGS<-list(FGS1=c("B","E","F","G","H"),FGS2=c("F","C"))
NETWORK<-c("A B","A F","E C","Q R","S T","U V")

res<-nea(ags=AGS, fgs = FGS, fgslib = NULL, network=NETWORK, pnet = NULL, nperm = 50, stat="F", seed = 1234)
res$MainResult 
}
## Example 2: use of annotaion data.
## We use gene symbols (upper case)

\dontrun{
AGS<-c("AIFM3","DIMT1L","ADNP","AHCYL1","EIF4H","RGL1","SEC23IP","EIF4A1","CSNK2B","NOS3")
NETWORK<-c("DNAJC6 RGL1","C1ORF156 NCBP2","AHCYL1 RTN3","PLK4 SKIV2L2","C22ORF28 MESDC2","TINP1 UTP23",    
"HEATR3 MVD","WBP11 XAB2","CSNK2B PA2G4","GCN1L1 RRM2","DIMT1L SMC1A","GPN3 THOC3",     
"DLG3 GPHN","C19ORF29 EXOSC4","AIFM3 SFXN5","HSPA1L RUVBL2","DLAT EIF4A1","ADNP XRCC5",     
"NOS2 NOS3","CIZ1 TLK2","MRPL49 RPS7","GSPT1 SLK","LUC7L2 SEC23IP","DHX8 IGF2BP3",   
"CNTROB SASS6","MRPS12 RPLP2","DHODH EIF4H","GINS3 KIF23","ANXA5 TGFBI","CDK5 PMM1")

res<-nea(ags=AGS, fgs = "KEGG", fgslib = "KEGG.db", network=NETWORK, pnet = NULL, nperm = 5, stat="F", seed = 1234)
res$MainResult 
}
}