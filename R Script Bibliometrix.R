library(bibliometrix)
## Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier.
##                         
## 
## http:\\www.bibliometrix.org

# Sink Log of all data output
con <- file("/Users/stefan_kleipoedszus/Documents/@Promotion/R Literature Review/Biblometrics_Data.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")


D <- readFiles("/Users/stefan_kleipoedszus/Documents/@Promotion/R Literature Review/MetaData/Corpus_WoK.bib")
M <- convert2df(D, dbsource = "isi", format = "bibtex")
results <- biblioAnalysis(M, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)
# To obtain the most frequent cited manuscripts
print("Most Cited Manuscripts")
CR <- citations(M, field = "article", sep = ".  ")
cbind(CR$Cited[1:10])

# To obtain the most frequent cited first authors
print("Most Cited First Authors")
CR <- citations(M, field = "author", sep = ".  ")
cbind(CR$Cited[1:10])
# Find most frequent cited local author
print("Most Cited First Author and paper (Local)")
CR <- localCitations(M, sep = ".  ")
CR$Authors[1:10,]
CR$Papers[1:10,]

# Authors dominance ratings
print("Dominance Ranking as proposed by Kumar & Kumar (2008)")
DF <- dominance(results, k = 10)
DF

# Bibliographic network matrices
A <- cocMatrix(M, Field = "AU", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")
tiff(paste0("/Users/stefan_kleipoedszus/Documents/@Promotion/R Literature Review/Plots/", "Co_Citation.tiff"), 
     units="in", width=5, height=5, res=300)
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Co-Citation - Network", 
                type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,
                label.cex=F)
dev.off()

# Create keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
tiff(paste0("/Users/stefan_kleipoedszus/Documents/@Promotion/R Literature Review/Plots/", "Keyword_co_occ.tiff"), 
     units="in", width=5, height=5, res=300)
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 60, Title = "Keyword Co-occurrences", 
                type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
dev.off()

# Conceptual Analysis
CS <- conceptualStructure(M,field="ID", method="ID", minDegree=4, k.max=8, stemming=FALSE, 
                          labelsize=10, documents=20)

# Create a themtic map 
#Cobo et al. 2011 
S <- normalizeSimilarity(NetMatrix, type = "association")
net <- networkPlot(S, n = 150, Title = "co-occurrence network",type="fruchterman",
                   labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE,
                   remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res <- thematicMap(net, NetMatrix, S)
plot(res$map)




years=c(2014)
list_df=timeslice(M, breaks = years)
M1=list_df[[1]]
M2=list_df[[2]]
NetMatrix1 <- biblioNetwork(M1, analysis = "co-occurrences", 
                            network = "keywords", sep = ";")
S1 <- normalizeSimilarity(NetMatrix1, type = "association")
net1 <- networkPlot(NetMatrix1, normalize = "association",n = 50, 
                    Title = "co-occurrence network",type="fruchterman",
                    labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE,
                    remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res1 <- thematicMap(net1, NetMatrix1, S1, minfreq = 1)
#plot(res1$map)

NetMatrix2 <- biblioNetwork(M2, analysis = "co-occurrences", 
                            network = "keywords", sep = ";")
S2 <- normalizeSimilarity(NetMatrix2, type = "association")
net2 <- networkPlot(NetMatrix2, normalize = "association",n = 50, 
                    Title = "co-occurrence network",type="fruchterman",
                    labelsize = 0.7, halo = FALSE, cluster = "walktrap",remove.isolates=FALSE,
                    remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res2 <- thematicMap(net2, NetMatrix2, S2, minfreq = 1)
#plot(res2$map)

nexus <- thematicEvolution(res1,res2,weighted=FALSE)


