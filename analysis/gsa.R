setwd(paste0(Sys.getenv('CS_HOME'),'/Governance/Models/CoevolGov'))

library(dplyr)
library(ggplot2)
library(sensitivity)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))


# test of sobol salt
n <- 10000;X1 <- data.frame(matrix(runif(8 * n), nrow = n));X2 <- data.frame(matrix(runif(8 * n), nrow = n))
#f<-function(X){c(rowSums(X),rowMeans(X))} # does not seem to work with multidim outputs
f<-function(X){rowSums(X)}
s = sobolSalt(model = f, X1 = X1, X2 = X2 , scheme = "A")#, nboot = 1000)


res <- read.csv('saltelli/20201207_081644_GSA_GRID.csv')

indics = c("complexityAccessibility","complexityCloseness","complexityPop","deltaAccessibility",
           "deltaAccessibilityBaseline","deltaCloseness","deltaClosenessBaseline","deltaEntropyAccessibility",
           "deltaEntropyCloseness","deltaEntropyPop","deltaHierarchiesAccessibility","deltaHierarchiesAccessibilityBaseline",
           "deltaHierarchiesCloseness","deltaHierarchiesClosenessBaseline","deltaHierarchiesPop","deltaHierarchiesPopBaseline",  
           "deltaPop","deltaPopBaseline","deltaRankCorrsClosenessAccessibility","deltaRankCorrsPopAccessibility",  
           "deltaRankCorrsPopCloseness","diversityAccessibility","diversityCloseness","diversityPop",                  
           "governanceAccessDiff","governanceCollab","governanceCostDiff","governanceLevel",                  
           "governanceProbas","networkCost","networkCostBasline","rankCorrAccessibility","rankCorrCloseness","rankCorrPop")

params = c("gravityDecay","gravityGamma","gravityInterRatio","govCostToAccess","govEffectiveProba",
           "nwExponent","nwGmax","nwReinQuantile","synthRankSize","seed")

paramindex <- function(row){paste0(format(row,digits = 5),collapse = "" )} # dirty
pnames <- apply(res[,params],1,paramindex)
length(which(duplicated(pnames))) # a few duplicated params: why? bug in Saltelli? rq: less when adding seed
length(which(duplicated(res[,params]))) # indeed param values
X = res[!duplicated(pnames),params]
rownames(X) <- pnames[!duplicated(pnames)]
Y = res[!duplicated(pnames),indics]
rownames(Y) <- pnames[!duplicated(pnames)]

X1 = X[1:(nrow(X)/2),]
X2 = X[(nrow(X)/2+1):nrow(X),]

#debug
p <- ncol(X1);s <- seq(1, p); I1 <- (p + 1) * s
X0 <- matrix(c(X1, rep(X2, p + 1)), ncol = (p + 2) * p)
X0[, I1] = X1[, s]
X <- matrix(X0[, c(outer(seq(1, (p + 2) * p, by = p), seq(0, p - 1), "+"))], ncol = p)
x <- list(model = model, X1 = X1, X2 = X2, scheme = "A", 
          nboot = 0, conf = 0.95, X = X, call = match.call())
class(x) <- "sobolSalt"
tell(x,y = model(x$X))

for(indic in indics){
  model <- function(xmat){Y[rownames(xmat),indic]}
  sens=sobolSalt(model = model, X1 = X1, X2 = X2, scheme = "A") # no need for conf int
}

