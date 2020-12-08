
setwd(paste0(Sys.getenv('CS_HOME'),'/Governance/Models/CoevolGov'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

resprefix = '20201206_223823_GRID_EXPLORATION_GRID'
res <- as.tbl(read.csv(paste0('exploration/',resprefix,'.csv')))
resdir = paste0('../../Results/CoevolGov/',resprefix,'/');dir.create(resdir)

indics = c("complexityAccessibility","complexityCloseness","complexityPop","deltaAccessibility",
           "deltaAccessibilityBaseline","deltaCloseness","deltaClosenessBaseline","deltaEntropyAccessibility",
           "deltaEntropyCloseness","deltaEntropyPop","deltaHierarchiesAccessibility","deltaHierarchiesAccessibilityBaseline",
           "deltaHierarchiesCloseness","deltaHierarchiesClosenessBaseline","deltaHierarchiesPop","deltaHierarchiesPopBaseline",  
           "deltaPop","deltaPopBaseline","deltaRankCorrsClosenessAccessibility","deltaRankCorrsPopAccessibility",  
           "deltaRankCorrsPopCloseness","diversityAccessibility","diversityCloseness","diversityPop",                  
           "governanceAccessDiff","governanceCollab","governanceCostDiff","governanceLevel",                  
           "governanceProbas","networkCost","networkCostBasline","rankCorrAccessibility","rankCorrCloseness","rankCorrPop")

params = c("gravityDecay","gravityGamma","gravityInterRatio","govCostToAccess","govEffectiveProba",
           "nwExponent","nwGmax","nwReinQuantile","synthRankSize")
# "finalTime"

sindics = c("deltaPop","deltaHierarchiesPop","deltaAccessibility","networkCost","governanceCollab","rankCorrPop")

# smoothed plots?
for(indic in sindics){
for(gravityInterRatio in unique(res$gravityInterRatio)){
for(govCostToAccess in unique(res$govCostToAccess)){
  for(nwExponent in unique(res$nwExponent)){
    for(nwGmax in unique(res$nwGmax)){
    for(nwReinQuantile in unique(res$nwReinQuantile)){
  sres = res[res$gravityInterRatio==gravityInterRatio&res$govCostToAccess==govCostToAccess&res$nwExponent==nwExponent&
               res$nwGmax==nwGmax&res$nwReinQuantile==nwReinQuantile,]
  g=ggplot(sres,aes_string(x='gravityDecay',y=indic,color='govEffectiveProba',group='govEffectiveProba'))
  ggsave(g+geom_point(pch='.')+geom_smooth()+facet_grid(synthRankSize~gravityGamma,scales='free'),file=paste0(resdir,indic,'_gravityInterRatio',parstr(gravityInterRatio),'_govCostToAccess',parstr(govCostToAccess),
                     '_nwExponent',parstr(nwExponent),'_nwGmax',parstr(nwGmax),'_nwReinQuantile',parstr(nwReinQuantile),'.png'),width=30,height=25,units='cm')
}}}}}
}




#### cloud point/possible Pareto fronts with saltelli lhs?


resprefix = '20201207_081644_GSA_GRID'
res <- as.tbl(read.csv(paste0('saltelli/',resprefix,'.csv')))
resdir = paste0('../../Results/CoevolGov/',resprefix,'/');dir.create(resdir)

sres = res %>% mutate_at(
  params,list(function(x){cut(x,2)})
) %>% group_by_at(params) %>% summarize_at(
  sindics,list(mean)
)

g=ggplot(sres,aes(x=deltaAccessibility,y=-networkCost,color=governanceCollab))
g+geom_point()+facet_grid(synthRankSize~gravityInterRatio,scales='free')+stdtheme+scale_color_continuous(name='g')
ggsave(file=paste0(resdir,'pareto-access-cost_cut2.png'),width=20,height=18,units='cm')








