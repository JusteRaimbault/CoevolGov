setwd(paste0(Sys.getenv('CS_HOME'),'/Governance/Models/CoevolGov'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

resprfix = ''
res <- as.tbl(read.csv(paste0('exploration/',resprefix,'.csv')))
resdir = paste0('../../Results/CoevolGov/',resprefix,'/');dir.create(resdir)

indics = c()

seed = 42
set.seed(seed)
samples = 10

# some histogram plots for some interesting indics
for(indic in c()){
  g=ggplot(res[res$id %in% sample(unique(res$id),samples),],aes_string(x=indic,fill=id,group=id))
  g+geom_density(alpha=0.3)+stdtheme
  ggsave(file=paste0(resdir,indic,'_samples',samples,'-seed',seed,'.png'),width=20,height=18,units='cm')
}

# sharpes 
sres = res %>% group_by(id) %>% summarize_at(
  indics,list(mean = mean, sd = sd, sharpe = ~ abs(mean(.x))/sd(.x))
)
summary(sres)

# distances between averages in comparison of std
# 2 * |mu_i - mu_j| / (sigma_i + sigma_j)
reldistance <- function(indic,sdindic){
  c(2*abs(matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = T) - matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = F))/(matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = T) + matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = F)))
}
sapply(indics,function(indic){summary(reldistance(paste0(indic,"_mean"),paste0(indic,"_sd")))})



