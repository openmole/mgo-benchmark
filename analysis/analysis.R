
setwd(paste0(Sys.getenv('CS_HOME'),'/NoisyEA/Results/MGOBenchmark'))

library(dplyr)
library(ggplot2)


# remove old result files -> TODO do definitively in docker

resdir='20181107_120452_DIRECTSAMPLING_GRID'
figdir=paste0(Sys.getenv('CS_HOME'),'/NoisyEA/Results/MGOBenchmark/',resdir,'/');dir.create(figdir)

res=data.frame()
for(f in list.files(paste0(resdir,'/data'))){
  if(substr(f,nchar(f)-9,nchar(f))!='params.csv'){
    currentdata=as.tbl(read.csv(paste0(resdir,'/data/',f),sep=';'))
    # read parameters
    params = read.csv(paste0(resdir,'/data/',strsplit(f,'.',fixed = T)[[1]][1],'_params.csv'),sep=";")
    for(param in names(params)){currentdata[[param]]=rep(params[[param]],nrow(currentdata))}
    res=rbind(res,currentdata)
  }
}

res$problem = sapply(strsplit(as.character(res$id),'_'),function(l){paste(l[1:3],sep="",collapse = "_")})
#res$algo = sapply(strsplit(as.character(res$id),'_'),function(l){paste(l[4:length(l)],sep="",collapse = "_")})
res$algo = sapply(strsplit(as.character(res$id),'_'),function(l){strsplit(l[4],"-")[[1]][1]})

#res%>% group_by(problem,algo,sigma) %>% summarise(count=n())

epsilon = 0.1

res$solved = ifelse(res$precision < epsilon,1,0)
res$runfactor = cut(log(res$runs),breaks = 20)

sres = res %>% group_by(algo,sigma,runfactor)  %>% summarise(
  success = sum(solved)/n()
)

g=ggplot(sres,aes(x=runfactor,y=success,group=algo,colour=algo))
g+geom_point()+geom_line()+facet_wrap(~sigma)
ggsave(file=paste0(figdir,'successrate-runs_epsilon',epsilon,'.png'),width=15,height=10,units='cm')




