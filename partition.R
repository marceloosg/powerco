data_partition.make=function(index,y,k) lapply(1:k,function(i) downSample(index,factor(y),list=F))
data_partition.apply=function(self,ds,y=F){
  assert("dimension mismatch",dim(ds)[1]==length(self$index))
  result=lapply(self$partition,function(p){
    parted=ds[p$x]
    if(y) parted=cbind(parted,data.table(y=p$Class))
    parted
  } )
  names(result)=paste("Resample",1:(self$k),sep=".")
  result
}
data_partition.measure= function(partition,func){
  lcor=rbindlist(lapply(names(partition),function(sname){
    dt=partition[[sname]]
    d=as.data.table(t(dt[,lapply(.SD,function(x) func(x,y))]),keep.rownames=T)
    names(d)[2]="measure"
    d=d[order(-measure)][1:10]
    d$sample=sname
    d}))
  lcor=lcor[,.(sd=sd(measure),measure=mean(measure)),.(rn)]
  lcor[,rn:=factor(rn,levels=rn,ordered=T)]
  lcor[,sdf:=factor(substr(as.character(sd),1,5))]
  ggplot(data=lcor,aes(x=rn,y=measure,fill=sd,ymin=measure-sd,ymax=measure+sd))+geom_bar(stat="identity")+coord_flip()+geom_errorbar()  
}
data_partition.regression = function(partition){
  lapply(names(partition),function(sname){
    dt=partition[[sname]]
    ggplot(dt,aes(x=activ_year*100+(activ_month/12-0.5/12)*100,y=as.numeric(y)-1))+
      geom_point(data=dt[,.(.N,y=mean(as.numeric(y)-1)),.(x=activ_year*100+(activ_month/12-0.5/12)*100)],aes(x=x,y=y,size=N),alpha=0.5)+
      geom_smooth(span=0.2)+
      ggtitle(sname)+xlab("")+ylab("")+
      theme(axis.title.x=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none")
  })
}


data_partition=function(x,y,k=5){
  index=1:dim(x)[1]
  assert("dimension mismatch",length(index)==length(y))
  self=list()
  self$k=k
  self$index=index
  self$partition=data_partition.make(index,y,k)
  self$apply=data_partition.apply
  self$measure=data_partition.measure
  self
}
