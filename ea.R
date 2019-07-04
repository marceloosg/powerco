mh=merge(hist,output,by='id')
mmg=melt(mh,id.vars=c("id","churn","price_date"))
z=mmg[,.(N=length(unique(id))),.(churn,price_date,is.na(variable),variable,value)]
z=z[,.(N,churn,value=value/max(value,na.rm=T)),.(price_date,is.na(variable),variable)]

zz=z[order(price_date)][order(price_date,variable,value,N)]
zz=zz[,.(N,value,z=N/sum(N),p=cumsum(N)/sum(N)),.(price_date=month(price_date),variable,churn=as.character(churn))]
zz=rbind(zz,zz[order(price_date,variable,value,churn,N),.(churn="T",N,z=N/sum(N),value,p=cumsum(N)/sum(N)),
               .(price_date,variable)])

ggplot(zz[variable=="price_p1_fix"][,.(z=sum(z)),.(price_date,churn,value=cut(value,b=10))],
       aes(x=value,y=z,fill=factor(churn)))+geom_bar(stat="identity",position="dodge")+
  facet_wrap(~price_date)+coord_flip()

+geom_hline(yintercept = 0.4)

ggplot(zz,aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_grid(price_date~variable)+
  geom_hline(yintercept = 0.4)


v=cbind(preprocessed_training_data$data$raw$values,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
vz=v[variable=="nb_prod_act"][,.N,.(churn,value)][order(value,churn)][,.(value,N,p=cumsum(N)/sum(N)),.(churn)]
ggplot(vz,aes(x=value,y=p,color=factor(churn)))+geom_line()


v=cbind(preprocessed_training_data$data$raw$values,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
v=v[,.N,.(variable,value,churn)]
v=v[,.(N,value=value/max(value),churn),.(variable)]
v=v[order(variable,value,churn,N)][,.(N,value,p=cumsum(N)/sum(N)),.(churn,variable)]
ggplot(v,aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_wrap(~variable)

ggplot(v[variable=="nb_prod_act"],aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_wrap(~variable)



v=cbind(preprocessed_training_data$data$output$imputed_values,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
v=v[,.N,.(variable,value,churn)]
v=v[,.(N,value=value/max(value),churn),.(variable)]
v=v[order(variable,value,churn,N)][,.(N,value,p=cumsum(N)/sum(N)),.(churn,variable)]
ggplot(v,aes(x=value,y=log(p),color=factor(churn)))+geom_line()+facet_wrap(~variable)+geom_hline(yintercept = log(0.5))
  geom_vline(xintercept = 0)


v=cbind(preprocessed_training_data$output$val,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
v=v[,.N,.(variable,value,churn)]
v=v[,.(N,value=value/max(value),churn),.(variable)]
v=v[order(variable,value,churn,N)][,.(N,value,p=cumsum(N)/sum(N)),.(churn,variable)]
ggplot(v,aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_wrap(~variable)


v=cbind(preprocessed_training_data$output$bin,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
v=v[,.N,.(variable,value,churn)]
v=v[,.(N,value=value/max(value),churn),.(variable)]
v=v[order(variable,value,churn,N)][,.(N,value,p=cumsum(N)/sum(N)),.(churn,variable)]
ggplot(v,aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_wrap(~variable)

v=cbind(preprocessed_training_data$data$combined$binaries,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
v[,cor(value,churn),variable][order(-abs(V1))]


v=v[ ,.N,.(variable,value,churn)]
v=v[variable=="origin_up.lxidpiddsbxsbosboudacockeimpuepw",.(N,value=value/max(value),churn),.(variable)]
v=v[order(variable,value,churn,N)][,.(N,value,p=cumsum(N)/sum(N)),.(churn,variable)]
ggplot(v,aes(x=value,y=p,color=factor(churn)))+geom_point()+facet_wrap(~variable)

v=cbind(preprocessed_training_data$data$output$imputed_values,preprocessed_training_data$data$output$id)
a=merge(v,output,by='id')
ggplot(a,aes(x=log(cons_12m+1),y=..density..,fill=factor(churn)))+geom_histogram()+geom_density()



cumplot=function(v,col){
  names(v)[grep(col,names(v))]="value"
  v=v[,.N,.(churn,value)][order(value,churn)][,.(value,N,p=cumsum(N)/sum(N)),.(churn)]
  ggplot(v,aes(x=value,y=p,color=factor(churn)))+geom_line()
}
ggplot(a,aes(x=factor(churn),y=log(cons_last_month)))+geom_boxplot()

ggplot(a,aes(x=log(cons_last_month+1),y=..density..,fill=factor(churn)))+geom_histogram()+geom_density()

b=a
b$cut=cut(log(b$cons_last_month-min(b$cons_last_month)+1)/max(log(b$cons_last_month-min(b$cons_last_month)+1)),100)
b=b[,.N,.(cut,churn)][,.(N,p=N/sum(N),cut),churn]
ggplot(b,aes(x=cut,y=p,fill=factor(churn)),alpha=0.5)+geom_bar(stat="identity",position="dodge")+coord_flip()

dif=function(v) { q=v[1]-v[2];w=v[1]+v[2];ifelse(q*w < 0,0,v[1])}

date_churn[,.N,.(year(date_end),month(date_end),churn)][order(year,month,churn,N)]
ggplot(date_churn[year(date_end)==2016,.N,.(date_end,churn)],aes(x=date_end,y=N))+geom_point()+facet_wrap(~churn)

ggplot(data=date_churn[year(date_end)==2016,.(date_end,churn)],aes(x=date_end,y=..density..,fill=factor(churn)))+geom_histogram()+geom_density()

fm=mmg[,.(f=sd(value,na.rm=T),price_date=as.Date(price_date,format="%Y-%m-%d"),value,churn),.(id,variable)]

sfm=fm[,.(slope=ifelse(is.na(f),as.double(NA),dif(summary(lm(value~price_date))$coefficients[c(2,4)]))),.(churn,variable,id,f)]
sfm[is.na(f)]$f=0
hsfm=merge(sfm,cbind(preprocess_model$data$output$id,preprocess_model$data$output$binary),by="id")

ifsm=hsfm[,.(id,churn,x=has_gas+(V1-min(V1,na.rm=T)/max(V1,na.rm=T))),.(variable)]
hsfma=merge(hsfm,a,by=c("id","churn"))


mm=hsfma[,.(slope,cg=mean(cons_gas_12m)),.(has_gas,churn,variable)]
umm=unique(mm)
library(MASS)
fumm=cbind(umm[,-"variable"],getdummy(umm[,.(variable)]))
fumm[is.na(slope)]$slope=0
pca=prcomp(fumm[,-c("churn")],rank. = 4)
dpca=predict(pca,fumm[,-("churn")])
lda.fit=lda(churn ~ .,data=fumm)
