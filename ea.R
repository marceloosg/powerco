mh=merge(hist,output,by='id')
mmg=melt(mh,id.vars=c("id","churn","price_date"))
z=mmg[,.(N=length(unique(id))),.(churn,price_date,is.na(variable),variable,value)]
z=z[,.(N,value=value/max(value,na.rm=T)),.(churn,price_date,is.na(variable),variable)]

zz=z[order(price_date)][order(price_date,variable,value,N)]
zz=zz[,.(N,value,p=cumsum(N)/sum(N)),.(price_date=month(price_date),variable,churn=as.character(churn))]
zz=rbind(zz,zz[order(price_date,variable,value,churn,N),.(churn="T",N,value,p=cumsum(N)/sum(N)),.(price_date,variable)])

ggplot(zz,aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_grid(price_date~variable)



v=cbind(preprocessed_training_data$data$raw$values,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
v=v[,.N,.(variable,value,churn)]
v=v[,.(N,value=value/max(value)),.(variable,churn)]
v=v[order(variable,value,churn,N)][,.(N,value,p=cumsum(N)/sum(N)),.(churn,variable)]
ggplot(v,aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_wrap(~variable)



v=cbind(preprocessed_training_data$data$output$imputed_values,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
v=v[,.N,.(variable,value,churn)]
v=v[,.(N,value=value/max(value)),.(variable,churn)]
v=v[order(variable,value,churn,N)][,.(N,value,p=cumsum(N)/sum(N)),.(churn,variable)]
ggplot(v,aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_wrap(~variable)


v=cbind(preprocessed_training_data$output$val,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
v=v[,.N,.(variable,value,churn)]
v=v[,.(N,value=value/max(value)),.(variable,churn)]
v=v[order(variable,value,churn,N)][,.(N,value,p=cumsum(N)/sum(N)),.(churn,variable)]
ggplot(v,aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_wrap(~variable)


v=cbind(preprocessed_training_data$output$bin,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
v=v[,.N,.(variable,value,churn)]
v=v[,.(N,value=value/max(value)),.(variable,churn)]
v=v[order(variable,value,churn,N)][,.(N,value,p=cumsum(N)/sum(N)),.(churn,variable)]
ggplot(v,aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_wrap(~variable)

v=cbind(preprocessed_training_data$data$combined$binaries,preprocessed_training_data$data$output$id)
v=melt(merge(v,output,by='id'),id.vars= c('id','churn'))
v[,cor(value,churn),variable][order(-abs(V1))]


v=v[,.N,.(variable,value,churn)]
v=v[,.(N,value=value/max(value)),.(variable,churn)]
v=v[order(variable,value,churn,N)][,.(N,value,p=cumsum(N)/sum(N)),.(churn,variable)]
ggplot(v,aes(x=value,y=p,color=factor(churn)))+geom_line()+facet_wrap(~variable)

