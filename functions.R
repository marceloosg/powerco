library(caret)
library(FactoMineR)
library(factoextra)
train.na=function(frame){
  is.na.data=frame[,lapply(.SD,is.na)]  
  na.model=is.na.data[,which(sapply(.SD,any))]
  na.model
}
predict.na=function(na.model,frame){
  is.na.data=frame[,lapply(.SD,is.na)]  
  na.data=is.na.data[,na.model,with=F]+0
  na.data
}
fill.na=function(frame){
  na.data=frame[,lapply(.SD,is.na)]  
  frame[,na.cols,with=F][,lapply(.SD,function(s) ifelse(is.na(s),median(s,na.rm=T),s))]
}
as.names=function(frame,index){
  names(frame)[index]
}
get_index=function(frame){
  id=1
  dummy=nearZeroVar(frame)
  dates=grep("date",names(frame))
  dummy=setdiff(dummy,dates)
  binary=grep("has_gas",names(frame))
  categorical=setdiff(which(sapply(data,function(s) class(s)=="character")==T),dates)
  categorical=setdiff(categorical,c(id,dummy,binary))
  value=setdiff(1:dim(data)[2],c(id,dummy,dates,categorical))
  output=list("id"=id,"dummy"=dummy,"dates"=dates,"binary"=binary, "categorical"=categorical,"value"=value)
  lapply(output,function(s) as.names(frame,s))
}
get_binary=function(frame,indexes){
  data[,indexes$binary,with=F][,lapply(.SD,function(s) as.numeric(as.factor(s))-1)]
}
transform_dates=function(cdates){
  cdates[,activ_year:=year(date_activ)]
  cdates[,activ_month:=month(date_activ)]
  cdates[,days_from_first_activation:= as.numeric(date_activ-date_first_activ)]
  cdates[,days_from_modif_prod:= as.numeric(date_modif_prod - date_activ)]
  cdates[,days_from_date_end:= as.numeric(date_end - date_activ)]
  cdates[,days_from_renewal:= as.numeric(date_renewal - date_activ)]
  cdates[,-c(1:5)]
}
get_dates=function(frame,indexes){
  cdates=data[,indexes$dates,with=F][,lapply(.SD,function(d) as.Date(d,format="%Y-%m-%d"))]
  transform_dates(cdates)
}

get_categorical=function(frame,indexes){
  data[,indexes$categorical,with=F][,lapply(.SD,as.factor)]
}
get_values=function(frame,indexes){
  data[,indexes$value,with=F]
}
format_types=function(frame,indexes){
  cid=frame[,indexes$id,with=F]
  binary=get_binary(frame,indexes)
  cdates=get_dates(frame,indexes)
  ccategorical=get_categorical(frame,indexes)
  cvalues=get_values(frame,indexes)
  cvalues=cbind(cdates,cvalues)
  list("output"=list("id"=cid,"binary"=binary),"raw"=list("categorical"=ccategorical,"values"=cvalues))
}
train_data_structure=function(frame,indexes){
  formated_data=format_types(frame,indexes)
  na_model=train.na(formated_data$raw$values)
  list("indexes"=indexes, "format"=formated_data,"na_model"=na_model)
}
predict_data_structure=function(structure_model,frame){
  formated_data=format_types(frame,structure_model$indexes)
  na_values=predict.na(structure_model$na_model,formated_data$raw$values)
  formated_data$output$na_values=na_values
  formated_data
}
train_categorical_one_hot=function(structure_model){
  dummyVars("~ .",data= structure_model$format$raw$categorical)
}
predict_one_hot_model=function(one_hot_categorical_model,structure_data){
  one_hot_categorical_data=data.table(predict(one_hot_categorical_model,structure_data$raw$categorical))
  one_hot_categorical_data
}

train_preprocess_model=function(frame){
  print("Remove Zero Variance columns and split columns by type")
  indexes=get_index(frame)
  print("-------ok")
  print("Train data structure")
  structure_model=train_data_structure(frame,indexes)
  print("-------ok")
  print("Train one Hot Encode of categorical columns")
  one_hot_categorical_model=train_categorical_one_hot(structure_model)
  print("-------ok")
  print("Train imput model")
  imput_model=preProcess(structure_model$format$raw$values,method="knnImpute")
  print("-------ok")
  list("structure_model"=structure_model,
       "one_hot_categorical_model"=one_hot_categorical_model, 
       "imput_model"=imput_model)
}
predict_preprocess_model=function(model,frame){
  print("Predict data structure")
  structure_data=predict_data_structure(model$structure_model,frame)
  print("-------ok")
  print("Predict one Hot Encode of categorical columns")
  structure_data$output$one_hot_categorical_data=predict_one_hot_model(model$one_hot_categorical_model,structure_data)
  print("-------ok")
  print("Predict Imput data") 
  structure_data$output$imputed_values=predict(model$imput_model,structure_data$raw$values)

  print("-------ok")
  structure_data
}
cbindlist=function(dlist){
  
  aux=dlist[names(dlist)[1]][[1]]
  for(a in dlist[names(dlist)[-1]]){
    aux=cbind(aux,a)
  }
  aux
}