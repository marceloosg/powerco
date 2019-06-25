tag.na=function(frame){
is.na.data=frame[,lapply(.SD,is.na)]  
na.cols=is.na.data[,which(sapply(.SD,any))]
na.data=is.na.data[,na.cols,with=F]+0

}
fill.na=function(frame){
  na.data=frame[,lapply(.SD,is.na)]  
  frame[,na.cols,with=F][,lapply(.SD,function(s) ifelse(is.na(s),median(s,na.rm=T),s))]
 }
