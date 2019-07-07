jaccard = function (x, y) {
  M.11 = sum(x == 1 & y == 1)
  M.10 = sum(x == 1 & y == 0)
  M.01 = sum(x == 0 & y == 1)
  return (M.11 / (M.11 + M.10 + M.01))
}
accuracy = function (x, y) {
  M.11 = sum(x == 1 & y == 1)
  M.10 = sum(x == 1 & y == 0)
  M.01 = sum(x == 0 & y == 1)
  M.00 = sum(x == 0 & y == 0)
  return ((M.11+M.00) / (M.00 + M.11 + M.10 + M.01))
}
brierSummary <- function (data, lev = NULL, model = NULL) { # for training on a next-period return
  #browser() #essential for debugging
  dat=dim(data)
  
  
  lvls <- levels(data$obs)
  if (length(lvls) > 2) 
    stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
  if (!all(levels(data[, "pred"]) == lvls)) 
    stop("levels of observed and predicted data do not match")
  rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 
                                     1), data[, lvls[1]])
  out.ROC <- c(rocAUC, sensitivity(data[, "pred"], data[, "obs"], 
                                   lev[1]), specificity(data[, "pred"], data[, "obs"], lev[2]))
  names(out.ROC) <- c("ROC", "Sens", "Spec")
  
  
  
  # get observed dummy
  Y_obs = model.matrix( ~ data[, "obs"] - 1) # create dummy - for each level of the outcome
  # get predicted probabilities
  Y_pre=as.data.frame(data[ , c("f","t")])
  # get rownumbers
  rows=data[,"rowIndex"]
  
  # put it all together
  df_temp=data.frame(Y_obs,Y_pre)
  names(df_temp)=c("Y_cur","Y_pre","p_cur","p_pre")
  out=df_temp %>% summarise(BS_cur=1/n()*sum((Y_cur-p_cur)^2),
                            BS_pre=1/n()*sum((Y_pre-p_pre)^2),
                            Accuracy=1/n()*sum((Y_cur== ifelse(p_cur>0.5,1,0))))
  # name 
  names(out)=c("BS_cur","BS_pre","Accuracy")
  # now create one line of return - caret seems to be able to hande only one
  out=c(out,out.ROC)
  out=as.data.frame(out)
  out_stack=stack(out)
  
  # recall, the return type must be simply numeric
  out_final=as.numeric(t(out_stack[,1]))
  names(out_final)=(out_stack[,2])
  
  
  return(out_final)
}