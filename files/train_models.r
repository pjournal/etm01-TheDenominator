
  train_glmnet <- function(train_features, test_features){
  
  set.seed(1)
  #SIL
  #train_features = temp4
  #glm does not work with complete data
  glm_features=train_features[complete.cases(train_features)]
  train_class=glm_features$outcome
  #SIL
  not_included_feature_indices=c(1:5)
  #require(caTools)
  #split=sample.split(glm_features$outcome,SplitRatio=0.7)
  #glm_train_data=subset(glm_features,split==TRUE)
  #glm_test_data=subset(glm_features,split==FALSE)
  glm_train_data=glm_features[,-not_included_feature_indices,with=F]
  glm_test_data=test_features[,-not_included_feature_indices,with=F]
  #SIL
  #sum(is.na(glm_train_data))
  #glm_test_data=test_features[,-not_included_feature_indices,with=F]
  ##BURALAR HEP CV IDI
  # fit final glmnet model with the lambda with minimum error
  #final_glmnet_fit = glm(as.matrix(glm_train_data),as.factor(train_class),family=binomial(link="logit")), alpha = alpha,lambda=cvResultsSummary$lambda.min)
  #final_glm_fit = glm(as.matrix(glm_train_data),as.factor(train_class),family=binomial)
  def = as.data.frame(glm_train_data)
  def$outcome=factor(def$outcome)
  default = as.factor(train_class)
  final_glm_fit = glm(outcome~.,data=def,family="binomial")
  # obtain predictions
  predicted_probabilities=predict(final_glm_fit, as.data.frame(glm_test_data), type = "response")
  #SIL
  #Pred_Prob_Obj=array(predicted_probabilities)
  #check order of predictions
  #order_of_class=attr(Pred_Prob_Obj,'dimnames')[[2]]
  #new_order=c(which(order_of_class=='OVER'),which(order_of_class=='under'))
  
  #final_result=data.table(test_features[,list(matchId,outcome)],predicted_probabilities[,new_order,1])
  
  final_result=data.table(test_features[,list(matchId,outcome)],predicted_probabilities)
  
  return(list(predictions=final_result))
}
#testStart=as.Date('2018-11-16')
#trainStart=as.Date('2012-07-15')
#train_features=temp4[Match_Date>=trainStart & Match_Date<testStart] 
#test_features=temp4[Match_Date>=testStart] 

#temp5=train_glmnet(train_features, test_features)
#temp5=train_glmnet(train_features, test_features,not_included_feature_indices=c(1:5), alpha=1,nlambda=50, tune_lambda=TRUE,nofReplications=2,nFolds=10,trace=T)
