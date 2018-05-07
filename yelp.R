#question2
yelp=read.csv("aggregate.csv",header = T)#load the data
yelp <- subset(yelp, select = -businessId)
yelp <- subset(yelp, select = -name)
yelp <- subset(yelp, select = -neighborhood)
yelp <- subset(yelp, select = -address)
yelp <- subset(yelp, select = -postal_code)
yelp <- subset(yelp, select = -categories)
#perform a logistic regression
model=glm(ifFail~.-failTimes,data=yelp)
summary(model)
train=yelp[1:floor(nrow(yelp)*0.8),]#get the train set
test=yelp[(floor(nrow(yelp)*0.8)+1):nrow(yelp),]#get the test set
#perform a logistic regression using the validation set approach
glmModel=glm(ifFail~.-failTimes,data=train)
summary(glmModel)
#do the prediction
probs=predict(glmModel,test)
#if possiblity>0.5, the result is supposed to be 1
prediction=rep("0",nrow(test))
prediction[probs >.5]="1"
table(prediction,test$ifFail)
#calculate the error
mean(prediction==test$ifFail)
1-mean(prediction==test$ifFail)

lmModel=lm(failTimes~.-ifFail,data=yelp)
summary(lmModel)

library(MASS)
ldaModel=lda(ifFail~.-failTimes,data=train)
ldaModel
ldaPrediction=predict(ldaModel,test)
ldaResult=ldaPrediction$class
table(ldaResult,test$ifFail)
1-mean(ldaResult==test$ifFail)

#perform QDA
qdaModel=qda(ifFail~.-failTimes,data=train)
qdaModel
qdaPrediction=predict(qdaModel,test)
qdaResult=qdaPrediction$class
table(qdaResult,test$ifFail)
1-mean(qdaResult==test$ifFail)