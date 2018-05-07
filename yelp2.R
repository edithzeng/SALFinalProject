#question2
yelp=read.csv("aggregate.csv",header = T)#load the data
yelp <- subset(yelp, select = -business_id)
yelp <- subset(yelp, select = -name)
yelp <- subset(yelp, select = -demerit)
yelp <- subset(yelp, select = -address)
yelp <- subset(yelp, select = -grade)

cor_matrix=cor(yelp)
yelp_zscore=scale(yelp, center = TRUE, scale = TRUE)

yelp_zscore=data.frame(yelp_zscore)
model=lm(inspection_fail_count~.-inspection_fail_indicator,data=yelp_zscore)
summary(model)

yelp <- subset(yelp, select = -infest)
yelp <- subset(yelp, select = -e.coli)
yelp <- subset(yelp, select = -salmonella)
yelp <- subset(yelp, select = -contaminate)
yelp <- subset(yelp, select = -expired)
yelp <- subset(yelp, select = -mouse)
yelp <- subset(yelp, select = -the.runs)
yelp <- subset(yelp, select = -throw.up)
yelp <- subset(yelp, select = -gross)
yelp <- subset(yelp, select = -filthy)
cor_matrix=cor(yelp)
pairs(yelp)#draw scatter

#perform a logistic regression
model=glm(inspection_fail_indicator~.-inspection_fail_count,data=yelp_zscore)
summary(model)
train=yelp[1:floor(nrow(yelp)*0.8),]#get the train set
test=yelp[(floor(nrow(yelp)*0.8)+1):nrow(yelp),]#get the test set
#perform a logistic regression using the validation set approach
glmModel=glm(yelp_zscore$inspection_fail_indicator~.,data=yelp_zscore)
summary(glmModel)
#do the prediction
probs=predict(glmModel,yelp_zscore)
#if possiblity>0.5, the result is supposed to be 1
prediction=rep("0",nrow(yelp_zscore))
prediction[probs >.5]="1"
table(prediction,yelp_zscore$inspection_fail_indicator)
#calculate the error
mean(prediction==test$inspection_fail_indicator)
1-mean(prediction==test$inspection_fail_indicator)

lmModel=lm(inspection_fail_count~.-inspection_fail_indicator,data=yelp)
summary(lmModel)

library(MASS)
ldaModel=lda(inspection_fail_indicator~.-inspection_fail_count,data=train)
ldaModel
ldaPrediction=predict(ldaModel,test)
ldaResult=ldaPrediction$class
table(ldaResult,test$inspection_fail_indicator)
1-mean(ldaResult==test$inspection_fail_indicator)

#perform QDA
qdaModel=qda(inspection_fail_indicator~.-inspection_fail_count,data=train)
qdaModel
qdaPrediction=predict(qdaModel,test)
qdaResult=qdaPrediction$class
table(qdaResult,test$inspection_fail_indicator)
1-mean(qdaResult==test$inspection_fail_indicator)


library(leaps)
regfit.fwd=regsubsets(inspection_fail_indicator~.-inspection_fail_count,data = yelp_zscore,method ="forward")
regSumFwd=summary(regfit.fwd)
regfit.bwd=regsubsets(inspection_fail_indicator~.-inspection_fail_count,data = yelp,method ="backward")
regSumBwd=summary(regfit.bwd)
plot(regSumFwd$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(regSumFwd$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")
which.max(regSumFwd$adjr2)
plot(regSumBwd$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(regSumBwd$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")
which.max(regSumBwd$adjr2)

#The lasso
library(glmnet)
xAll=model.matrix(inspection_fail_indicator~.,yelp_zscore)[,-1]#create the X matrix
inspection_fail_indicator=yelp_zscore$inspection_fail_indicator;
set.seed(1)
grid=10^seq(2,-2,length=1000)#decide the range of lambda
cv.out=cv.glmnet(xAll,inspection_fail_indicator,alpha=1,lambda=grid)
plot(cv.out)#plot the MSE to Lambda
bestlam=cv.out$lambda.min#get the best Lambda
lasso.mod=glmnet(xAll,inspection_fail_indicator,alpha=1,lambda=bestlam)
lasso.pred=predict(lasso.mod,s=bestlam ,newx=xAll)#Using lasso result to predict
coefficients<-coef(lasso.mod,s=cv.out$lambda.min)
mean((lasso.pred-Y)^2)#calculate the MSE
