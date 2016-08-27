#SVM
#pre-process data split it into x and y
set.seed(1)
x = as.matrix(newdata.red[,1:ncol(newdata.red)-1])
dat = data.frame(x, y = as.factor(newdata.red[,ncol(newdata.red)]))
train = sample(1:nrow(newdata.red), 0.7*nrow(newdata.red))

attach(dat)

#use cross-validation to find best parameter by using linear kernel
#corss validation

tune.out=tune(svm,y~.,data=dat[train,],kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))

#observing from the best parameter is cost =5
summary(tune.out)

bestmod=tune.out$best.model

#summary of best model there are 946 support vectors
summary(bestmod)

#
#build svm model
svmfit=svm(y~., data=dat[train,], kernel="linear", cost=100,scale=FALSE)
plot(svmfit, dat[train,], fixed.acidity~volatile.acidity)

#build test data
test = dat[-train,]
ypred=predict(bestmod,test)
table(predict=ypred, truth=test$y)

#correctly classified
(162+122)/472
#0.55

#non-linear model
#cross-validation
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
bestmod=tune.out$best.model

#plot best model
plot(bestmod, dat[train,], fixed.acidity~volatile.acidity)

#best model with cost =1, gamma 0.5
summary(tune.out)
summary(bestmod)

#predict model
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newdata=dat[-train,]))

#accuracy
(139+151+27)/472
#0.62 a little bit improved

#white
#SVM
#pre-process data split it into x and y
newdata.white = winequality.white[winequality.white$quality>3 & winequality.white$quality<9,]
set.seed(1)
x = as.matrix(newdata.white[,1:ncol(newdata.white)-1])
dat = data.frame(x, y = as.factor(newdata.white[,ncol(newdata.white)]))
train = sample(1:nrow(newdata.white), 0.7*nrow(newdata.white))

attach(dat)

#use cross-validation to find best parameter by using linear kernel
#corss validation

tune.out=tune(svm,y~.,data=dat[train,],kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))

#observing from the best parameter is cost =5
summary(tune.out)

bestmod=tune.out$best.model

#summary of best model there are 946 support vectors
summary(bestmod)

#
#build svm model
svmfit=svm(y~., data=dat[train,], kernel="linear", cost=0.1,scale=FALSE)
plot(svmfit, dat[train,], fixed.acidity~volatile.acidity)

#build test data
test = dat[-train,]
ypred=predict(bestmod,test)
table(predict=ypred, truth=test$y)

#correctly classified
(230+527)/1462
#0.55

#non-linear model
#cross-validation
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
bestmod=tune.out$best.model

#plot best model
plot(bestmod, dat[train,], fixed.acidity~volatile.acidity)

#best model with cost =1, gamma 0.5
summary(tune.out)
summary(bestmod)

#predict model
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newdata=dat[-train,]))

#accuracy
(4+270+527+135+20)/1462
#0.62 a little bit improved


