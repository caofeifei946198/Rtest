#read file
indian <- read.table("indian", header = TRUE, sep = ",")
#data explore
#list to display NA
library(mice)
md.pattern(indian)
#graph explore NA data aggr()
library(VIM)
aggr(indian, prop=T, numbers = T)
#data complete
library(missForest)#missforest replace
z = missForest(indian)
indian.mf = z$ximp
md.pattern(indian.mf)#list validation
aggr(indian.mf, prop=T, numbers = T)#graph validation
#regression complete
indian.lm <- indian
#complete glucose
ind <- which(is.na(indian.lm[,2])==T)#return row 2 is NA
data_NL <- indian.lm[-ind,]#receive 2 is not NA of data
data_NA <- indian.lm[ind,]#receive 2 is NA of data
#build regression model
lm = lm(glucose~preg.nt+pedigree+age,data=data_NL)
summary(lm)#see lm significant
lm = lm(glucose~pedigree+age,data=data_NL)#rebuild the regression model
indian.lm[ind,2] = round(predict(lm, data_NA))#comlete data
#complete pressure
ind <- which(is.na(indian.lm[,3])==T)
data_NL <- indian.lm[-ind,]
data_NA <- indian.lm[ind,]
#build regression model
lm = lm(pressure~preg.nt+pedigree+age,data=data_NL)
summary(lm)
lm = lm(pressure~age,data=data_NL)
indian.lm[ind,3] = round(predict(lm, data_NA))
#complete triceps
ind <- which(is.na(indian.lm[,4])==T)
data_NL <- indian.lm[-ind,]
data_NA <- indian.lm[ind,]
#build regression model
lm = lm(triceps~preg.nt+pedigree+age,data=data_NL)
summary(lm)
lm = lm(triceps~pedigree+age,data=data_NL)
indian.lm[ind,4] = round(predict(lm, data_NA))
#complete insulin
ind <- which(is.na(indian.lm[,5])==T)
data_NL <- indian.lm[-ind,]
data_NA <- indian.lm[ind,]
#build regression model
lm = lm(insulin~preg.nt+pedigree+age,data=data_NL)
summary(lm)
lm = lm(insulin~preg.nt+pedigree+age,data=data_NL)
indian.lm[ind,5] = round(predict(lm, data_NA))
#complete mass
ind <- which(is.na(indian.lm[,6])==T)
data_NL <- indian.lm[-ind,]
data_NA <- indian.lm[ind,]
#build regression model
lm = lm(mass~preg.nt+pedigree+age,data=data_NL)
summary(lm)
lm = lm(insulin~pedigree,data=data_NL)
indian.lm[ind,6] = round(predict(lm, data_NA))
#list validation 
md.pattern(indian.lm)
aggr(indian.lm, prop=T, numbers = T)#graph validation





#data divide and build model
#data divide
library(caret)
#build result to cave 
result <-  data.frame(model = c("naiveBayes", "c5.0",
        "CART", "ctree", "bagging", "boosting", 
        "随机森林"), errTrain_lm=rep(0,7), 
        errTest_lm=rep(0,7), errTrain_rf=rep(0,7), 
        errTest_rf=rep(0,7))
#build result.10 to save (10 crossvalidation)
result.10 <- data.frame(model = c("决策树", "随机森林", 
          "人工神经网络"), errTrain_lm=rep(0,3), 
          errTest_lm=rep(0,3), errTrain_rf=rep(0,3), 
          errTest_rf=rep(0,3))
#build 10 crossvalidation
library(rpart)
control =trainControl(method = "repeatedcv", number = 
        10, repeats = 3)
rpart.model.mf <- train(diabetes~., data = indian.mf,
      method = "rpart", trControl = control)
rpart.model.lm <- train(diabetes~., data = indian.lm,
              method = "rpart", trControl = control)
#missforest
rf.model.mf <- train(diabetes~., data = indian.mf,
                method = "rf", trControl = control)
rf.model.lm <- train(diabetes~., data = indian.lm,
                     method = "rf", trControl = control)

#netral network
nnet.model.mf <- train(diabetes~., data=indian.mf,
              method = "nnet", trControl = control)
nnet.model.lm <- train(diabetes~., data=indian.lm,
                       method = "nnet", trControl = control)


for(a in 1:2)
{
  #use mf and lm to build model
  ind <- createDataPartition(switch(a, indian.lm$diabetes,
          indian.mf$diabetes), times = 1, p=0.8, list = F)
  train <- indian.lm[ind,]#build train set
  test <- indian.lm[-ind,]#build test set
  
  prop.table(table(indian.lm$diabetes))
  prop.table(table(train$diabetes))
  prop.table(table(test$diabetes))
}




#see the result
#build midel and evaluate
#use naivebayes to build naiveBayes classifier
library(e1071)
str(indian.lm)
naiveBayes.model <- naiveBayes(diabetes~., data = train)#buile model
#predict result
train_predict <- predict(naiveBayes.model, newdata = train)
test_predict <- predict(naiveBayes.model, newdata = test)
#build matrix

tableTrain <- table(actual=train$diabetes,predict=train_predict)
tableTest <- table(actual=test$diabetes,predict=test_predict)

#calculate error rate
errTrain <- paste0(round((sum(tableTrain)-
          sum(diag(tableTrain)))*100/sum(tableTrain),2),"%")
errTest <- paste0(round((sum(tableTest)-
                            sum(diag(tableTest)))*100/sum(tableTest),2),"%")
#decesion tree model
library(C50)
C5.0.model <- C5.0(diabetes~., data = train)
library(rpart)
rpart.model <- rpart(diabetes~., data = train)
library(party)
ctree.model <- ctree(diabetes~., data = train)
result[1,switch(a,2,4)] <- errTrain
result[1,switch(a,3,5)] <- errTest
for (i in 1:3)#predict result
{
  train_predict <- predict(switch(i, C5.0.model, rpart.model,
                  ctree.model), newdata=train,type = switch(i,
                  "class", "class", "response"))
  test_predict <- predict(switch(i, C5.0.model,rpart.model,
                  ctree.model), newdata=test, type=switch(i,
                  "class", "class", "response"))

#build matrix
tableTrain <- table(actual=train$diabetes,predict=train_predict)
tableTest <- table(actual=test$diabetes,predict=test_predict)
#calculate error rate
result[i+1,switch(a,2,4)] <- paste0(round((sum(tableTrain)-
                                sum(diag(tableTrain)))*100/sum(tableTrain)
                                ,2),"%")
result[i+1,switch(a,3,5)] <- paste0(round((sum(tableTest)-
                                            sum(diag(tableTest)))
                                          *100/sum(tableTest)
                                         ,2),"%")
}
#use randomforest to build model
library(adabag)
bagging.model <- bagging(diabetes~., data=train)
boosting.model <- boosting(diabetes~., data=train)
library(randomForest)
randomForest.model <- randomForest(diabetes~., data=train)
for (i in 1:3)#predict result
{
  train_predict <- predict(switch(i, bagging.model, boosting.model,
                  randomForest.model), newdata=train)
  test_predict <- predict(switch(i, bagging.model, boosting.model,
                                   randomForest.model), newdata=test)
    #build matrix
  tableTrain <- table(actual=train$diabetes,predict=switch(i,train_predict$class,
                                          train_predict$class, train_predict))
  tableTest <- table(actual=test$diabetes,predict=switch(i,test_predict$class,
                        test_predict$class, test_predict))
  #calculate error rate
  result[i+1,switch(a,2,4)] <- paste0(round((sum(tableTrain)-
                                               sum(diag(tableTrain)))*100/sum(tableTrain)
                                            ,2),"%")
  result[i+1,switch(a,3,5)] <- paste0(round((sum(tableTest)-
                                               sum(diag(tableTest)))
                                            *100/sum(tableTest)
                                            ,2),"%")
}
  #through 10 crossvalidation to select best parameters
  #library(caret)
  #use rpart to build classtree
  rpart.model <- rpart::rpart(diabetes~., data = 
      train, control = c(op=switch(a, 0.01741294, 0.02985075)))
  #use randomforest to build randomforest
  rf.model <- randomForest::randomForest(diabetes~., data=train,
                  ntry = switch(a,2,8))
  #use nnet to build nnet
  nnet.model <- nnet::nnet(diabetes~., data=train,
        size=switch(a,5,3), decay=0.1)
for (i in 1:3)
{
  #predict result
  train_predict <- predict(switch(i, rpart.model, rf.model,
                                  nnet.model), newdata=train, type="class")
  test_predict <- predict(switch(i, rpart.model, rf.model,
                                 nnet.model), newdata=test, type="class")
  #build matrix
  tableTrain <- table(actual=train$diabetes,predict=train_predict)
  tableTest <- table(actual=test$diabetes,predict=test_predict)
  #calculate error rate
  result.10[i+1,switch(a,2,4)] <- paste0(round((sum(tableTrain)-
                                               sum(diag(tableTrain)))*100/sum(tableTrain)
                                            ,2),"%")
  result.10[i+1,switch(a,3,5)] <- paste0(round((sum(tableTest)-
                                               sum(diag(tableTest)))
                                            *100/sum(tableTest)
                                            ,2),"%")
}
