
data <- read.csv("banknotes.csv")
View(data)
typeof(data)

df <- as.data.frame(data)
RNGversion("3.5.2")
set.seed(321)
rnd_ind <- sample(1372,1050)
train <- df[rnd_ind,]
test <- df[-rnd_ind,]
prop.table(table(train$class))
prop.table(table(test$class))

library(C50)
train$class<-as.factor(train$class)
model <- C5.0(train[-5], train$class)
model


pred <- predict.C5.0(model,test)
library(gmodels)
CrossTable(test$class,pred,prop.chisq = FALSE,prop.c = FALSE,prop.r=FALSE,dnn= c('actual classes', 'predicted classes'))

plot(model)
model_boost <- C5.0(train[-5], train$class, trails = 2)
pred_boost <- predict.C5.0(model_boost,test)
CrossTable(test$class,pred_boost,prop.chisq = FALSE,prop.c = FALSE,prop.r=FALSE,dnn= c('actual classes', 'predicted classes'))

set.seed(42)
rows <- sample(nrow(train))
rows2 <- sample(nrow(test))
train <- train[rows,]
test <- test[rows2,]
model <- C5.0(train[-5], train$class)
pred <- predict.C5.0(model,test)
CrossTable(test$class,pred,prop.chisq = FALSE,prop.c = FALSE,prop.r=FALSE,dnn= c('actual classes', 'predicted classes'))

error <- matrix(c(0,1,8,0), nrow = 2)
model_boost_err <- C5.0(train[-5], train$class, trails = 2, costs = error)
pred_boost_err <- predict.C5.0(model_boost_err,test)
CrossTable(test$class,pred_boost_err,prop.chisq = FALSE,prop.c = FALSE,prop.r=FALSE,dnn= c('actual classes', 'predicted classes'))

model_err <- C5.0(train[-5], train$class, costs = error)
pred_err <- predict.C5.0(model_err,test)
CrossTable(test$class,pred_err,prop.chisq = FALSE,prop.c = FALSE,prop.r=FALSE,dnn= c('actual classes', 'predicted classes'))

error2 <- matrix(c(0,2,1,0), nrow = 2)
model_err2 <- C5.0(train[-5], train$class, costs = error2)
pred_err2 <- predict.C5.0(model_err2,test)
CrossTable(test$class,pred_err2,prop.chisq = FALSE,prop.c = FALSE,prop.r=FALSE,dnn= c('actual classes', 'predicted classes'))

rnd_ind2 <- sample(1372,900)
train2 <- df[rnd_ind2,]
test2 <- df[-rnd_ind2,]
train2$class<-as.factor(train2$class)
model2 <- C5.0(train2[-5], train2$class)
pred2 <- predict.C5.0(model2,test2)
CrossTable(test2$class,pred2,prop.chisq = FALSE,prop.c = FALSE,prop.r=FALSE,dnn= c('actual classes', 'predicted classes'))

nor <- function(x){return (x - min(x)/(max(x) - min(x)))}
library(class)
mod_knn <- knn(train = train, test = test, cl = train$class, k = 4)
CrossTable(x = test$class, y = mod_knn, prop.chisq = FALSE)
mod_knn_2 <- knn(train = train2, test = test2, cl = train2$class, k = 5)
CrossTable(x = test2$class, y = mod_knn_2, prop.chisq = FALSE)
