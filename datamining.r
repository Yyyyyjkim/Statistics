library(tidyverse)
library(readr)

train <-read_csv("../data/train.csv")
test <- read_csv("../data/test.csv")

class(train)
mode(train)
typeof(train)
summary(train)

# summarize

train %>% select(Pclass,Survived) %>% 
  group_by(Pclass) %>%
  summarize_all(mean) %>%
  arrange(desc(Survived))

train %>% select(Sex,Survived) %>% 
  group_by(Sex) %>%
  summarize_all(mean)

train %>% select(SibSp,Survived) %>% 
  group_by(SibSp) %>%
  summarize(Survived = mean(Survived)) %>%
  arrange(desc(Survived))


train %>% select(Parch,Survived) %>% 
  group_by(Parch) %>%
  summarize(Survived = mean(Survived)) %>%
  arrange(desc(Survived))

# Analyze by visualizing data

# age0 <- ggplot(train %>% filter(Survived==0)) + geom_histogram(aes(Age),bins=20)
age1 <- ggplot(train %>% filter(Survived==1)) + geom_histogram(aes(Age),bins=20)

library(gridExtra)
grid.arrange(nrow=1,ncol=2)

head(train)

ggplot(train,aes(y=Age,x=Embarked,col=Embarked)) + geom_boxplot() + facet_grid(~Sex)
ggplot(train,aes(x=Embarked,fill=Sex)) + geom_bar(position='dodge')
ggplot(train,aes(x=Pclass,fill=Embarked)) + geom_bar(position='fill')

ggplot(train,aes(x=Age,y=Fare,col=Sex)) + geom_line() + facet_grid(~Survived)
ggplot(train,aes(x=Age,fill=Sex)) + geom_histogram(position='stack',binwidth=5) + facet_grid(Pclass~Survived)

ggplot(train,aes(x=Pclass,y=Sex,fill=Age)) + geom_tile()
heatmap(as.data.frame(table(train$Pclass,train$Survived)))

ggplot(train, aes(x=Age)) + geom_histogram(aes(y=stat(count)/sum(count)),bins=20) + geom_line(stat="density",col="red")+ facet_grid(~Survived)

ggplot(train,aes(x=Age)) + geom_histogram(aes(y=stat(..count..)/sum(..count..)),bins=20) + geom_line(stat="density",col='red') + facet_grid(Pclass~Survived)
# ggplot(train,aes(x=Age)) + geom_histogram(bins=20) + facet_wrap(~Pclass,nrow=3)

ggplot(train) + geom_point(aes(x=Pclass,y=mean(Survived),col=Survived)) + facet_grid(~Embarked)


ggplot(train) + geom_histogram(aes(x=Sex,y=Fare)) + facet_grid(Embarked~Survived)

#  Wrangle Data

dim(train)
dim(test)

head(train)
head(subset(train,select=-c(Ticket,Cabin)))

train = train %>% select(-Ticket,-Cabin,-PassengerId)
test = test %>% select(-Ticket, -Cabin)

train %>%
  arrange(desc(Age),Pclass) 

table(train$Sex,train$Pclass)
dat <- '19950423'
as.Date(dat,format="%Y%m%d")
as.numeric(format(str_sub(dat,1,4),format='%Y'))
day <- '2020년06월05일03시46분05초'
a <- strptime(day,format='%Y년%m월%d일%H시%M분%S초')
str_extract(day,pattern='^[ㄱ-힣0-9]+(일)')
as.numeric(factor(weekdays(a),levels=c("월요일","화요일","수요일","목요일","금요일","토요일","일요일")))

head(train)

dim(train)
dim(test)

library(rex)
train_a = train %>% 
  mutate(Name=str_extract(Name,regex("[A-Za-z]+[.]"))) %>%
  mutate(Name=str_replace(Name,pattern=c('Lady|Countess|Capt|Col|Don|Dr|
                                         Major|Rev|Sir|Jonkheer|Dona'),'Rare')) %>%
  mutate(Name=str_replace(Name,'Mlle','Miss')) %>%
  mutate(Name=str_replace(Name,'Ms','Miss')) %>%
  mutate(Name=str_replace(Name,'Mme','Mrs')) %>%
  mutate(Name=as.numeric(as.factor(Name))) %>%
  mutate(Sex=as.numeric(as.factor(Sex)))

test = test %>%
  mutate(Name=str_extract(Name,regex("[A-Za-z]+[.]"))) %>%
  mutate(Name=str_replace(Name,pattern=c('Lady|Countess|Capt|Col|Don|Dr|
                                         Major|Rev|Sir|Jonkheer|Dona'),'Rare')) %>%
  mutate(Name=str_replace(Name,'Mlle','Miss')) %>%
  mutate(Name=str_replace(Name,'Ms','Miss')) %>%
  mutate(Name=str_replace(Name,'Mme','Mrs')) %>%
  mutate(Name=as.factor(Name)) %>%
  mutate(Sex=as.factor(Sex))

table(train$Name, train$Sex)
table(test$Name, test$Sex)

ggplot(train,aes(x=Age)) + geom_histogram(bins=20) + facet_grid(Pclass~Survived)

train <-read_csv("../data/train.csv")
train = train %>% select(-Ticket,-Cabin,-PassengerId)

train = train %>%
  mutate(Sex = as.numeric(as.factor(Sex)), Pclass = as.numeric(as.factor(Pclass))) 

guess_age = matrix(0,nrow=2,ncol=3)
for (i in 1:2) {
  for (j in 1:3) {
    age = median(train$Age[(train$Sex==i)&(train$Pclass==j)],na.rm=TRUE)
    train$Age[is.na(train$Age)&(train$Sex==i)&(train$Pclass==j)] = age
    guess_age[i,j] = age
  }
}

table(train$Embarked)
train$Embarked[is.na(train$Embarked)] = "S"

bins_age <- seq(min(train$Age),max(train$Age)+1,length.out=6)
train$Age = cut(train$Age, breaks = bins_age, right = FALSE, labels = c(1,2,3,4,5))

bins_fare <- quantile(as.numeric(train$Fare),probs=seq(0,1,0.2))
bins_fare[length(bins_fare)] = bins_fare[length(bins_fare)]+1
train$Fare = cut(train$Fare, breaks=bins_fare, right=FALSE, labels=c(1,2,3,4,5))

train %>% group_by(Age) %>%
  summarize(mean(Survived))

train %>% group_by(Fare) %>%
  summarize(mean(Survived))

train = train %>% 
  mutate(Name=str_extract(Name,regex("[A-Za-z]+[.]"))) %>%
  mutate(Name=str_replace(Name,pattern=c('Lady|Countess|Capt|Col|Don|Dr|
                            Major|Rev|Sir|Jonkheer|Dona'),'Rare')) %>%
  mutate(Name=str_replace(Name,'Mlle','Miss')) %>%
  mutate(Name=str_replace(Name,'Ms','Miss')) %>%
  mutate(Name=str_replace(Name,'Mme','Mrs')) %>%
  mutate(Name=as.numeric(as.factor(Name))) %>%
  mutate(family = SibSp+Parch+1) %>%
  mutate(isalone = as.numeric(family==1)) %>%
  mutate(ageclass = as.numeric(Age)*Pclass) %>%
  mutate(Embarked = as.factor(Embarked)) %>%
  select(-SibSp,-Parch,-family)


test <-read_csv("../data/test.csv")
test = test %>% select(-Ticket,-Cabin,-PassengerId)

test = test %>%
  mutate(Sex = as.numeric(as.factor(Sex)), Pclass = as.numeric(as.factor(Pclass))) 

guess_age
for (i in 1:2) {
  for (j in 1:3) {
    test$Age[is.na(test$Age)&(test$Sex==i)&(test$Pclass==j)] = guess_age[i,j]
  }
}

test$Embarked[is.na(test$Embarked)] = "S"

test$Age[test$Age <= bins_age[2]] <- 1
test$Age[(test$Age > bins_age[2])&(test$Age <= bins_age[3])] <- 2
test$Age[(test$Age > bins_age[3])&(test$Age <= bins_age[4])] <- 3
test$Age[(test$Age > bins_age[4])&(test$Age <= bins_age[5])] <- 4
test$Age[test$Age > bins_age[5]] <- 5
table(test$Age)

test$Fare[test$Fare <= bins_fare[2]] <- 1
test$Fare[(test$Fare > bins_fare[2])&(test$Fare <= bins_fare[3])] <- 2
test$Fare[(test$Fare > bins_fare[3])&(test$Fare <= bins_fare[4])] <- 3
test$Fare[(test$Fare > bins_fare[4])&(test$Fare <= bins_fare[5])] <- 4
test$Fare[test$Fare > bins_fare[5]] <- 5
table(test$Fare)

test = test %>% 
  mutate(Name=str_extract(Name,regex("[A-Za-z]+[.]"))) %>%
  mutate(Name=str_replace(Name,pattern=c('Lady|Countess|Capt|Col|Don|Dr|
                                         Major|Rev|Sir|Jonkheer|Dona'),'Rare')) %>%
  mutate(Name=str_replace(Name,'Mlle','Miss')) %>%
  mutate(Name=str_replace(Name,'Ms','Miss')) %>%
  mutate(Name=str_replace(Name,'Mme','Mrs')) %>%
  mutate(Name=as.numeric(as.factor(Name))) %>%
  mutate(Age = as.factor(Age), Fare = as.factor(Fare)) %>%
  mutate(family = SibSp+Parch+1) %>%
  mutate(isalone = as.numeric(family==1)) %>%
  mutate(ageclass = as.numeric(Age)*Pclass) %>%
  mutate(Embarked = as.factor(Embarked)) %>%
  select(-SibSp,-Parch,-family)

test = na.omit(test)

# logistic

idx = base::sample(seq(nrow(train)),0.7*nrow(train),replace=TRUE)
train_set = train[idx,]
test_set = train[-idx,]

logistic_fit = glm(Survived ~ ., data=train_set, family="binomial")
summary(logistic_fit)
confint(logistic_fit)
confint.default(logistic_fit)
exp(coef(logistic_fit))
pred = predict(logistic_fit,test_set)
pred[pred <= 0.5] = 0
pred[pred > 0.5] = 1
sum(diag(table(test_set$Survived,pred)))/nrow(test_set)

k = 5
folds = base::sample(seq(1,k),nrow(train),replace=TRUE)
folds_pred = matrix(nrow=nrow(train),ncol=k)
accuracy = c()

for (i in 1:k) {
  train_set = train[folds!=i,]
  test_set = train[folds==i,]
  
  logistic_fit = glm(Survived ~ ., data=train_set, family="binomial")
  pred = predict(logistic_fit,test_set)
  pred[pred <= 0.5] = 0
  pred[pred > 0.5] = 1
  accuracy[i] = sum(diag(table(test_set$Survived,pred)))/nrow(test_set)
  folds_pred[folds==i,i] = pred
}

folds_pred[is.na(folds_pred)] <- 0
apply(folds_pred,1,sum)

# svm

library(e1071)

idx = base::sample(seq(nrow(train)),0.7*nrow(train),replace=TRUE)
train_set = train[idx,]
test_set = train[-idx,]

cost.weights <- c(0.1,10,100)
gamma.weights <- c(0.1, 0.25, 0.5, 1)
svm_tune = tune.svm(as.factor(Survived) ~., data=train, kernel='radial', cost=cost.weights, gamma=gamma.weights)
summary(svm_tune)
svm_tune$best.parameters

svm_fit = svm(Survived~.,train_set,type="C-classification",kernel="radial", cost=10, gamma=0.1)
pred = predict(svm_fit,test_set)
sum(diag(table(test_set$Survived,pred)))/nrow(test_set)

k = 5
folds = base::sample(seq(1,k),nrow(train),replace=TRUE)
folds_pred = matrix(nrow=nrow(train),ncol=k)
accuracy = c()

for (i in 1:k) {
  train_set = train[folds!=i,]
  test_set = train[folds==i,]
  
  svm_fit = svm(Survived~.,train_set,type="C-classification",kernel="radial", cost=10, gamma=0.1)
  pred = predict(svm_fit,test_set)
  accuracy[i] = sum(diag(table(test_set$Survived,pred)))/nrow(test_set)
  folds_pred[folds==i,i] = pred
}

folds_pred[is.na(folds_pred)] <- 0
apply(folds_pred,1,sum)

# knn
library(class)

train_knn = as.data.frame(apply(train,2,as.factor))

idx = base::sample(seq(nrow(train)),0.7*nrow(train),replace=TRUE)
train_x = train_knn[idx,-1]
train_y = train_knn[idx,1]
test_x = train_knn[-idx,-1]
test_y = train_knn[-idx,1]

knn_fit <- knn(train=train_x, test=test_x, cl = train_y, k = 1)

# naive bayes

library(e1071)

train_nb = as.data.frame(apply(train,2,as.factor))

idx = base::sample(seq(nrow(train)),0.7*nrow(train),replace=TRUE)
train_x = train_nb[idx,-1]
train_y = train_nb[idx,1]
test_x = train_nb[-idx,-1]
test_y = train_nb[-idx,1]

nb_fit = naiveBayes(train_x, train_y)
summary(nb_fit)
nb_fit

pred = predict(nb_fit,test_x)
sum(diag(table(test_y,pred)))/length(test_y)

# tree

library(caret)
library(rpart)

idx = base::sample(seq(nrow(train)),0.7*nrow(train),replace=TRUE)
train_set = train[idx,]
test_set = train[-idx,]

tree_fit <- rpart(Survived~., train_set, method="class")
plot(tree_fit)
text(tree_fit)

pred = predict(tree_fit, test_set)
pred[pred <= 0.5] <- 0
pred[pred > 0.5] <- 1
table(test_set$Survived,pred[,2])

# random forest

library(randomForest)

idx = base::sample(seq(nrow(train)),0.7*nrow(train),replace=TRUE)
train_set = train[idx,]
test_set = train[-idx,]

rf_fit = randomForest(as.factor(Survived)~., data=train_set, mtry = floor(sqrt(7)), ntree = 500, importance = T)
importance(rf_fit)
pred = predict(rf_fit,test_set)
sum(diag(table(test_set$Survived, pred)))/nrow(test_set)

mtry_grid = (1:5)
nodesize_grid = (3:10)
rf_tune = tune.randomForest(as.factor(Survived)~., data=train_set, mtry = mtry_grid, 
                            nodesize = nodesize_grid, ntree = 500, importance = T)
mtry_grid <- seq(1,10,1)
ntree_grid <- seq(100,1000,100)
c <- tune.randomForest(as.factor(Survived~.,train_set,mtry=mtry_grid,ntree=ntree_grid))


rf_tune$best.parameters
pred <- predict(rf_tune$best.model,test_set)
sum(diag(table(test_set$Survived, pred)))/nrow(test_set)


# stacking

library(caret)
idx <- createDataPartition(train$Survived, p=0.7, list=FALSE)
train_set <- train[idx,]
test_set <- train[-idx,]

fit_control <- trainControl(method='repeatedcv',number=5,repeats=10)
rf_fit <- train(as.factor(Survived)~.,data=train_set,method='rf',ntree=100,trControl=fit_control,metric='Accuracy')
rf_fit

predict(rf_fit,test_set) %>%
  confusionMatrix(as.factor(test_set$Survived))

knn_fit <- train(as.factor(Survived)~., data=train_set, method='knn', trControl=trainControl(method='cv',number=5),metric='Accuracy')

predict(knn_fit,test_set) %>%
  confusionMatrix(as.factor(test_set$Survived))

svm_fit <- train(as.factor(Survived)~., data=train_set, method='svmRadial',metric='Accuracy')
predict(svm_fit,test_set) %>%
  confusionMatrix(as.factor(test_set$Survived))

xg_fit <- train(as.factor(Survived)~., data=train_set, method='xgbTree', metric='Accuracy')
