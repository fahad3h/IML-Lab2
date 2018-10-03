library(readxl)
library(tree)
library(e1071)
library(MASS)

# Importing Data - 2.1
data <- read_excel("creditscoring.xls")
data <- as.data.frame(data)
data$good_bad <- as.factor(data$good_bad)
# Dividing Data into three Train(50%) Test(25%) Validation(25%)
n = nrow(data)
set.seed(12345)

n=dim(data)[1]
set.seed(12345)
# 50% Training Data
id=sample(1:n, floor(n*0.5))
train=data[id,]
#train$good_bad <- as.factor(train$good_bad)

# 25% validation & testing Data
Sub_id = data[-id,]
m = dim(Sub_id)[1]
part1 = sample(1:m, floor(m*0.5))
validation = Sub_id[part1,]
testing = Sub_id[-part1,]

# Fitting data using Deviance and gini - 2.2
tree_deviance = tree(as.factor(good_bad) ~ ., data = train, split = "deviance")
tree_gini = tree(as.factor(good_bad) ~ ., data = train, split = "gini")

# Prediction
devi_yfit = predict(tree_deviance, newdata = testing,type="class")
gini_yfit = predict(tree_gini, newdata = testing,type="class")
plot(tree_deviance)
plot(tree_gini)

devi_table = table(devi_yfit,testing$good_bad)
geni_table = table(devi_yfit,testing$good_bad)

devi_table
# Missclassification rate Deviance
missclass_devi <- 1-sum(diag(devi_table))/sum(devi_table)

geni_table
# Missclassification rate Gigi
missclass_geni <- 1-sum(diag(geni_table))/sum(geni_table)

index = summary(tree_deviance)[4]$size
trainScore = rep(0,index)
testScore = rep(0,index)

# Graph training and validation
for(i in 2:index) {
  prunedTree=prune.tree(tree_deviance,best=i)
  pred=predict(prunedTree, newdata=validation,type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}

plot(2:index,trainScore[2:index], col="Red",type = "b", ylim=c(min(testScore[2:index]),max(trainScore)))
points(2:index,testScore[2:index],col="Blue",type="b")

# misclassification rate for test data
missclass_test_t = prune.tree(tree_deviance, best = 4)
yfit = predict(missclass_test_t, newdata = validation, type="class")
valid_ = table(validation$good_bad,yfit)
valid_
mc <- 1-sum(diag(valid_))/sum(valid_)
plot(missclass_test_t)


# Naïve Bayes 2.4
naye = naiveBayes(good_bad ~., data=train)

nav_test = predict(naye, newdata = testing, type = "class")
nav_train = predict(naye,newdata = train[,-ncol(train)])

# Confusion Matrix Using Naive Bayes
naive_table = table(nav_test,testing$good_bad)
print(naive_table)
naive_table_train <- table(nav_train,train$good_bad)
print(naive_table_train)

# Missclassification train data value Using Naive Bayes
mc_nav_train <- 1-sum(diag(naive_table_train))/sum(naive_table_train)
mc_nav_train

# Missclassification test data value Using Naive Bayes
mc_nav_test <- 1-sum(diag(naive_table))/sum(naive_table)
mc_nav_test  

# Naive Bayes With loss matrix 2.5
naye = naiveBayes(good_bad ~ ., data = train)

# Predicting using Naive
nav_test = predict(naye, testing[,-ncol(testing)] , type="raw")
nav_train = predict(naye, train[,-ncol(train)] , type="raw")

# applying loss matrix if greater then 10 True else False
nav_test =  (nav_test[, 2] / nav_test[, 1]) > 10
nav_train =  (nav_train[, 2] / nav_train[, 1]) > 10

# confusion matrix for train & test
naive_table = table(nav_test , testing$good_bad)
naive_table_train = table(nav_train , train$good_bad)

# missclasification for train & test
naive_table_train
1-sum(diag(naive_table_train))/sum(naive_table_train)

naive_table
1-sum(diag(naive_table))/sum(naive_table)

