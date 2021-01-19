library(reshape2)
library(plyr)
library(skimr)
library(class)
library(rpart)
library(rpart.plot)
library(caret)

data_main <- read.csv("student_#########data.csv")

# Create boxplot of Class vs Scores

data_melt <- melt(data_main, measure.vars = 6:10)

p1 <- ggplot(data_melt, aes(x = factor(round_any(Class,0.5)), y=value,fill=variable)) +
  geom_boxplot() +
  facet_grid(.~variable) +
  ylab("Score") +
  xlab("Class")
p1

data_melt_2 <- melt(data_main, measure.vars = 11:12)

p2 <- ggplot(data_melt_2, aes(x = factor(round_any(Class,0.5)), y=value,fill=variable)) +
  geom_boxplot() +
  facet_grid(.~variable) +
  xlab("Class") +
  ylab("Scores")

p2

# K-nearest neighbours

# Split data
data_main[, c(1:4, 13)] <- lapply(data_main[, c(1:4, 13)], factor)
set.seed(31)
n <- nrow(data_main)
ind1 <- sample(c(1:n),round(n/2))
ind2 <- sample(c(1:n)[-ind1],round(n/4))
ind3 <- setdiff(c(1:n),c(ind1,ind2))
train.data <- data_main[ind1, ]
valid.data <- data_main[ind2, ]
test.data <- data_main[ind3, ]

dim(train.data)
dim(valid.data)
dim(test.data)

train.data_1 <- train.data[, -1]
valid.data_1 <- valid.data[, -1]
test.data_1 <- test.data[, -1]

# Plot correct validation rates against k to choose optimal k
class.rate<-numeric(25)
for(k in 1:25)
{
  pred.class<-knn(train.data_1[,-12], valid.data_1[,-12], train.data_1[, 12], k=k)
  class.rate[k]<-sum((pred.class==valid.data_1$Class))/length(pred.class)
}
plot(c(1:25),class.rate,type="l",
     main="Correct Classification Rates Over A Range Of K",
     xlab="k",ylab="Correct Classification Rate",cex.main=0.7, ylim = c(0.7, 0.8))

which.max(class.rate)

# Fit knn model

pred<-knn(train.data_1[,-12], test.data_1[,-12], train.data_1[,12], k=25)
sum((pred==test.data_1$Class))/length(pred)
print(pred)

# Sensitivity and specificity
confusionMatrix(pred, test.data_1[, 12])

#####################

# Classification Trees


first_tree <- rpart(Class~ Oscore + Nscore + Escore + Cscore + Ascore + Age +
                      Education + X.Country + Ethnicity + Impulsive + SS,
                     data = train.data, method = "class",
                     parms = list(split = 'information'))
rpart.plot(first_tree,type=2,extra=4)

# Create fully grown tree, and prune it

first_tree_fully_grown <- rpart(Class~ Oscore + Nscore + Escore + Cscore + Ascore + Age +
                      Education + X.Country + Ethnicity + Impulsive + SS,
                    data = train.data, method = "class",
                    parms = list(split = 'information'),
                    cp = -1, minsplit = 2, minbucket = 1)

printcp(first_tree_fully_grown)

# Need the largest tree that has xerror less than 0.6172 (0.56338 + 0.053940).
# Cp for this tree is 0.0176056.

first_tree_pruned <- prune(first_tree_fully_grown, cp = 0.018)
rpart.plot(first_tree_pruned, type = 2, extra = 4)

# Test tree on validation and test data

predict_data <- predict(first_tree_pruned, valid.data, type = 'class')
table_mat <- table(valid.data$Class, predict_data)

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

# Accuracy on valid_data is 70.67%
# On test data:

predict_data_2 <- predict(first_tree_pruned, test.data, type = 'class')
table_mat_2 <- table(test.data$Class, predict_data_2)

accuracy_Test_2 <- sum(diag(table_mat_2)) / sum(table_mat_2)

# Accuracy on test data is 69.33

# Check sensitivity and specificity for the test data

cross.class.tab<-table(test.data$Class, predict_data_2)
cross.class.rates<-sweep(cross.class.tab,1,apply(cross.class.tab,1,sum),"/")

cross.class.rates[1, 1]
cross.class.rates[2, 2]

# Check sensitivity and specificity for the validation data

cross.class.tab_2 <- table(valid.data$Class, predict_data)
cross.class.rates_2 <- sweep(cross.class.tab_2,1,apply(cross.class.tab_2,1,sum),"/")

cross.class.rates_2[1, 1]
cross.class.rates_2[2, 2]

# Random forest
set.seed(65)
library(randomForest)
model <- randomForest(Class~ Oscore + Nscore + Escore + Cscore + Ascore + Age +
                           Education + X.Country + Ethnicity + Impulsive + SS,
                         data = train.data, ntree = 200)

pred <- predict(model, test.data)
confusionMatrix(pred, test.data$Class)

###################

# Support vector machines
set.seed(75)
library(MASS)
library(e1071)

n<-nrow(data_main)
intrain<-sample(c(1:n),n/2)
invalid<-sample((c(1:n)[-intrain]),n/4)
train.data<-data_main[intrain,-c(1)]
valid.data<-data_main[invalid,-c(1)]
test.data<-data_main[-c(intrain,invalid),-c(1)]

pred.error<-function(pred,truth)
{
  1-sum(diag(table(pred,truth)))/length(truth)
}
C.val<-c(0.1,0.5,1,2,5,10)
C.error<-numeric(length(C.val))
for(i in 1:length(C.val))
{
  model<-svm(Class~.,data=train.data,type="C-classification",kernel="linear",
             cost=C.val[i])
  pred.model<-predict(model, valid.data)
  C.error[i]<-pred.error(pred.model,valid.data$Class)
}
C.sel<-C.val[min(which.min(C.error))]
C.sel

# Value of C that minimises prediction error is 0.5
# Plot

plot(C.val,C.error,type="b", ylim = c(0.22, 0.26), ylab = "Prediction Error", 
     xlab = "Values of C", main = "Optimum Values of C")
abline(v=C.sel,lty=2)

# Run svm

final.svm<-svm(Class~.,data=train.data,kernel="linear",
               cost=C.sel,type="C-classification")
summary(final.svm)

pred.test<-predict(final.svm,test.data)
pred.error(pred.test,test.data$Class)

# Try now with a radial kernel model
set.seed(13)
tune.res<-tune(svm,Class~., data=train.data, type="C-classification", kernel="radial",
               ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.res)

# Sensitivity/specificity for final.svm
yhat <- predict(final.svm, test.data)
confusionMatrix(yhat, test.data$Class)

# For tune.res/radial model
yhat_2 <- predict(tune.res$best.model, test.data)
confusionMatrix(yhat_2, test.data$Class)
