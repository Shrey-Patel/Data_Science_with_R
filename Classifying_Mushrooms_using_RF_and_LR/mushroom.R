library(tidyverse)
library(tictoc)

##### Loading data set #####

#mushroom <- read.csv("mushroom.csv", header=TRUE)
mushroom <- read.csv("https://s3.amazonaws.com/notredame.analytics.data/mushrooms.csv", header=TRUE)

data <- mushroom[c("type", "odor", "cap_shape", "cap_color", "gill_attachment")]
dim(data)

# list of independent variables
features <- c("odor", "cap_shape", "cap_color", "gill_attachment")

data <- as_tibble(data)

# adding outcome variable, "edible"; where 0:poisonous and 1:edible
data <- mutate(data,
               edible = as.integer(as.logical(type=="edible")))
#summary(data)
#head(data)

# removing type column
data <- select(data, -type)

# converting columns to factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                           as.factor)
data$edible <- as.factor(data$edible)

summary(data)
head(data)

##### train-test split #####
library(caTools)

sample = sample.split(data$edible, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

##### Random Forest ######
library(dslabs)
library(randomForest)

tic("Random Forest")
rf <- randomForest(train$edible~., data=train[1:4])
rf_pred <- predict(rf, test[1:4])
toc()

# confusion matrix
library(caret)

confusionMatrix(table(rf_pred, 
                      test$edible), positive="1")

# ROC curve
library(ROCR)

pred_rf <- prediction(as.numeric(rf_pred), test$edible)

roc_rf <- performance(pred_rf, "tpr", "fpr")
plot(roc_rf, colorize = FALSE, lwd = 2, col = "blue", main = "ROC curve")
lines(c(0:100)/100, c(0:100)/100, lty = 2)
auc_rf <- performance(pred_rf, measure = "auc")
auc_rf <- auc_rf@y.values[[1]]
text(0.9, 0.1, "AUC=", cex = 1)
text(0.96, 0.1, round(auc_rf, 3), cex = 1)

# Precision-Recall curve
prc_rf <- performance(pred_rf, "prec", "rec")
plot(prc_rf, colorize = FALSE, lwd = 2, col = "blue", main = "Precision-Recall curve")
lines(c(0:100)/100, c(0:100)/100, lty = 2)


##### Logistic Regression ######

library(ISLR)

tic("Logistic Regression")
lr <- glm(train$edible ~., family="binomial", data=train[1:4])
#summary(lr)
lr_pred_prob <- predict(lr, test[1:4], type="response")
toc()

lr_pred <- ifelse(lr_pred_prob > 0.5, "1", "0")

# confusion matrix
confusionMatrix(table(lr_pred, 
                      test$edible), positive="1")

# ROC curve
pred_lr <- prediction(lr_pred_prob, test$edible)

roc_lr <- performance(pred_lr,"tpr","fpr")
plot(roc_lr,colorize=FALSE, lwd=2, col="red")
lines(c(0:100)/100,c(0:100)/100, lty=2)
#lines(rep(0,101),c(0:100)/100, lty=2, lwd=2, col="blue")
#lines(c(0:100)/100, rep(1,101), lty=2, lwd=2, col="blue") 
auc_lr <- performance(pred_lr, measure = "auc")
auc_lr <- auc_lr@y.values[[1]]
text(0.9, 0.1, "AUC=", cex = 1)
text(0.96, 0.1, round(auc_lr, 3), cex = 1)

# Precision-Recall curve
prc_lr <- performance(pred_lr, "prec", "rec")
plot(prc_lr, colorize = FALSE, lwd = 2, col = "blue", main = "Precision-Recall curve")
lines(c(0:100)/100, c(0:100)/100, lty = 2)

