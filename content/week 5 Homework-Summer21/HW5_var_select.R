data <- read.table("uscrime.txt", header=TRUE)
print(head(data))

set.seed(123)
n <- nrow(data)
train_idx <- sample(1:n, size=round(n*0.7), replace = FALSE)
train <- data[train_idx,]
test <- data[-train_idx,]

model1 <- glm(Crime~., data=train)
summary(model1)
pred <- predict(model1, test)
print(pred)

#print(test[,16] - pred)
squared_err = sum((test[,16] - pred)^2)

keep <- c(1, 3, 14, 15, 16)

model2 <- glm(Crime~., data=train[,keep])
summary(model2)
pred2 <- predict(model2, test[,keep])
print(pred2)
squared_err = sum((test[,16] - pred2)^2)

# Lasso
library(glmnet)
lasso <- cv.glmnet(as.matrix(train[,1:15]), as.matrix(train[,16]),
                   alpha=1, standardize = TRUE)

print(lasso$lambda.min)
plot(lasso)
best_lambda <- lasso$lambda.min
best_lasso <- glmnet(as.matrix(train), as.matrix(train[,16]),
                   alpha=1, standardize = TRUE, lambda = best_lambda)
coef(lasso)
pred3 <- predict(best_lasso, as.matrix(test))
print(pred3)
squared_err = sum((test[,16] - pred3)^2)

#Elastic net
library(tidyverse)
library(caret)
elastic <- train(Crime ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10), tuneLength = 50)
print(elastic$bestTune)
coef(elastic$finalModel, elastic$bestTune$lambda)
print(elastic)
pred4 <- elastic%>%predict(test)
squared_err = sum((test[,16] - pred4)^2)
