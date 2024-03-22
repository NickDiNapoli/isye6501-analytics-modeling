data <- read.table("uscrime.txt", header=TRUE)
print(head(data))

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
squared_err = sum((test[,16] - pred2)^2)

# Lasso
library(glmnet)
lasso <- glmnet(as.matrix(train), as.matrix(train[,16]) , family="mgaussian", 
                alpha=1)



