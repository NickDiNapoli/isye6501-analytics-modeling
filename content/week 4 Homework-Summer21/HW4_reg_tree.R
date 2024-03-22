library(rpart)
library(rpart.plot)
library(randomForest)
data_raw <- read.table("uscrime.txt", header=TRUE)
data <- as.data.frame(data_raw[,1:15])
response <- data_raw[,16]

reg_tree <- rpart(response~., data=data, method="anova", 
                  control=rpart.control(minsplit=10))

printcp(reg_tree) 
#plotcp(reg_tree)
summary(reg_tree) 

#visualizations 
rpart.plot(reg_tree)
rpart.plot(reg_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
#plot(reg_tree, uniform=TRUE)# , main="Regression tree on crimes data")
#text(reg_tree, use.n=TRUE, all=TRUE)#, cex=.5)

preg_tree <- prune(reg_tree,
                   cp=reg_tree$cptable[which.min(reg_tree$cptable[,"xerror"]),"CP"])
rpart.plot(preg_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

# random forest
rand_for <- randomForest(response~., data=data, ntree= 1000, mtry=4)
print(rand_for) 
importance(rand_for) 
pred <- predict(rand_for, data)
plot(response, col="green", main='Optimized random forest predictions', 
     xlab='test point', ylab='prediction')
points(pred, col="red")
