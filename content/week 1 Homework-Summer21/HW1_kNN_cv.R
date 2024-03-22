#install.packages("kknn")
library(kknn)
data <- read.table("credit_card_data.txt")

# splitting data 
train_size <- floor(0.6 * nrow(data))
val_size <- floor(0.2 * nrow(data))
test_size <- floor(0.2 * nrow(data))

# random sampling of data w/o replacement 
train_idx <- sort(sample(seq_len(nrow(data)), size=train_size))
not_train_idk <- setdiff(seq_len(nrow(data)), train_idx)
val_idx <- sort(sample(not_train_idk, size=val_size))
test_idx <- setdiff(not_train_idk, val_idx)
#test_idx <- sort(sample(not_train_idk, size=test_size))

# different sets
train <- data[train_idx,1:10]
val <- data[val_idx,1:10]
test <- data[test_idx,1:10]

model = train.kknn((data[train_idx,11])~., data=data[train_idx,1:10], kmax= 20, 
                   distance = 2, kernel = c("optimal","rectangular", "inv", "gaussian",
                    "triangular", "cos", "biweight", "triweight"), scale=TRUE)

#print(sum(as.integer(round(predict(model, test))) == data[test_idx,11]) / nrow(test))
print(model)
acc_array <- vector()
acc_array_test <- vector()
# this is run for val and test
for (k in 1:25) {
  model = kknn((data[train_idx,11])~., train=data[train_idx,1:10], 
               test=data[test_idx,1:10], k = k, distance = 2, kernel = "inv", scale=TRUE)
  
  pred_rounded <- as.integer(round(predict(model)))
  acc_array_test[k] <- sum(pred_rounded == data[test_idx,11]) / nrow(test)
}

plot(acc_array_test, main='Optimized kNN model with cross-validation accuracy 
     changes with k', xlab='k', ylab='Accuracy', col="red", cex.main=0.75)
points(acc_array, col="green")
