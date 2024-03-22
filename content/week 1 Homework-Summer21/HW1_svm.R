#install.packages("kernlab")
library(kernlab)
data <- read.table("credit_card_data.txt")
data <- as.matrix(data)

# calling svm, using vanilladot as simple linear kernel 
model <- ksvm(data[,1:10], data[,11], type="C-svc", kernel="vanilladot", C=1, scaled=TRUE)

# calculate a_1...a_m
a_vector <- colSums(model@xmatrix[[1]] * model@coef[[1]])

# calculate a_0
a0 <- model@b
pred <- predict(model, data[,1:10])
#print(typeof(pred))

# frac of model predictions match actual classification 
acc <- sum(pred == data[,11]) / nrow(data)
#print(a_vector)