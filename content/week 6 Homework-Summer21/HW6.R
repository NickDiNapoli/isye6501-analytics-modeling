data <- read.table("breast-cancer-wisconsin.data.txt", header=FALSE, sep = ",")
data <- data[,-1]
print(data)

miss_val <- c()
for (i in 1:nrow(data)) {
  if (data[i,6] == '?') {
    print(i)
    miss_val <- c(miss_val, i)
  }
}
#print(miss_val)
#print(as.numeric(data[-miss_val,7]))
V7_keep <- as.numeric(data[-miss_val,6])
print(mean(V7_keep))
print(sd(V7_keep))

model_missing <- glm(as.numeric(V7) ~ V2+V3+V4+V5+V6+V8+V9+V10+V11, 
                     data=data[-miss_val,])
summary(model_missing)
pred <- predict(model_missing, data[miss_val,-6])
print(pred)

perturb <- rnorm(683, mean = 3.544, sd = 3.643)
plot(perturb)
rand_i <- sample(1:683, 16)
rand_oper <- sample(1:2, 1)
perturb_keep <- perturb[rand_i]

perturb_imputed <- c()
for (i in 1:16) {
  r = sample(1:2, 1)
  if (r == 1) {
    perturb_imputed <- c(perturb_imputed, pred[i] + perturb_keep[i])
  } else {
    perturb_imputed <- c(perturb_imputed, pred[i] - perturb_keep[i])
  }
}

###############################################################################

# SVM using removed data
data1 <- data[-miss_val,]
data1 <- transform(data1, V7 = as.integer(V7))
data1 <- as.matrix(data1)
set.seed(123)
n1 <- nrow(data1)
train_idx1 <- sample(1:n1, size=round(n1*0.7), replace = FALSE)
train1 <- data[train_idx1,]
test1 <- data[-train_idx1,]

model1 <- ksvm(data1[train_idx1,-10], data1[train_idx1,10], type="C-svc", 
               kernel="vanilladot", C=1)
a_vector <- colSums(model1@xmatrix[[1]] * model1@coef[[1]])
a0 <- model1@b
pred1 <- predict(model1, data1[-train_idx1,-10])

# frac of model predictions match actual classification 
acc1 <- sum(pred1 == data1[-train_idx1,10]) / (n1*0.3)

###############################################################################

# SVM using mean data
data2 <- data
for (i in 1:nrow(data2)) {
  if (data2[i,6] == '?') {
    data2[i,6] = 3.544656
  }
}

data2 <- transform(data2, V7 = as.double(V7))
data2 <- as.matrix(data2)
set.seed(123)
n2 <- nrow(data2)
train_idx2 <- sample(1:n2, size=round(n2*0.7), replace = FALSE)
train2 <- data[train_idx2,]
test2 <- data[-train_idx2,]

model2 <- ksvm(data2[train_idx2,-10], data2[train_idx2,10], type="C-svc", 
               kernel="vanilladot", C=1)
a_vector <- colSums(model2@xmatrix[[1]] * model2@coef[[1]])
a0 <- model2@b
pred2 <- predict(model2, data2[-train_idx2,-10])

# frac of model predictions match actual classification 
acc2 <- sum(pred2 == data2[-train_idx2,10]) / (n2*0.3)

###############################################################################

# SVM using regression data
data3 <- data
count = 1
for (i in 1:nrow(data3)) {
  if (data3[i,6] == '?') {
    data3[i,6] = pred[count]
    count = count + 1
    print(count)
  }
}
#print(data3)
data3 <- transform(data3, V7 = as.double(V7))
data3 <- as.matrix(data3)
set.seed(123)
n3 <- nrow(data3)
train_idx3 <- sample(1:n3, size=round(n3*0.7), replace = FALSE)
train3 <- data[train_idx3,]
test3 <- data[-train_idx3,]

model3 <- ksvm(data3[train_idx3,-10], data3[train_idx3,10], type="C-svc", 
               kernel="vanilladot", C=1)
a_vector <- colSums(model3@xmatrix[[1]] * model3@coef[[1]])
a0 <- model3@b
pred3 <- predict(model3, data3[-train_idx3,-10])

# frac of model predictions match actual classification 
acc3 <- sum(pred3 == data3[-train_idx3,10]) / (n3*0.3)

###############################################################################

# SVM using regression data
data4 <- data
count = 1
for (i in 1:nrow(data4)) {
  if (data4[i,6] == '?') {
    data4[i,6] = perturb_imputed[count]
    count = count + 1
    print(count)
  }
}

data4 <- transform(data4, V7 = as.double(V7))
data4 <- as.matrix(data4)
set.seed(123)
n4 <- nrow(data4)
train_idx4 <- sample(1:n4, size=round(n4*0.7), replace = FALSE)
train4 <- data[train_idx4,]
test4 <- data[-train_idx4,]

model4 <- ksvm(data4[train_idx4,-10], data4[train_idx4,10], type="C-svc", 
               kernel="vanilladot", C=1)
a_vector <- colSums(model4@xmatrix[[1]] * model4@coef[[1]])
a0 <- model4@b
pred4 <- predict(model4, data4[-train_idx4,-10])

# frac of model predictions match actual classification 
acc4 <- sum(pred4 == data4[-train_idx4,10]) / (n4*0.3)
