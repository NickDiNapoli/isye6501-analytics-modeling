data <- read.table("uscrime.txt", header=TRUE)
crimes <- as.matrix(data[,1:15])

PCA <- prcomp(crimes, scale.=TRUE, retx=TRUE)
print(PCA)
summary(PCA)
screeplot(PCA, main='PCA results on all scaled data', xlab='PCs')
x4 <- PCA$x[,1:4]
rot <- PCA$rotation
#print(x4)
crimesPC4 <- as.data.frame(cbind(x4, data[,16]))
print(crimesPC4)
lr_PCA <- lm(V5~., data=crimesPC4)
summary(lr_PCA)
intercept <- lr_PCA$coefficients[1]
Bs <- lr_PCA$coefficients[2:5]
# rotation
As <- rot[,1:4] %*% Bs
t(As)
#de-scaling
descaled_A <- As / sapply(data[,1:15], sd)
descaled_intercept <- intercept - sum(As*sapply(data[,1:15], mean)/sapply(data[,1:15], sd))
t(descaled_A)
print(descaled_intercept)
#prediction of test point
test_point <- c(14.0, 0, 10.0, 12.0, 15.5, 0.640, 94.0, 150, 1.1, 
                0.120, 3.6, 3200, 20.1, 0.04, 39.0)
print((t(as.matrix(descaled_A)) %*% as.matrix(test_point)) + descaled_intercept)

