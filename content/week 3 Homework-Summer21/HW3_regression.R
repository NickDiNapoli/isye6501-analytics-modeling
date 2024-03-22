data <- read.table("uscrime.txt", header=TRUE)
crimes <- data[,1:15]

model <- lm(data[,16]~., data=crimes)
summary(model)
#anova(model)

test_point <- c(14.0, 0, 10.0, 12.0, 15.5, 0.640, 94.0, 150, 1.1, 
                       0.120, 3.6, 3200, 20.1, 0.04, 39.0)

#pred <- predict(model, test_point)
coeffs <- model$coefficients
print(coeffs)
keep <- c(2, 4, 14, 15)

print(as.matrix(coeffs[keep]))
print(as.matrix(point))

print((t(as.matrix(coeffs[-1])) %*% as.matrix(test_point)) + coeffs[1])

model_concise <- lm(data[,16]~., data=crimes[,keep-1])
summary(model_concise)
coeffs2 <- model_concise$coefficients
print(coeffs2)
library(jtools)
point <- test_point[keep-1]
print(t(as.matrix(coeffs2[-1])) %*% as.matrix(point) + coeffs2[1])
effect_plot(model_concise, pred = Prob, interval = TRUE, plot.points = TRUE)
