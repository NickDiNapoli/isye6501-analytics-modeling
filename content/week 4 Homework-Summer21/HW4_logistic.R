data_raw <- read.table("germancredit.txt", sep=' ')
#data <- data_raw[,1:20]
#response <- data_raw[,21] - 1 

m <- nrow(data_raw)
train <- sample(1:m, size=round(m*0.7), replace = FALSE)
learn <- data_raw[train,]
val <- data_raw[-train,]

log_reg <- glm(V21 -1 ~., data=learn, family=binomial(link='logit'))
summary(log_reg)

pred <- predict(log_reg, val)

log_reg1 <- glm(V21 -1 ~ V1 + V2 +V3 +V4 +V5 +V6 +V8 +V10, data=learn,
                family=binomial(link='logit'))
summary(log_reg1)

pred <- predict(log_reg1, val, type="response")
