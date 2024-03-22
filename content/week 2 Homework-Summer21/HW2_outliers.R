library(outliers)
data <- read.table("uscrime.txt", header=TRUE)
crimes <- data[,16]
crimesdf <- data.frame(crimes)

outlier <- grubbs.test(crimes, type=10)
print(outlier)

boxplot(crimesdf, main="Outlier detection", ylab="Crimes per 100,000 people")
