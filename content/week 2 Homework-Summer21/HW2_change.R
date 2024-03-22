data <- read.table("temps.txt", header=TRUE)
avg_array <- c()
for (year in 2:ncol(data)) {
  avg_array <- c(avg_array, sum((data[,year])/nrow(data)))
}

#180, 3
t = 15
c = 3
for (i in 2:ncol(data)) {
  cusum = 0 
  for (j in 1:nrow(data)) {
    cusum <- max(0, cusum - data[j,i] + avg_array[i-1] - c)
    #print(cusum)
    if (cusum >= t) {print(data[j,1])}
  }
  print("---------------------------")
}
plot(avg_array, type = "l", main='Avg temperature in July-Oct in Atlanta from 1996-2015',
     xlab='Years after 1995', ylab='Avg temperature', cex.main=0.75)

# CUSUM on avg temp data
t = 3
c = 1
cusum = 0
counter = 0
avg <- sum(avg_array)/length(avg_array)
for (k in avg_array) {
  cusum <- max(0, cusum + k - avg - c)
  #print(cusum)
  if (cusum >= t) {print(counter)}
  counter <- counter + 1 
}

