a <- c(1:100)
b <- a^2
df1 <- data.frame(a, b, NA)

d <- seq(1, 100, by =5)
f <- d^3
df2 <- data.frame(d, f)

l <- c(1:nrow(df1))
k <- c(1:nrow(df2))

for(i in l){
  for(j in k){
    if ((df1[i,1] <= df2[j,1]) & (df1[i,1] > df2[(j+1),1]) & ((j+1) <= k)){
      df1[i,3] <- df2[j,2]
    }
  }
} 

plot(df1)