

a <- c(1:100)
b <- a^2
df1 <- data.frame(a, b)

d <- seq(1, 100, by =5)
f <- d^3
df2 <- data.frame(d, f)

l <- c(1:nrow(df1))
k <- c(2:nrow(df2))
df3 <- data.frame()
df4 <- data.frame()
for(j in k){
  for(i in l){
    if (df1[i,1] == df2[j,1]){
      df3 <- c(df1[i,1],df1[i,2],df2[j,2])
      df4 <- rbind(df4,df3)
    }
    else if((df1[i,1] < df2[j,1]) & (df1[i,1] >= df2[(j-1),1])){
      df3 <- c(df1[i,1],df1[i,2],df2[(j-1),2])
      df4 <- rbind(df4,df3)
  }
  }
}



plot(df4)