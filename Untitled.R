NoStep <- 100

a <- c(1:NoStep)
b <- a^2
df1 <- data.frame(a, b)

a <- seq(1, NoStep, by =5)
f <- a^3
df2 <- data.frame(a, f)

r <- seq(1, NoStep, by =10)
p <- r*10
df3 <-  data.frame(r, p)

dfTotal <- data.frame(Steps = c(1:NoStep))

dfTotal <- cbind(dfTotal,df1[,2])

total <- merge(df1,df2,by = "a", all.x =TRUE, all.y =TRUE)

# for (i in dfTotal[,1]){
#   if ()
# }


