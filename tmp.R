f <- function(x)
{
  a <- runif(1, x + 0.01, x + 0.05)
  return(a)
}
#install.packages("matlib")
library("matlib")
a <- matrix(c(1/7, -1, -1/2, -1/3), 2, 2)
b <- matrix(c(-5/14, -17/3), 2, 1)
res <-gaussianElimination(a, b)
res[,3]
plot(1, 1, xlim = c(0,10), ylim = c(0,10))
lines(c(1,3), c(1,8))
lines(c(2,5), c(5,4))
points(3,8)
points(res[2,3], res[1,3])

install.packages("C:\\spatstat_1.56-1.tar")

library("spatstat")
f <- rpoislpp(2*10^-1, win = owin(c(0,40), c(0,40)), simplenet, nsim = 100)
g <- rpoispp(2*10^-3, 200, win = owin(c(0,40), c(0,40)))
g <- rpoispp(8*10^-3, 200, win = owin(c(0,40), c(0,40)))
plot(g$x,g$y)


a <- array(0, c(2,2))
a

g <- rpoispp(2*10^-3, win = owin(c(0,40), c(0,40)))
plot(g$x, g$y)













