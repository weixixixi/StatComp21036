## -----------------------------------------------------------------------------
x<-c(0.10, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.20, 0.21, 0.23)
y<-c(42.0, 43.5, 45.0, 45.5, 45.0, 47.5, 49.0, 53.0, 50.0, 55.0, 55.0, 60.0)
lm.sol<-lm(y ~ 1+x)
summary(lm.sol)

## -----------------------------------------------------------------------------
Height<-c(56.5, 65.3, 64.3, 56.3, 59.8, 66.5, 51.3, 62.5, 62.8, 69.0, 63.5, 67.0, 57.3, 62.5, 59.0, 72.0, 64.8, 57.5, 66.5)
Weight<-c( 84.0, 98.0, 90.0, 77.0, 84.5, 112.0, 50.5, 112.5, 102.5, 112.5, 102.5, 133.0, 83.0, 84.0, 99.5, 150.0, 128.0, 85.0, 112.0)
plot(Weight~Height)

## -----------------------------------------------------------------------------
library(knitr)
a <- c(0.3901401, 0.38509514, 0.3951850, 0.1016328, 0.08827257, 0.1149931)
m <- matrix(a,nrow=2,ncol=3,dimnames=list(c("a","b"),c("Estimate","Left","Right")))
knitr::kable(m, caption="CI")

## -----------------------------------------------------------------------------
#σ=1
n <- 100
u <- runif(n)
x1 <- sqrt(-2*log(u))
x1
#σ=2
x2 <- sqrt(-8*log(u))
x2
#σ=0.5
x3 <- sqrt(-0.5*log(u))
x3
#σ=0.1
x4 <- sqrt(-0.02*log(u))
x4

## -----------------------------------------------------------------------------
#σ=1
hist(x1, prob = TRUE, breaks = 20, col="light blue", main = expression(f(x)==x*exp(-x^2/2)), axes = TRUE, cex.main = 0.75)
y <- seq(0, 3.5, .01)
lines(y, y*exp(-y^2/2), col="red")
#σ=2
hist(x2, prob = TRUE, breaks = 20, col="light blue", main = expression(f(x)==(x/4)*exp(-x^2/8)), axes = TRUE, cex.main = 0.75)
y <- seq(0, 7, .01)
lines(y, (y/4)*exp(-y^2/8), col="red")
#σ=0.5
hist(x3, prob = TRUE, breaks = 20, col="light blue", main = expression(f(x)==4*x*exp(-2*x^2)), axes = TRUE, cex.main = 0.75)
y <- seq(0, 1.5, .01)
lines(y, 4*y*exp(-2*y^2), col="red")
#σ=0.1
hist(x4, prob = TRUE, breaks = 20, col="light blue", main = expression(f(x)==100*x*exp(-50*x^2)), axes = TRUE, cex.main = 0.75)
y <- seq(0, 3.5, .01)
lines(y, 100*y*exp(-50*y^2), col="red")

## -----------------------------------------------------------------------------
f_sample <- function(p1){
n <- c(1:1000)
i <- 1
while (i<=1000) {
  u <- runif(1)
  if(u < p1)
    {n[i] <- rnorm(1,0,1)} else
    {n[i] <- rnorm(1,3,1)}
  i=i+1
}
n}
n <- f_sample(0.75)

## -----------------------------------------------------------------------------
hist(n, breaks=50, prob = TRUE)

## -----------------------------------------------------------------------------
#p_1=0.9
n1 <- f_sample(0.9)
hist(n1, breaks=50, col="red")

#p_1=0.5
n2 <- f_sample(0.5)
hist(n2, breaks=50, col="orange")

#p_1=0.1
n3 <- f_sample(0.1)
hist(n3, breaks=50, col="green")
plot.ecdf(n)
text(1.8,0.8,expression(p1==0.75))
y <- seq(0.001, 1, by = 0.001)
lines(sort(n1), y, col="red")
text(1,0.95,expression(p1==0.9))
lines(sort(n2), y, col="orange")
text(1.8,0.5,expression(p1==0.5))
lines(sort(n3), y, col="green")
text(2,0.1,expression(p1==0.1))

## -----------------------------------------------------------------------------

hist(f_sample(0.3), breaks=50, prob = TRUE)
hist(f_sample(0.7), breaks=50, prob = TRUE)
hist(f_sample(0.45), breaks=50, prob = TRUE)
hist(f_sample(0.55), breaks=50, prob = TRUE)


## -----------------------------------------------------------------------------
f_poisson <- function(t, lambda, alpha, beta){
X <- 0
I <- 1
N <- rpois(1, lambda*t)
while (I <= N) {
  u <- rgamma(1, alpha, beta)
  X = X + u
  I = I + 1
}
X}
round(f_poisson(10,6,1,2),4)

## -----------------------------------------------------------------------------
f_meanvar <- function(N, lambda, alpha, beta){
  m <- c(1:N)
  for(i in 1:N){
    m[i] <- f_poisson(10, lambda, alpha, beta)
  }
  a1 <- mean(m)
  b1 <- var(m)
  a2 <- 10*alpha*lambda/beta
  b2 <- 10*(alpha+alpha^2)*lambda/beta^2
  e1 <- abs(a1-a2)/a2
  e2 <- abs(b1-b2)/b2
  e2
  d <- c(a1, a2, e1, b1, b2, e2)
  d
}
r1 <- round(f_meanvar(1000,1,1,1),4)
r1

## -----------------------------------------------------------------------------
r2 <- round(f_meanvar(1000,1,1,2),4)
r2

## -----------------------------------------------------------------------------
r3 <- round(f_meanvar(1000,1,2,1),4)
r3

## -----------------------------------------------------------------------------
r4 <- round(f_meanvar(1000,1,2,2),4)
r4

## -----------------------------------------------------------------------------
r5 <- round(f_meanvar(1000,2,1,1),4)
r5

## -----------------------------------------------------------------------------
r6 <- round(f_meanvar(1000,2,1,2),4)
r6

## -----------------------------------------------------------------------------
r7 <- round(f_meanvar(1000,2,2,1),4)
r7

## -----------------------------------------------------------------------------
r8 <- round(f_meanvar(1000,2,2,2),4)
r8

## -----------------------------------------------------------------------------
a <- c(r1, r2, r3, r4, r5, r6, r7, r8)
m <- matrix(a, nrow = 8, ncol = 6, byrow = TRUE, dimnames = list(c("(1,1,1)", "(1,1,2)", "(1,2,1)", "(1,2,2)", "(2,1,1)", "(2,1,2)", "(2,2,1)", "(2,2,2)"),c("Simutation m", "Theory m", "Error m", "Simutation v", "Theory v", "Error v")))
knitr::kable(m)

## -----------------------------------------------------------------------------
f_1 <- function(x){
  m <- 1e4
  y <- runif(m, min = 0, max = x)
  F_hat <- mean(y^2*(1-y)^2)*30*x
  F_hat
}
for(i in 1:9){
  a <- f_1(i/10)
  b <- pbeta(i/10,3,3)
  err <- abs((a-b)/b)
  d <- c(a, b, err)
  print(round(d,4))
}

## -----------------------------------------------------------------------------
#σ=1
n <- 1000
u <- runif(n/2)
#σ=1
x1 <- sqrt(-2*log(u))
#σ=2
x2 <- sqrt(-8*log(u))
#σ=0.5
x3 <- sqrt(-0.5*log(u))
#σ=0.1
x4 <- sqrt(-0.02*log(u))
#σ=1
y1 <- sqrt(-2*log(1-u))
#σ=2
y2 <- sqrt(-8*log(1-u))
#σ=0.5
y3 <- sqrt(-0.5*log(1-u))
#σ=0.1
y4 <- sqrt(-0.02*log(1-u))
u <- runif(n/2)
#σ=1
z1 <- sqrt(-2*log(u))
#σ=2
z2 <- sqrt(-8*log(u))
#σ=0.5
z3 <- sqrt(-0.5*log(u))
#σ=0.1
z4 <- sqrt(-0.02*log(u))
scales::percent((var((x1+z1)/2)-var((x1+y1)/2))/var((x1+z1)/2),0.01)
scales::percent((var((x2+z2)/2)-var((x2+y2)/2))/var((x2+z2)/2),0.01)
scales::percent((var((x3+z3)/2)-var((x3+y3)/2))/var((x3+z3)/2),0.01)
scales::percent((var((x4+z4)/2)-var((x4+y4)/2))/var((x4+z4)/2),0.01)

## -----------------------------------------------------------------------------
x <- seq(1, 4, .01)
w <- 2
f1 <- 1/x^2
f2 <- 1 / ((1 + (x-sqrt(2))^2) * pi)#Cauchy distribution f(x,sqrt(2),1)
g <- (x^2/sqrt(2*pi))*exp(-x^2/2)
#figure (a)
plot(x, g, type = "l", main = "", ylab = "", ylim = c(-0.5,1), lwd = w)
lines(x, f1, lty = 2, lwd = w)
lines(x, f2, lty = 3, lwd = w)
legend("topright", legend = c("g", 0:2), lty = 1:3, lwd = w, inset = 0.02)
#figure (b)
plot(x, g, type = "l", main = "", ylab = "", ylim = c(-0.5,3), lwd = w, lty = 2)
lines(x, g/f1, lty = 3, lwd = w)
lines(x, g/f2, lty = 4, lwd = w)
legend("topright", legend = c(0:2), lty = 2:4, lwd = w, inset = 0.02)
sd(g/f1)
sd(g/f2)

## -----------------------------------------------------------------------------
n <- 10000
u <- runif(n)
x <- 1/(1-u)
mean((x^4/sqrt(2*pi))*exp(-x^2/2))

## -----------------------------------------------------------------------------
n <- 50
q <- 1:200
for (k in 1:200){
  x <- rchisq(n, df = 2)
  m <- mean(x)
  m
  s <- sd(x)
  p <- m+c(-1,1*qt(0.975,n-1)*s/sqrt(n))
  a <- 1:1000
  b <- 0
  for (i in 1:1000){
    x <- rchisq(n, df = 2)
    a[i] <- mean(x)
    if (p[1]<a[i]&a[i]<p[2]){
      b <- b+1
    }
  }
  q[k] <- b/1000
}
mean(q)
UCL <- replicate(1000, expr = {
x <- rchisq(n, df = 2)
(n-1) * var(x) / qchisq(0.05, df = n-1)
})
mean(UCL>4)

## -----------------------------------------------------------------------------
n <- 20
alpha <- .05
mu0 <- 1
m <- 10000 #number of replicates

p1 <- numeric(m) #storage for p-values
for (j in 1:m) {
x1 <- rchisq(n, df=1)
ttest <- t.test(x1,alternative = "two.sided", mu = mu0)
p1[j] <- ttest$p.value
}
p.hat1 <- mean(p1 < alpha)
se.hat1 <- sqrt(p.hat1 * (1 - p.hat1) / m)
print(c(p.hat1, se.hat1))

p2 <- numeric(m) #storage for p-values
for (j in 1:m) {
x2 <- runif(n, 0,2)
ttest <- t.test(x2,alternative = "two.sided", mu = mu0)
p2[j] <- ttest$p.value
}
p.hat2 <- mean(p2 < alpha)
se.hat2 <- sqrt(p.hat2 * (1 - p.hat2) / m)
print(c(p.hat2, se.hat2))

p3 <- numeric(m) #storage for p-values
for (j in 1:m) {
x3 <- rexp(n,1)
ttest <- t.test(x3,alternative = "two.sided", mu = mu0)
p3[j] <- ttest$p.value
}
p.hat3 <- mean(p3 < alpha)
se.hat3 <- sqrt(p.hat3 * (1 - p.hat3) / m)
print(c(p.hat3, se.hat3))

Type_I_error<-c(p.hat1,p.hat2,p.hat3)
sd_Type_I_error<-c(se.hat1,se.hat2,se.hat3)
Three_Distribution<-c("Chisq","Uniform","Exp")
MC_result<-data.frame(Three_Distribution,Type_I_error,sd_Type_I_error)
MC_result

## -----------------------------------------------------------------------------
d <- 2
#Type I error
n <- c(10, 20, 30, 50, 100, 500) #sample sizes
cv <- (6/n)*qchisq(.95, d*(d+1)*(d+2)/6) 

sk <- function(x,n) {
#computes the sample skewness coeff.
xbar <- apply(x,2,mean)
x2 <- matrix(nr=n,nc=n) 
for(i in 1:n){
  x1 <- (x[i,]-xbar)%*%solve(cov(x)/(n-1)*n)
  for(j in 1:n){
    x2[i,j] <- x1%*%(x[j,]-xbar)
  }
}
return(mean(x2^3))
}

p.reject <- numeric(length(n)) #to store sim. results
m <- 200 #num. repl. each sim.
for (i in 1:length(n)) {
  sktests <- numeric(m) #test decisions
  for (j in 1:m) {
    x <- matrix(rnorm(n[i]*d),nr=n[i],nc=d)
#test decision is 1 (reject) or 0
    sktests[j] <- as.integer(sk(x,n[i]) >= cv[i] )
  }
  p.reject[i] <- mean(sktests) #proportion rejected
}
p.reject

## -----------------------------------------------------------------------------
#Type II error
alpha <- .1
n <- 30
m <- 200
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
N <- length(epsilon)
pwr <- numeric(N)
#critical value for the skewness test
cv <- (6/n)*qchisq(1-alpha, d*(d+1)*(d+2)/6) 
for (j in 1:N) { #for each epsilon
  e <- epsilon[j]
  sktests <- numeric(m)
for (i in 1:m) { #for each replicate
  sigma <- sample(c(1, 10), replace = TRUE, size = n, prob = c(1-e, e))
  x <- matrix(rnorm(n*d, 0, sigma),nr=n,nc=d)
  sktests[i] <- as.integer(sk(x,n) >= cv)
}
pwr[j] <- mean(sktests)
}
#plot power vs epsilon
plot(epsilon, pwr, type = "b",
xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)
pwr

## -----------------------------------------------------------------------------
library(boot)
data(scor,package = "bootstrap")
n <- nrow(scor)
mec <- scor$mec
vec <- scor$vec
alg <- scor$alg
ana <- scor$ana
sta <- scor$sta
x <- cbind(mec,vec,alg,ana,sta)
a <- eigen(cov(scor))
theta.hat <- max(a$val)/sum(a$val)
theta.hat

#jackknife
theta.jack <- numeric(n)
for (i in 1:n){
  scor_new <- cbind(mec[-i],vec[-i],alg[-i],ana[-i],sta[-i])
  a_new <- eigen(cov(scor_new))
  theta.jack[i] <- max(a_new$val)/sum(a_new$val)
}
bias <- (n-1)*(mean(theta.jack)-theta.hat)
se <- sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))
bias
se

#bootstrap
B <- 200
R <- numeric(B)
for(b in 1:B){
  i <- sample(1:n,size = n, replace = TRUE)
  mec <- scor$mec[i]
  vec <- scor$vec[i]
  alg <- scor$alg[i]
  ana <- scor$ana[i]
  sta <- scor$sta[i]
  scor_new0 <- cbind(mec,vec,alg,ana,sta)
  a_new0 <- eigen(cov(scor_new0))
  R[b] <- max(a_new0$val)/sum(a_new0$val)
}
bias.R <- mean(R-theta.hat)
bias.R
se.R <- sd(R)
se.R

eigenmax <- function(x,i){
  scor_new1 <- cbind(x[i,1],x[i,2],x[i,3],x[i,4],x[i,5])
  a_new1 <- eigen(cov(scor_new1))
  max(a_new1$val)/sum(a_new1$val)
}
obj <- boot(data = scor, statistic = eigenmax, R = 2000)
obj

#95%percentile
boot.ci(obj, type = c("basic", "norm", "perc"))

#BCa
boot.BCa <-
function(x, th0, th, stat, conf = .95) {
# bootstrap with BCa bootstrap confidence interval
# th0 is the observed statistic
# th is the vector of bootstrap replicates
# stat is the function to compute the statistic
  x <- as.matrix(x)
  n <- nrow(x) #observations in rows
  N <- 1:n
  alpha <- (1 + c(-conf, conf))/2
  zalpha <- qnorm(alpha)
  # the bias correction factor
  z0 <- qnorm(sum(th < th0) / length(th))
  # the acceleration factor (jackknife est.)
  th.jack <- numeric(n)
  for (i in 1:n) {
    J <- N[1:(n-1)]
    th.jack[i] <- stat(x[-i, ], J)
  }
  L <- mean(th.jack) - th.jack
  a <- sum(L^3)/(6 * sum(L^2)^1.5)
  # BCa conf. limits
  adj.alpha <- pnorm(z0 + (z0+zalpha)/(1-a*(z0+zalpha)))
  limits <- quantile(th, adj.alpha, type=6)
  return(list("est"=th0, "BCa"=limits))
}
boot.BCa(x, th0 = theta.hat, th = R, stat = eigenmax)

## -----------------------------------------------------------------------------
library(moments)
m<-100
B<-100
n<-50
SK<-function(x,i){
  skewness(x[i])
}
ci_basic_No<-ci_percent_No<-matrix(NA,m,2)
for (i in 1:m) {
  dt<-rnorm(n,mean = 0,sd=1)
  de<-boot(data=dt,statistic=SK,R=B)
  ci<-boot.ci(de,type=c("basic","perc"))
  ci_basic_No[i,]<-ci$basic[4:5]
  ci_percent_No[i,]<-ci$perc[4:5]
}
ci_basic_Chi<-ci_percent_Chi<-matrix(NA,m,2)
for (i in 1:m) {
  dt<-rchisq(n,5)
  de<-boot(data=dt,statistic=SK,R=B)
  ci<-boot.ci(de,type=c("basic","perc"))
  ci_basic_Chi[i,]<-ci$basic[4:5]
  ci_percent_Chi[i,]<-ci$perc[4:5]
}

r_No_Basic<-mean(ci_basic_No[,1]<0 & ci_basic_No[,2]>0)
r_No_percent<-mean(ci_percent_No[,1]<0 & ci_percent_No[,2]>0)
r_Chi_Basic<-mean(ci_basic_Chi[,1]>0)
r_Chi_percent<-mean(ci_percent_Chi[,1]>0)

Lable1<-c("r_No_Basic","r_No_percent","r_Chi_Basic","r_Chi_percent")
res1<-c(r_No_Basic,r_No_percent,r_Chi_Basic,r_Chi_percent)
re1<-data.frame(Lable1,res1)
re1

r1_NL <- mean(ci_basic_No[,1] > 0)
r1_NR <- mean(ci_basic_No[,2] < 0)
r2_NL <- mean(ci_percent_No[,1] > 0)
r2_NR <- mean(ci_percent_No[,2] < 0)
r1_ChiL <- mean(ci_basic_Chi[,1] < 0)
r2_ChiL <- mean(ci_percent_Chi[,1] < 0)

Lable2 <- c("NoSK_Left_Basic", "NoSK_Right_Basic", "NoSK_Left_percent", "NoSK_Right_percent", "ChiSK_Left_Basic", "ChiSK_Left_percent")
res2 <- c(r1_NL, r1_NR, r2_NL, r2_NR, r1_ChiL, r2_ChiL)
re2 <- data.frame(Lable2, res2)
re2

## -----------------------------------------------------------------------------
dt <- matrix(data=NA, nrow = 100, ncol = 2)
x <- dt[1:100, 1] <- rnorm(100, 0, 1)
y <- dt[1:100, 2] <- rnorm(100, 0, 1)
cor_res <- cor.test(x, y, method = "spearman")
cor0 <- cor_res$statistic
R <- 999 #number of replicates
z <- c(x, y) #pooled sample
K <- 1:200
reps <- numeric(R) #storage for replicates
for (i in 1:R) {
#generate indices k for the first sample
  k <- sample(K, size = 100, replace = FALSE)
  x1 <- z[k]
  y1 <- z[-k] #complement of x1
  reps[i] <- cor.test(x1, y1, method = "spearman")$statistic
}
p_hat <- mean(c(cor0, reps) >= cor0)
cor_res
p_hat

## -----------------------------------------------------------------------------
m <- 100
#unequal var & equal E
x <- rnorm(50, 0, 1)
y <- rnorm(50, 0, 2)
library(RANN) # implementing a fast algorithm
# for locating nearest neighbors
# (alternative R package: "yaImpute")
library(boot)
z <- c(x, y)
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ]
  NN <- nn2(data=z, k=k+1) # what's the first column?
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5)
  i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (k * n)
}
N <- c(length(x), length(y))
NN <- function(z, N, k){
  boot.obj <- boot(data = z, statistic = Tn, R = 99,
  sim = "permutation", sizes = N,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p <- mean(ts>=ts[1])
  p
}
p.value <- NN(z, N, 3)
p.value
library(energy)
boot.obs <- eqdist.etest(z, sizes=N, R=99)
p.value1 <- boot.obs$p.value
p.value1
library(Ball)
p.value2 <- bd.test(x = x, y = y, num.permutations=99)$p.value
p.value2
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- rnorm(50, 0, 1)
  y <- rnorm(50, 0, 2)
  z <- c(x, y)
  p.values[i,1] <- NN(z, N, 3)
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=99)$p.value
}
pow <- colMeans(p.values<0.05)
pow

#unequal var & unequal E
x <- rnorm(50, 0, 1)
y <- rnorm(50, 1, 2)
z <- c(x, y)
N <- c(length(x), length(y))
p.value <- NN(z, N, 3)
p.value
boot.obs <- eqdist.etest(z, sizes=N, R=99)
p.value1 <- boot.obs$p.value
p.value1
p.value2 <- bd.test(x = x, y = y, num.permutations=99)$p.value
p.value2
for(i in 1:m){
  x <- rnorm(50, 0, 1)
  y <- rnorm(50, 1, 2)
  z <- c(x, y)
  p.values[i,1] <- NN(z, N, 3)
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=99)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=99)$p.value
}
pow <- colMeans(p.values<0.05)
pow

#Non-normal distributions
x <- rt(50, 1)
y <- 0.3*rnorm(50, 0, 1) + 0.7*rnorm(50, 1, 3)
z <- c(x, y)
N <- c(length(x), length(y))
p.value <- NN(z, N, 3)
p.value
boot.obs <- eqdist.etest(z, sizes=N, R=99)
p.value1 <- boot.obs$p.value
p.value1
p.value2 <- bd.test(x = x, y = y, num.permutations=99)$p.value
p.value2
for(i in 1:m){
  x <- rt(50, 1)
  y <- 0.3*rnorm(50, 0, 1) + 0.7*rnorm(50, 1, 3)
  z <- c(x, y)
  p.values[i,1] <- NN(z, N, 3)
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=99)$p.value
}
pow <- colMeans(p.values<0.05)
pow

#Unbalanced samples
x <- rnorm(20, 0, 1)
y <- rnorm(100, 1, 1)
z <- c(x, y)
N <- c(length(x), length(y))
p.value <- NN(z, N, 3)
p.value
boot.obs <- eqdist.etest(z, sizes=N, R=99)
p.value1 <- boot.obs$p.value
p.value1
p.value2 <- bd.test(x = x, y = y, num.permutations=99)$p.value
p.value2
for(i in 1:m){
  x <- rnorm(20, 0, 1)
  y <- rnorm(100, 1, 1)
  z <- c(x, y)
  p.values[i,1] <- NN(z, N, 3)
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=99)$p.value
}
pow <- colMeans(p.values<0.05)
pow

## -----------------------------------------------------------------------------
# # data generation
# m <- 10000
# x <- numeric(m)
# x[1] <- rnorm(1,10)
# k <- 0
# u <- runif(m)
# for (i in 2:m) {
#   xt <- x[i-1]
#   y <- rnorm(1,xt)
#   num <- dcauchy(y) * dnorm(xt, y)
#   den <- dcauchy(xt) * dnorm(y, xt)
#   if (u[i] <= num/den) 
#     x[i] <- y 
#   else {
#     x[i] <- xt
#     k <- k+1 #y is rejected
#   }
# }
# print(k)/m
# index <- 1000:5000
# y1 <- x[index]
# plot(index, y1, type="l", main="", ylab="x")

## -----------------------------------------------------------------------------
# b <- 1000 #discard the burning sample
# y <- x[b:m]
# a <- seq(0.1, 0.9, 0.1)
# QR <- tan((a-0.5)*pi) #deciles of Cauchy
# Q <- quantile(x, a)
# qqplot(QR, Q, main="", xlab="Cauchy deciles", ylab="Sample deciles")
# y <- x <- seq(-4, 3, 0.01)
# lines(x,y)

## -----------------------------------------------------------------------------
gib.chain <-function(N,a1,a2){
  X <- matrix(0, N, 2)
  X[1,]<-c(a1,a2)
  for(i in 2:N){
    X[i,1] <- rbinom(1, 20, X[i-1,2])
    X[i,2] <- rbeta(1, X[i,1]+10, 30-X[i,1])
  }
  return(X)
}
# N <- 5000
# b <- 1000
# gib.chain(N, 0, 0)[(b+1):N,]

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) #row means
  B <- n * var(psi.means) #between variance est.
  psi.w <- apply(psi, 1, "var") #within variances
  W <- mean(psi.w) #within est.
  v.hat <- W*(n-1)/n + (B/n) #upper variance est.
  r.hat <- v.hat / W #G-R statistic
  return(r.hat)
}
#9.3 Gelman.Rubin
cauchy.chain <- function(N, x0) {
  x <- rep(0, N)
  x[1] <- x0
  u <- runif(N)
  for (i in 2:N) {
    xt <- x[i-1]
    y <- rnorm(1,xt,1)#candidate point
    r1 <- dcauchy(y) * dnorm(xt, y, 1)
    r2 <- dcauchy(xt) * dnorm(y, xt, 1)
    r <- r1 / r2
    if (u[i] <= r) 
      x[i] <- y 
    else
      x[i] <- xt
  }
  return(x)
}
# k <- 4 #number of chains to generate
# n <- 15000 #length of chains
# b <- 1000 #burn-in length
# #choose overdispersed initial values
# x0 <- c(-10, -5, 5, 10)
# #generate the chains
# Y <- matrix(0, nrow=k, ncol=n)
# for (i in 1:k)
#   Y[i, ] <- cauchy.chain(n, x0[i])
# #compute diagnostic statistics
# psi <- t(apply(Y, 1, cumsum))
# for (i in 1:nrow(psi))
#   psi[i,] <- psi[i,] / (1:ncol(psi))
# print(Gelman.Rubin(psi))
# #plot psi for the four chains
# plot(psi[1, (b+1):n], type="l")
# plot(psi[2, (b+1):n], type="l")
# plot(psi[3, (b+1):n], type="l")
# plot(psi[4, (b+1):n], type="l")
# rhat <- rep(0, n)
# for (j in (b+1):n)
#   rhat[j] <- Gelman.Rubin(psi[,1:j])
# plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
# 
# #9.8 Gelman.Rubin
# k <- 4
# n <- 15000
# b <- 1000
# #generate the chains
# X1 <- X2 <- matrix(0, nrow=k, ncol=n)
# for (i in 1:k){
#   X1[i, ] <- gib.chain(n, 0, 0)[,1]
#   X2[i, ] <- gib.chain(n, 0, 0)[,2]
# }
# #compute diagnostic statistics
# psi1 <- t(apply(X1, 1, cumsum))
# psi2 <- t(apply(X2, 1, cumsum))
# for (i in 1:nrow(psi)){
#   psi1[i,] <- psi1[i,] / (1:ncol(psi1))
#   psi2[i,] <- psi2[i,] / (1:ncol(psi2))
# }
# print(Gelman.Rubin(psi1))
# print(Gelman.Rubin(psi2))
# #plot psi for the four chains
# rhat1 <- rhat2 <- rep(0, n)
# for (j in (b+1):n){
#   rhat1[j] <- Gelman.Rubin(psi1[,1:j])
#   rhat2[j] <- Gelman.Rubin(psi2[,1:j])
# }
# plot(rhat1[(b+1):n], type="l", xlab="", ylab="R", main="x")
# plot(rhat2[(b+1):n], type="l", xlab="", ylab="R", main="y")

## -----------------------------------------------------------------------------
f_1 <- function(k,a,d){
  y <- (-1/2)^k/prod(1:k)*norm(a,"2")^(2*k+2)/((2*k+1)*(2*k+2))*gamma((d+1)/2)*gamma(k+3/2)/gamma(k+d/2+1)
  return(y)
}

f1_sum <- function(a,d){
  sum <- 0
  k <- 1
  y <- f_1(k,a,d)
  while(abs(y) > 1e-5){# condition for convergence (error)
    sum <- sum+y
    k <- k+1
    y <- f_1(k,a,d)
  }
  return(list(sum,k))
}

k_a <- f_1(10,c(1,2),2)# the kth of this sum
k_a
sum_a <- f1_sum(c(1,2),2)# print out the sum and k when it converges(error<1e-5).
sum_a

## -----------------------------------------------------------------------------
# 11.4/11.5
# s_k(a)
sk1 <- function(a,k){
  return(1-pt(sqrt(a^2*(k-1)/(k-a^2)),k-1))
}
k <- c(seq(4,25,1),100,500,1000)
res1 <- res2 <- length(k)
for (i in 1:length(k)) {
  a <- seq(0,sqrt(k[i]),0.005)
  p1 <- p2 <- numeric(length(a))
  for (j in 1:length(a)) {
    p1[j]<-sk1(a[j],k[i])
    p2[j]<-sk1(a[j],k[i]+1)
  }
  # the best choice for approximate equality and get its index
  index_min <- which.min(abs(p2[-1]-p1[-1]))
  res1[i] <- a[-1][index_min]
}
# two sides of the equation in 11.5 are pdf of t distribution with (k-1) and k degrees of freedom
for (i in 1:length(k)) {
  sk2 <- function(a) {
    int1 <- pt(sqrt(a^2*(k[i]-1)/(k[i]-a^2)),k[i]-1)
    int2 <- pt(sqrt(a^2*(k[i])/(k[i]+1-a^2)),k[i])
    return(int1-int2)
  }
  res2[i] <- uniroot(sk2,c(0.0001,sqrt(k[i])-0.0001))$root
}
res1
res2
error <- abs(res1-res2)
res <-as.data.frame(cbind(res1,res2,error))
res

## -----------------------------------------------------------------------------
m <- 200
lambda <- numeric(m)
lambda[1] <- 1
y <- c(0.54,0.48,0.33,0.43,1,1,0.91,1,0.21,0.85)
for(i in 2:m){
  lambda[i] <- 10/(sum(y)+3/lambda[i-1])
}
lambda_0 <- 7/sum(y)
lambda[m]
lambda_0

## -----------------------------------------------------------------------------
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)
lapply(trims, function(trim) mean(x, trim = trim))
lapply(trims, mean, x = x)

## -----------------------------------------------------------------------------
# 1.3
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
# With a for loop
out_1 <- vector("list", length(formulas))
i <- 1
for (formula in formulas) { 
  out_1[[i]] <- lm(formula, data = mtcars)
  i <- i + 1 
}
# With lapply
out_2 <- lapply(formulas, lm, data = mtcars)

# 1.4
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE)
mtcars[rows, ]
})
# With a for loop
out_3 <- vector("list", length(bootstraps))  
i <- 1
for (bootstrap in bootstraps) {
  out_3[[i]] <- lm(mpg ~ disp, data = bootstrap)
  i <- i + 1
}
# With lapply
out_4 <- lapply(bootstraps, lm, formula = mpg ~ disp)

# 1.5
rsq <- function(mod) summary(mod)$r.squared
outs <- list(out_1, out_2, out_3, out_4)
rsqs <- function(out) lapply(out, rsq)
lapply(outs, rsqs)

## -----------------------------------------------------------------------------
vapply(mtcars, sd, numeric(1)) # numeric data frame
sd0 <- function(data) vapply(data[vapply(data, is.numeric, logical(1))], sd, numeric(1))
sd0(USJudgeRatings) # mixed data frame

## -----------------------------------------------------------------------------
# mcapply()
mcsapply<-function(x,fun){
library(parallel)
  cl <- makeCluster(3)
  f <- parSapply(cl, x, fun)
  stopCluster(cl)
  return(f)
}
simulation <- replicate(500,
t.test(rnorm(10, 5, 10), rcauchy(10, 5)), simplify = FALSE)
list(system.time(mcsapply(simulation, function(x) {
  unlist(x)[3]
})),system.time(sapply(simulation, function(x) {
  unlist(x)[3]
})))

## -----------------------------------------------------------------------------
# R code
gib.chain <- function(a, b, n){
  N <- 5000
  burn <- 1000
  X <- matrix(0, N, 2)
  X[1,]<-c(0, 0)
  for(i in 2:N){
    X[i, 1] <- rbinom(1, n, X[i-1, 2])
    X[i, 2] <- rbeta(1, X[i, 1] + a, n + b - X[i, 1])
  }
  x <- X[(burn + 1):N, ]
 return(x)
}
# C++ code (Rcpp function)
library(Rcpp)
cppFunction('NumericMatrix gib_chain_rcpp(double a, double b, double m) {
  int N = 5000;
  int burn = 1000;
  NumericMatrix X(N, 2);
  NumericVector v = {0, 0};
  X(0, _) = v;
  for (int i = 1; i < N; i++) {
    double x1 = X(i - 1, 1);
    X(i, 0) = as<int>(rbinom(1, m, x1));
    int x0 = X(i, 0);
    X(i, 1) = as<double>(rbeta(1, x0 + a, m + b - x0));
  }
  NumericMatrix x = X(Range(burn - 1, N - 1), Range(0, 1));
  return x;
}')

## -----------------------------------------------------------------------------
# qqplot
a <- 10
b <- 10
n <- 20
X_R <- gib.chain(a, b, n)
X_C <- gib_chain_rcpp(a, b, n)
qqplot(X_R[, 1], X_C[, 1], xlab = "Discrete_R", ylab = "Discrete_Rcpp")
qqplot(X_R[, 2], X_C[, 2], xlab = "Continuous_R", ylab = "Continuous_Rcpp")

## -----------------------------------------------------------------------------
library(microbenchmark)
ts <- microbenchmark(ts1 <- gib.chain(a, b, n), ts2 <- gib_chain_rcpp(a, b, n))
ts

