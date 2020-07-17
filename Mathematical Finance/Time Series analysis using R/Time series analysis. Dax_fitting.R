#install.packages("FinTS")
#install.packages("tseries")
#install.packages("fGarch")
#install.packages("datasets")
library(datasets)
library(FinTS)
library(fGarch)
library(tseries)



#LM-тест в R
# исходные данные
dax <- EuStockMarkets[,"DAX"]
T <- length(dax)-1
dax <- dax[2:(T+1)]/dax[1:T] - 1


## LM-тест

ArchTest(dax,lags=12)
#ARCH LM-test; Null hypothesis: no ARCH effects



#оценка параметров модели
dax.gfit <- garchFit(formula=~aparch(1,1),data=dax,delta=2,
                     include.delta=FALSE,leverage=TRUE,cond.dist="sged",
                     shape=1.25,include.shape=FALSE,trace=FALSE)
plot(dax.gfit,which=11)

#Тесты на единичный корень в R


adf.test(dax)
pp.test(dax)
kpss.test(dax, null="Level")

#Прогноз по модели ARMA-GARCH
# прогноз среднего и дисперсии на i шагов вперёд
dax.frc <- predict(dax.gfit,n.ahead=i)

# расчёт границы потерь
alpha <- 0.05
VaR <- dax.frc[1,1]+dax.frc[1,3]*qged(alpha,mean=0,sd=1, nu=dax.gfit@fit$par["shape"])

#Кривая VaR — набор последовательных во времени значений VaR
T1 <- 6*260; T2 <- T - T1 # обучающая и экзаменующая выборки

# на пространстве экзаменующей выборки построим набор
# последовательных значений VaR

VaR <- numeric()
h <- 0.5*260
for (i in (T1+1):(T1+T2)) {
  h.dax <- dax[(i-h):(i-1)]
  dax.gfit <- garchFit(formula=~aparch(1,1),data=h.dax,
                       delta=2,include.delta=FALSE,leverage=TRUE,cond.dist="sged",
                       shape=1.5,include.shape=FALSE,trace=FALSE)
  dax.frc <- predict(dax.gfit,n.ahead=1)
  VaR[i-T1] <- dax.frc[1,1]+dax.frc[1,3]*qsged(alpha,mean=0,sd=1,
                                               nu=1.5,xi=dax.gfit@fit$par["skew"])
}

# сравнение оценок риска с фактом

fact <- dax[(T1+1):(T1+T2)]
plot(fact,type="l")
lines(VaR,col="red")

#Тест Купика
#Идея состоит в сравнении модельной и эмпирической частот превышений фактическими убытками границы VaR



# тест Купика в R:
K <- sum(fact<VaR); alpha0 <- K/T2
S <- -2*log((1-alpha)^(T2-K)*alpha^K)+
  2*log((1-alpha0)^(T2-K)*alpha0^K)
p.value <- 1-pchisq(S,df=1)

p.value


#Модель «copula–GARCH» в R

# одномерные GARCH-модели
library(fGarch)
dax.gfit <- garchFit(data=dax,formula=~garch(1,1),
                     shape=1.25,include.shape=F,cond.dist="ged",trace=F)
smi.gfit <- garchFit(data=smi,formula=~garch(1,1),
                     shape=1.3,include.shape=F,cond.dist="sged",trace=F)

#стандартизированные остатки
z <- matrix(nrow=T,ncol=2)
z[,1] <- dax.gfit@residuals / dax.gfit@sigma.t
z[,2] <- smi.gfit@residuals / smi.gfit@sigma.t
# частные распределения остатков
mean <- c(0,0); sd <- c(1,1); nu <- c(1.25,1.3)
xi <- c(1, smi.gfit@fit$par["skew"])
cdf <- matrix(nrow=T,ncol=2)
for (i in 1:2) cdf[,i] <- psged(z[,i],mean=mean[i],
                                sd=sd[i],nu=nu[i],xi=xi[i])

# подгонка копул
norm.fit <- fitCopula(cdf,copula=norm.cop)
stud.fit <- fitCopula(cdf,copula=stud.cop)
gumb.fit <- fitCopula(cdf,copula=gumb.cop)
clay.fit <- fitCopula(cdf,copula=clay.cop)
# метод Монте-Карло
cdf.sim <- rcopula(n=N,copula=stud.fit@copula)
z.sim <- matrix(nrow=N,ncol=2)
for (i in 1:2) z.sim[,i] <- qsged(cdf.sim[,i],
                                  mean=mean[i],sd=sd[i],nu=nu[i],xi=xi[i])
frc1 <- predict(dax.gfit,n.ahead=1)
frc2 <- predict(smi.gfit,n.ahead=1)
mu <- c(frc1[,1],frc2[,1])
sigma <- c(frc1[,3],frc2[,3])

#Оценка финансового риска
# модельные доходности портфеля
prt.sim <- w[1]*(mu[1]+sigma[1]*z.sim[,1]) +
  w[2]*(mu[2]+sigma[2]*z.sim[,2])

# измерители риска
prt.sim <- sort(prt.sim)
VaR <- prt.sim[alpha*N]
ES <- mean(prt.sim[1:(alpha*N-1)])

