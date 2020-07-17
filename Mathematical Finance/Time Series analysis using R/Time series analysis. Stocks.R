#подгружаем необходимые пакеты
#install.packages("quantmod")
library(quantmod)

getSymbols("XOM", from="2016-01-01", to="2019-10-02")
getSymbols("VOW3.DE", from="2016-01-01", to="2019-10-02")
getSymbols("TCEHY", from="2016-01-01", to="2019-10-02")

#добавляем цены
prices_exxon_mobil <- as.numeric(Ad(XOM))
prices_volkswagen <- as.numeric(Ad(VOW3.DE))
prices_tencent_holdings <- as.numeric(Ad(TCEHY))

plot(prices_exxon_mobil, type="l", main="Цена акции Exxon Mobil")
plot(prices_volkswagen, type="l", main="Цена акции Volkswagen")
plot(prices_tencent_holdings, type="l", main="Цена акции Tencent Holdings")

#годоваядоходность
annual_terurn_exxon_mobil <- tail(prices_exxon_mobil, 1)/head(prices_exxon_mobil, 1) - 1
annual_terurn_volkswagen <- tail(prices_volkswagen, 1)/head(prices_volkswagen, 1) - 1
annual_terurn_tencent_holdings <- tail(prices_tencent_holdings, 1)/head(prices_tencent_holdings, 1) - 1

annual_return <- c(annual_terurn_exxon_mobil, 
                   annual_terurn_volkswagen,
                   annual_terurn_tencent_holdings)

annual_return

ret_exxon_mobil <- diff(prices_exxon_mobil)/head(prices_exxon_mobil, -1)
ret_volkswagen <- diff(prices_volkswagen)/head(prices_volkswagen, -1)
ret_tencent_holdings <- diff(prices_tencent_holdings)/head(prices_tencent_holdings, -1)


mean_of_returns_exxon_mobile <- mean(ret_exxon_mobil)
mean_of_returns_volkswagen <- mean(ret_volkswagen)
mean_of_returns_tencent_holdings <- mean(ret_tencent_holdings)

standart_deviation_of_returns_exxon_mobile <- sd(ret_exxon_mobil)
standart_deviation_of_returns_volkswagen <- sd(ret_volkswagen)
standart_deviation_of_returns_tencent_holdings <- sd(ret_tencent_holdings)

mean_of_returns <- c(mean_of_returns_exxon_mobile,
                     mean_of_returns_volkswagen,
                     mean_of_returns_tencent_holdings)

standart_deviation_of_returns <- c(standart_deviation_of_returns_exxon_mobile,
                                   standart_deviation_of_returns_volkswagen,
                                   standart_deviation_of_returns_tencent_holdings)

mean_of_returns
standart_deviation_of_returns

#коэф. Шарпа
Sharpe_ratio <- mean_of_returns/standart_deviation_of_returns
Sharpe_ratio


#Value-at-Risk
#квантиль на уровне 5%, то есть доходность бумаги 
#с 95% уровнем доверия будет не хуже, чем это значение


VaR_exxon_mobil <- sort(ret_exxon_mobil)[0.05*length(ret_exxon_mobil)]
VaR_volkswagen <- sort(ret_volkswagen)[0.05*length(ret_volkswagen)]
VaR_tencent_holdings <- sort(ret_tencent_holdings)[0.05*length(ret_tencent_holdings)]

VaR <- c(VaR_exxon_mobil,
         VaR_volkswagen,
         VaR_tencent_holdings)

#Expected shortfall - это среднее среди худшего 
#в тех процентах, что левее квантиля, употреблённого в VaR

ES_exxon_mobil <- mean(ret_exxon_mobil[ret_exxon_mobil <= quantile(ret_exxon_mobil, 0.05)])
ES_volkswagen <- mean(ret_volkswagen[ret_volkswagen <= quantile(ret_volkswagen, 0.05)])
ES_tencent_holdings <- mean(ret_tencent_holdings[ret_tencent_holdings <= quantile(ret_tencent_holdings, 0.05)])

ES <- c(ES_exxon_mobil,
        ES_volkswagen,
        ES_tencent_holdings)



#кривая VaR для Exxon Mobil
N <- 100 # Длина тренировочной выборки.
test_exxon_mobil <- ret_exxon_mobil[(N+1):length(ret_exxon_mobil)] # Тестовая выборка
VAR_exxon_mobil <- rep(0, length(test_exxon_mobil)) # Запишем сюда пока нули.
for (i in (N+1):length(ret_exxon_mobil)){
  train_exxon_mobil <- ret_exxon_mobil[(i-N):(i-1)]
  VAR_exxon_mobil[i-N] <- quantile(train_exxon_mobil, 0.05)
}

plot(test_exxon_mobil, type="l", main = "Кривая VaR для Exxon Mobil")
lines(VAR_exxon_mobil, col="red")

#кривая VaR для Volkswagen
N <- 100 # Длина тренировочной выборки.
test_volkswagen <- ret_volkswagen[(N+1):length(ret_volkswagen)] # Тестовая выборка
VAR_volkswagen <- rep(0, length(test_volkswagen)) # Запишем сюда пока нули.
for (i in (N+1):length(ret_volkswagen)){
  train_volkswagen <- ret_volkswagen[(i-N):(i-1)]
  VAR_volkswagen[i-N] <- quantile(train_volkswagen, 0.05)
}

plot(test_volkswagen, type="l", main = "Кривая VaR для Volkswagen")
lines(VAR_volkswagen, col="blue")

#кривая VaR для Tencent Holdings
N <- 100 # Длина тренировочной выборки.
test_tencent_holdings <- ret_tencent_holdings[(N+1):length(ret_tencent_holdings)] # Тестовая выборка
VAR_tencent_holdings <- rep(0, length(test_tencent_holdings)) # Запишем сюда пока нули.
for (i in (N+1):length(ret_tencent_holdings)){
  train_tencent_holdings <- ret_tencent_holdings[(i-N):(i-1)]
  VAR_tencent_holdings[i-N] <- quantile(train_tencent_holdings, 0.05)
}

plot(test_tencent_holdings, type="l", main = "Кривая VaR для Tencent Holdings")
lines(VAR_tencent_holdings, col="green")



#Построение кривой VaR для Exxon Mobil при предпосылке о нормальности распределения доходностей

quantile(ret_exxon_mobil, 0.05)
qnorm(0.05, mean=mean(ret_exxon_mobil), sd=sd(ret_exxon_mobil))

N <- 100
test_exxon_mobil_normal <- ret_exxon_mobil[(N+1):length(ret_exxon_mobil)]
VAR_exxon_mobil_normal <- rep(0, length(test_exxon_mobil_normal))
for (i in (N+1):length(ret_exxon_mobil)){
  train_exxon_mobil_normal <- ret_exxon_mobil[(i-N):(i-1)]
  VAR_exxon_mobil_normal[i-N] <- qnorm(0.05, mean=mean(train_exxon_mobil_normal), sd=sd(train_exxon_mobil_normal))
}
plot(test_exxon_mobil_normal, type="l", main = "VaR Exxon Mobil для нормальных доходностей")
lines(VAR_exxon_mobil_normal, col="red")

#Тест Купика
L_exxon_mobil_normal <- length(test_exxon_mobil_normal)
K_exxon_mobil_normal <- sum(test_exxon_mobil_normal < VAR_exxon_mobil_normal)
a0_exxon_mobil_normal <- K_exxon_mobil_normal/L_exxon_mobil_normal
a <- 0.05
S_exxon_mobil_normal <- 2*log( (1-a0_exxon_mobil_normal)^(L_exxon_mobil_normal-
                                                            K_exxon_mobil_normal) * 
                                 a0_exxon_mobil_normal^K_exxon_mobil_normal ) - 2*log( (1-a)^(L_exxon_mobil_normal-K_exxon_mobil_normal) * a^K_exxon_mobil_normal)
pval_exxon_mobile_normal <- 1 - pchisq(S_exxon_mobil_normal, 1)
pval_exxon_mobile_normal


#Построение кривой VaR для Volkswagen при предпосылке о нормальности распределения доходностей

quantile(ret_volkswagen, 0.05)
qnorm(0.05, mean=mean(ret_volkswagen), sd=sd(ret_volkswagen))

N <- 100
test_volkswagen_normal <- ret_volkswagen[(N+1):length(ret_volkswagen)]
VAR_volkswagen_normal <- rep(0, length(test_volkswagen_normal))
for (i in (N+1):length(ret_volkswagen)){
  train_volkswagen_normal <- ret_volkswagen[(i-N):(i-1)]
  VAR_volkswagen_normal[i-N] <- qnorm(0.05, mean=mean(train_volkswagen_normal), sd=sd(train_volkswagen_normal))
}
plot(test_volkswagen_normal, type="l", main = "VaR Volkswagen для нормальных доходностей")
lines(VAR_volkswagen_normal, col="blue")

#Тест Купика
L_volkswagen_normal <- length(test_volkswagen_normal)
K_volkswagen_normal <- sum(test_volkswagen_normal < VAR_volkswagen_normal)
a0_volkswagen_normal <- K_volkswagen_normal/L_volkswagen_normal
a <- 0.05
S_volkswagen_normal <- 2*log( (1-a0_volkswagen_normal)^(L_volkswagen_normal-K_volkswagen_normal) 
            * a0_volkswagen_normal^K_volkswagen_normal) - 2*log( (1-a)^(L_volkswagen_normal
                                                                        -K_volkswagen_normal)
                                                                 * a^K_volkswagen_normal )
pval_volkswagen_normal <- 1 - pchisq(S_volkswagen_normal, 1)
pval_volkswagen_normal

#Построение кривой VaR для Tencent Holdings при предпосылке о нормальности распределения доходностей

quantile(ret_tencent_holdings, 0.05)
qnorm(0.05, mean=mean(ret_tencent_holdings), sd=sd(ret_tencent_holdings))

N <- 100
test_tencent_holdings_normal <- ret_tencent_holdings[(N+1):length(ret_tencent_holdings)]
VAR_tencent_holdings_normal <- rep(0, length(test_tencent_holdings_normal))
for (i in (N+1):length(ret_tencent_holdings)){
  train_tencent_holdings_normal <- ret_tencent_holdings[(i-N):(i-1)]
  VAR_tencent_holdings_normal[i-N] <- qnorm(0.05, mean=mean(train_tencent_holdings_normal), sd=sd(train_tencent_holdings_normal))
}
plot(test_tencent_holdings_normal, type="l", main = "VaR Tencent Holdings для нормальных доходностей")
lines(VAR_tencent_holdings_normal, col="blue")

#Тест Купика
L_tencent_holdings_normal <- length(test_tencent_holdings_normal)
K_tencent_holdings_normal <- sum(test_tencent_holdings_normal < VAR_tencent_holdings_normal)
a0_tencent_holdings_normal <- K_tencent_holdings_normal/L_tencent_holdings_normal
a <- 0.05
S_tencent_holdings_normal <- 2*log( (1-a0_tencent_holdings_normal)^(L_tencent_holdings_normal-K_tencent_holdings_normal) 
                              * a0_tencent_holdings_normal^K_tencent_holdings_normal) - 2*log( (1-a)^(L_tencent_holdings_normal
                                                                                          -K_tencent_holdings_normal)
                                                                                   * a^K_tencent_holdings_normal )
pval_tencent_holdings_normal <- 1 - pchisq(S_tencent_holdings_normal, 1)
pval_tencent_holdings_normal


#Обобщённое гиперболическое распределение

#install.packages("ghyp")
library(ghyp)

#подгонка обобщённого геометрического распределения под Exxon Mobil
ghyp_dist_exxon_mobil <- fit.ghypuv(ret_exxon_mobil, silent = TRUE)
summary(ghyp_dist_exxon_mobil)

hist(ghyp_dist_exxon_mobil)
qqghyp(ghyp_dist_exxon_mobil)
aic_exxon_mobil <- stepAIC.ghyp(ret_exxon_mobil, dist=c("ghyp", "hyp", "t", "gauss"), silent=TRUE)
aic_exxon_mobil$best.model

qghyp(0.05, object = aic_exxon_mobil$best.model)

#ждать минуту
N <- 100
test_exxon_mobil_ghyp <- ret_exxon_mobil[(N+1):length(ret_exxon_mobil)]
VaR_exxon_mobil_ghyp <- rep(0, length(test_exxon_mobil_ghyp))
for (i in (N+1):length(ret_exxon_mobil)){
  train_exxon_mobil_ghyp <- ret_exxon_mobil[(i-N):(i-1)]
  model_exxon_mobil_ghyp <- stepAIC.ghyp(train_exxon_mobil_ghyp, dist=c("ghyp", "hyp", "t", "gauss"), silent=T)$best.model
  VaR_exxon_mobil_ghyp[i-N] <- qghyp(0.05, object = model)
}
plot(test_exxon_mobil_ghyp, type="l", main = "Кривая VaR Exxon Momil (GHYP) ")
lines(VaR_exxon_mobil_ghyp, col="red")

L_exxon_mobil_ghyp <- length(test_exxon_mobil_ghyp)
K_exxon_mobil_ghyp <- sum(test_exxon_mobil_ghyp < VaR_exxon_mobil_ghyp)
a0_exxon_mobil_ghyp <- K_exxon_mobil_ghyp/L_exxon_mobil_ghyp
a <- 0.05
S_exxon_mobil_ghyp <- 2*log( (1-a0_exxon_mobil_ghyp)^(L_exxon_mobil_ghyp-K_exxon_mobil_ghyp) *
                               a0_exxon_mobil_ghyp^K_exxon_mobil_ghyp ) - 2*log( (1-a)^(L_exxon_mobil_ghyp-
                                                                                          K_exxon_mobil_ghyp)
                                                                                 * a^K_exxon_mobil_ghyp )
pval_exxon_mobil_ghyp <- 1 - pchisq(S_exxon_mobil_ghyp, 1)
pval_exxon_mobil_ghyp


#подгонка обобщённого геометрического распределения под Volkswagen 

ghyp_dist_volkswagen <- fit.ghypuv(ret_volkswagen, silent = TRUE)
summary(ghyp_dist_volkswagen)

hist(ghyp_dist_volkswagen)
qqghyp(ghyp_dist_volkswagen)
aic_volkswagen <- stepAIC.ghyp(ret_volkswagen, dist=c("ghyp", "hyp", "t", "gauss"), silent=TRUE)
aic_volkswagen$best.model

qghyp(0.05, object = aic_volkswagen$best.model)

#ждать минуту
N <- 100
test_volkswagen_ghyp <- ret_volkswagen[(N+1):length(ret_volkswagen)]
VaR_volkswagen_ghyp <- rep(0, length(test_volkswagen_ghyp))
for (i in (N+1):length(ret_volkswagen)){
  train_volkswagen_ghyp <- ret_volkswagen[(i-N):(i-1)]
  model_volkswagen_ghyp <- stepAIC.ghyp(train_volkswagen_ghyp, dist=c("ghyp", "hyp", "t", "gauss"), silent=T)$best.model
  VaR_volkswagen_ghyp[i-N] <- qghyp(0.05, object = model)
}
plot(test_volkswagen_ghyp, type="l", main = "Кривая VaR Exxon Momil (GHYP) ")
lines(VaR_volkswagen_ghyp, col="red")

L_volkswagen_ghyp <- length(test_volkswagen_ghyp)
K_volkswagen_ghyp <- sum(test_volkswagen_ghyp < VaR_volkswagen_ghyp)
a0_volkswagen_ghyp <- K_volkswagen_ghyp/L_volkswagen_ghyp
a <- 0.05
S_volkswagen_ghyp <- 2*log( (1-a0_volkswagen_ghyp)^(L_volkswagen_ghyp-K_volkswagen_ghyp) *
                               a0_volkswagen_ghyp^K_volkswagen_ghyp ) - 2*log( (1-a)^(L_volkswagen_ghyp-
                                                                                          K_volkswagen_ghyp)
                                                                                 * a^K_volkswagen_ghyp )
pval_volkswagen_ghyp <- 1 - pchisq(S_volkswagen_ghyp, 1)
pval_volkswagen_ghyp

#подгонка обобщённого геометрического распределения под Tencent Holdings

ghyp_dist_tencent_holdings <- fit.ghypuv(ret_tencent_holdings, silent = TRUE)
summary(ghyp_dist_tencent_holdings)

hist(ghyp_dist_tencent_holdings)
qqghyp(ghyp_dist_tencent_holdings)
aic_tencent_holdings <- stepAIC.ghyp(ret_tencent_holdings, dist=c("ghyp", "hyp", "t", "gauss"), silent=TRUE)
aic_tencent_holdings$best.model

qghyp(0.05, object = aic_tencent_holdings$best.model)

#ждать минуту
N <- 100
test_tencent_holdings_ghyp <- ret_tencent_holdings[(N+1):length(ret_tencent_holdings)]
VaR_tencent_holdings_ghyp <- rep(0, length(test_tencent_holdings_ghyp))
for (i in (N+1):length(ret_tencent_holdings)){
  train_tencent_holdings_ghyp <- ret_tencent_holdings[(i-N):(i-1)]
  model_tencent_holdings_ghyp <- stepAIC.ghyp(train_tencent_holdings_ghyp, dist=c("ghyp", "hyp", "t", "gauss"), silent=T)$best.model
  VaR_tencent_holdings_ghyp[i-N] <- qghyp(0.05, object = model)
}
plot(test_tencent_holdings_ghyp, type="l", main = "Кривая VaR Tencent Holdings (GHYP) ")
lines(VaR_tencent_holdings_ghyp, col="red")

L_tencent_holdings_ghyp <- length(test_tencent_holdings_ghyp)
K_tencent_holdings_ghyp <- sum(test_tencent_holdings_ghyp < VaR_tencent_holdings_ghyp)
a0_tencent_holdings_ghyp <- K_tencent_holdings_ghyp/L_tencent_holdings_ghyp
a <- 0.05
S_tencent_holdings_ghyp <- 2*log( (1-a0_tencent_holdings_ghyp)^(L_tencent_holdings_ghyp-K_tencent_holdings_ghyp) *
                              a0_tencent_holdings_ghyp^K_tencent_holdings_ghyp ) - 2*log( (1-a)^(L_tencent_holdings_ghyp-
                                                                                       K_tencent_holdings_ghyp)
                                                                              * a^K_tencent_holdings_ghyp )
pval_tencent_holdings_ghyp <- 1 - pchisq(S_tencent_holdings_ghyp, 1)
pval_tencent_holdings_ghyp
