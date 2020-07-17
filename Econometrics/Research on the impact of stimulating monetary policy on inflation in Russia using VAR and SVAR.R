#Загрузка необходимых пакетов
library("zoo")
library("xts")
library("lubridate")
library("urca")
library("dplyr")
library("ggplot2")
library("forecast")
library(car)
library(tseries)
library(vars)
library(readxl)
library(readr)
library("stargazer") 
library("lmtest")
library("sandwich")
library(tidyverse)
library(writexl)
#Чтение данных
the_way <- '~/Семестр - 6/Эконометрика - 2/дз по таргетированию инфляции'
setwd(the_way)

df <- read_xlsx("data.xlsx", sheet = "data", range = "A3:I64", col_names = TRUE, col_types = NULL,
               na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")

df <- data.frame(df)
#df_quarterly <- na.omit(df)
#writexl::write_xlsx(df_quarterly, "./df_quarterly.xlsx")


GDP <- ts(df$index_gdp, start = c(2014, 12), frequency = 12)
i <- ts(df$interest_rate, start = c(2015, 1), frequency = 12)
CPI <- ts(df$cpi, start = c(2015, 1), frequency = 12)
E <- ts(df$exchange_rate, start = c(2015, 1), frequency = 12)
oil <- ts(df$oil_price, start = c(2015, 1), frequency = 12)
u <- ts(df$unemployment_rate, start = c(2015, 1), frequency = 12)
dGDP <- diff(GDP)




summary(ur.df(dGDP, type = "trend",  selectlags = "BIC"))
GDP
G <- decompose(GDP)
ir <- decompose(i)
C <- decompose(CPI)
Er <- decompose(E)
oil_p <- decompose(oil)
ur <- decompose(u)

plot(G)
title(main="
      GDP")
plot(ir)
title(main="
      interest rate")
plot(C)
title(main="
      CPI")
plot(Er)
title(main="
      exchange rate")
plot(oil_p)
title(main="
      oil price")
plot(ur)
title(main="
      unemployment rate")

T_GDP <- GDP - G$seasonal
T_i <- i - ir$seasonal
T_CPI <- CPI - C$seasonal
T_E <-  E - Er$seasonal
T_oil <- oil - oil_p$seasonal
T_u <- u - ur$seasonal

T_data <- data.frame(GDP=T_GDP, CPI=T_CPI, i=T_i, E=T_E, oil=T_oil, u=T_u)
T_datat <- ts(T_data, start = c(2014, 12), frequency = 12)
plot(T_datat)
T_datat
#Дики-Фуллер обычный
df_gdp <- ur.df(T_data$GDP,type="drift",selectlags="BIC")
summary(df_gdp)
#не стационарен
df_cpi <- ur.df(T_data$CPI,type="drift",selectlags="BIC") 
summary(df_cpi)
#не стационарен 
df_i <- ur.df(T_data$i,type="drift", selectlags="BIC") 
summary(df_i)
#стационарен при 1% уровне значимости
df_E <- ur.df(T_data$E,type="drift", selectlags="BIC") 
summary(df_E)
#стационарен при 5% уровне значимости
df_oil <- ur.df(T_data$oil,type="drift", selectlags = "BIC") 
summary(df_oil)
#не стационарен (для случая с trend аналогично)
df_u <- ur.df(T_data$u,type="drift", selectlags = "BIC") 
summary(df_u)
df_u <- ur.df(T_data$u,type="trend", selectlags = "BIC") 
summary(df_u)
#не стационарен

#произведём переход к стационарным разностям для нестационарных переменных
#Переход к первым разностям
dGDP <- diff(T_data$GDP)
#относительная динамика индекса ВВП в ценах 2000 года к предыдущему месяцу

dcpi <- diff(T_data$CPI)
#месячная инфляция 

#di <- diff(T_data$i)
i <- T_data$i
#Ruonia на момент времени

#dE <- diff(T_data$E)
E <- T_data$E
#курс рубля

doil <- diff(T_data$oil)
#прирост цены на нефть за месяц

du <- diff(T_data$u)
#месячное изменение безработицы в п.п.

#Дики-Фуллер для первых разностей (Ряд интегрирован 1 порядка)
df_gdp <- ur.df(dGDP,type="drift",selectlags="BIC")
summary(df_gdp)
#стационарен при 1% уровне значимости
df_cpi <- ur.df(dcpi,type="drift", selectlags = "BIC") 
summary(df_cpi)
#стационерна при 1% уровне значимости
#df_i <- ur.df(di,type="none")
#summary(df_i)
#df_E <- ur.df(dE,type="none") 
#summary(df_E)
df_oil <- ur.df(doil,type="none") 
summary(df_oil)
#стационарен при 1% уровне значимости
df_u <- ur.df(du,type="none") 
summary(df_u)
#стационарен при 5% уровне значимости

##Полагая, что мы рассматриваем переменные месячная инфляция, курс рубля, 
##относительная динамика индекса ВВП в ценах 2000 года к предыдущему месяцу,
##Ruonia на момент времени (ставка процента), прирост цены на нефть за месяц,
##месячное изменение безработицы в п.п.
##ТАК КАК РАССМАТРИВАЕМ ТЕПЕРЬ ЭТИ ПЕРЕМЕННЫЕ КАК ОТДЕЛЬНЫЕ ВЕЛИЧИНЫ, ТО МОЖНО ГОВОРИТЬ
##ОБ ОДНОМ ПОРЯДКЕ ИНТЕГРИРОВАННОСТЬ (в этом, новом рассмотрении это I(0))


Ddata <- data.frame(i=i, E=E)
Ddata <- Ddata[2:61,]
Ddata1 <- data.frame(dGDP = dGDP, dcpi = dcpi, doil = doil, du = du)
Ddata
rownames(Ddata) <- 1:nrow(Ddata)
Ddata <- data.frame(Ddata, Ddata1)


Ddata
Ddatat <- ts(Ddata, start = c(2015, 1), frequency = 12)
plot(Ddatat)
Ddatat
Ddata$doil
df_wo_oil <- Ddata[c('i','E','dGDP','dcpi','du')]
df_wo_oil
df_only_oil <- Ddata['doil']
df_only_oil
#colnames(df_only_oil$doil)
#exogen_df <- data.frame(doil = df_only_oil$doil)
#exogen_df
#cbind(x3=df_only_oil$doil)
#Моделирование VAR
VARselect(Ddata, lag.max = 8, type = "both")#,exogen = cbind(doil = doil)) #lag = 1
varm <- VAR(Ddata, p=1, type="const")
summary(varm)



roots(varm) #Модель стабильна
test1 <- serial.test(varm, type = "PT.adjusted") #Нет АК остатков
test1
# The null hypothesis of no autocorrelation is NOT rejected

#test2 <- serial.test(varm, type = "ES") #Точно нет АК остатков
#test2
#?serial.test
#test3 <- arch.test(varm) #Нет ARCH
#test3

#Моделирование SVAR
#классический монетарный VAR
#Матрица ограничений 1 - delta_GDP; 2 - inflation; 3 - interest rate;
amat <- diag(3)
amat
diag(amat) <- NA
amat[2, 1] <- NA
amat[3, 1] <- NA
amat[3, 2] <- NA

amat
Ddata_0 <- Ddata[c('dGDP','dcpi','i')]
VARselect(Ddata_0, lag.max = 8, type = "both")
varm_0_1 <- VAR(Ddata_0, p=1, type="const")
summary(varm_0_1)

VARselect(Ddata_0, lag.max = 8, type = "both")
varm_0_5 <- VAR(Ddata_0, p=5, type="const")
summary(varm_0_5)

roots(varm_0_5) #Модель стабильна
test1 <- serial.test(varm_0_5, type = "PT.adjusted") #Нет АК остатков
test1
# The null hypothesis of no autocorrelation is NOT rejected

varm_1_6 <- VAR(Ddata_1, p=6, type="const")
summary(varm_1_6)

final <- SVAR(varm_0_5, estmethod = "direct", Amat=amat, method="BFGS")
summary(final)

#Функции отклика
irf <- irf(varm_0_5, impulse="i", response = "dcpi", n.ahead=6, boot ="true", ci=0.9)
sirf <- irf(final, impulse="i", response = "dcpi", n.ahead=6, boot ="true", ci=0.9)
plot(irf)
plot(sirf)

#Кумулитивные функции отклика
irf <- irf(varm_0_1, impulse="i", response = "dcpi", n.ahead=6, boot ="true", cumulative = "true", ci=0.9)
sirf <- irf(final, impulse="i", response = "dcpi", n.ahead=6, boot ="true", cumulative = "true", ci=0.9)
plot(irf)
plot(sirf)

#классический монетарный VAR с добавлением нефтяных шоков
#Матрица ограничений 1 - delta_GDP; 2 - inflation; 3 - interest rate; 4 - delta_oil;

amat <- diag(4)
amat
diag(amat) <- NA

amat[1, 1] <- NA
amat[2, 1] <- NA
amat[3, 1] <- NA
amat[2, 2] <- NA
amat[3, 2] <- NA
amat[1, 4] <- NA
amat[3, 4] <- NA

amat
Ddata_1 <- Ddata[c('dGDP','dcpi','i','doil')]

VARselect(Ddata_1, lag.max = 8, type = "both")
varm_1_1 <- VAR(Ddata_1, p=1, type="const")
summary(varm_1_1)

roots(varm_1_1) #Модель стабильна
test1 <- serial.test(varm_1_1, type = "PT.adjusted") #Нет АК остатков
test1

varm_1_6 <- VAR(Ddata_1, p=6, type="const")
summary(varm_1_6)

varm_1_4 <- VAR(Ddata_1, p=4, type="const")
summary(varm_1_4)

varm_1_5 <- VAR(Ddata_1, p=5, type="const")
summary(varm_1_5)


roots(varm_1_4) #Модель стабильна
test4 <- serial.test(varm_1_4, type = "PT.adjusted") #Нет АК остатков
test4

roots(varm_1_5) #Модель стабильна
test5 <- serial.test(varm_1_5, type = "PT.adjusted") #Нет АК остатков
test5

final <- SVAR(varm_1_4, estmethod = "direct", Amat=amat, method="BFGS")
summary(final)

#Функции отклика

varm
?irf
irf <- irf(varm_1_4, impulse="i", response = "dcpi", n.ahead=18, boot ="true", ci=0.9)
irf_12 <- irf(varm_1_4, impulse="i", response = "dcpi", n.ahead=12, boot ="true", ci=0.9)
sirf <- irf(final, impulse="i", response = "dcpi", n.ahead=18, boot ="true", ci=0.9)
plot(irf)
plot(irf_12)
plot(sirf)

#Кумулитивные функции отклика
irf_18 <- irf(varm_1_4, impulse="i", response = "dcpi", n.ahead=18, boot ="true", cumulative = "true", ci=0.9)
sirf_18 <- irf(final, impulse="i", response = "dcpi", n.ahead=18, boot ="true", cumulative = "true", ci=0.7)
sirf_18_1 <- irf(final, impulse="i", response = "dcpi", n.ahead=18, boot ="true", cumulative = "true", ci=0.9)
plot(irf_18)
plot(sirf_18)
plot(sirf_18_1)






























#Красткосрочный период
#Матрица ограничений 1 - oil; 2 - CPI; 3 - E; 4 - i; 5 - GDP; 6 - u
amat <- diag(6)
amat
diag(amat) <- NA

amat[3, 1] <- NA
amat[5, 1] <- NA
amat[5, 1] <- NA
amat[5, 2] <- NA
amat[6, 2] <- NA
amat[3, 3] <- NA
amat[4, 3] <- NA
amat[5, 3] <- NA
amat[3, 4] <- NA
amat[5, 4] <- NA
amat[6, 4] <- NA
amat[2, 5] <- NA
amat[4, 5] <- NA
amat[5, 5] <- NA
amat[6, 5] <- NA
amat[4, 6] <- NA
amat[5, 6] <- NA



VARselect(Ddata, lag.max = 8, type = "both")
varm_3 <- VAR(Ddata, p=1, type="const")
summary(varm_3)

roots(varm_3) #Модель стабильна
test1 <- serial.test(varm_3, type = "PT.adjusted") #Нет АК остатков
test1
# The null hypothesis of no autocorrelation is NOT rejected



final <- SVAR(varm_3, estmethod = "direct", Amat=amat, method="BFGS")
summary(final)
#Функции отклика

varm
irf <- irf(varm_3, impulse="i", response = "dcpi", n.ahead=3, boot ="true", ci=0.9)
sirf <- irf(final, impulse="i", response = "dcpi", n.ahead=3, boot ="true", ci=0.9)
plot(irf)
plot(sirf)

#Кумулитивные функции отклика
irf <- irf(varm, impulse="di", response = "dcpi", n.ahead=6, boot ="true", cumulative = "true", ci=0.9)
sirf <- irf(final, impulse="dcpi", response = "di", n.ahead=12, boot ="true", cumulative = "true", ci=0.9)
plot(irf)
plot(sirf)


#Матрица ограничений 1 - oil; 2 - CPI; 3 - E; 4 - i; 5 - GDP; 6 - u
amat <- diag(6)
amat
diag(amat) <- NA

amat[2, 1] <- NA
amat[3, 2] <- NA
amat[4, 2] <- NA
amat[2, 3] <- NA
amat[2, 4] <- NA
amat[2, 5] <- NA
amat[3, 4] <- NA
amat[3, 5] <- NA
amat[4, 5] <- NA
amat[5, 4] <- NA
amat[6, 4] <- NA
amat[6, 5] <- NA
amat[2, 6] <- NA
amat[3, 6] <- NA
amat[6, 3] <- NA
amat[5, 6] <- NA


amat
VARselect(Ddata, lag.max = 8, type = "both")
varm_4 <- VAR(Ddata, p=1, type="const")
summary(varm_4)

roots(varm_4) #Модель стабильна
test1 <- serial.test(varm_4, type = "PT.adjusted") #Нет АК остатков
test1
# The null hypothesis of no autocorrelation is NOT rejected



final <- SVAR(varm_4, estmethod = "direct", Amat=amat, method="BFGS")
summary(final)
#Функции отклика

varm
irf <- irf(varm_4, impulse="i", response = "dcpi", n.ahead=12, boot ="true", ci=0.9)
sirf <- irf(final, impulse="i", response = "dcpi", n.ahead=12, boot ="true", ci=0.9)
plot(irf)
plot(sirf)

#Кумулитивные функции отклика
irf <- irf(varm, impulse="di", response = "dcpi", n.ahead=6, boot ="true", cumulative = "true", ci=0.9)
sirf <- irf(final, impulse="dcpi", response = "di", n.ahead=12, boot ="true", cumulative = "true", ci=0.9)
plot(irf)
plot(sirf)

