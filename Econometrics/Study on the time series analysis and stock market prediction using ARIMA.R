################################################
#Практическая работа по эконометрике
#выполнил студент э-301 Петраков Сергей
################################################
#Подключаем неоюходимые пакеты
#для радоты с временными рядами
library(zoo)
library(xts)
library(lubridate)
library(urca)
#Дополнительные пакеты
library(dplyr)
library(ggplot2)
library(forecast)
library(readxl)

#Импортируем данные 
ARIMA_301 <- read_excel("~/Семестр - 6/Эконометрика - 2/ARIMA_301.xlsx")
View(ARIMA_301)

#часть 1. ARIMA
Y5 <- ARIMA_301$Y5
#график
plot(Y5, type="line")
mean(Y5)
acf(Y5)
pacf(Y5)
acf(Y5,lag.max=6)
pacf(Y5, lag.max=6)
tsdisplay(Y5, lag.max = 6)


#Тестирование стационарности
#Тест Дики-Фуллера
df_test <- ur.df(Y5, type="none",lags = 0)
#этот тест выдаёт нестационарность
df_test <- ur.df(Y5, type="drift",selectlags = "BIC")
#результат - стационарность, хочется верить больше
summary(df_test)

#Другой вариант теста Дики-Фуллера
library(tseries)
another_df_test <- adf.test(y, k=3)
another_df_test
test_kpss <- kpss.test(y)
#нулевая гипотеза - ряд является тренд стационарным 
#(альтернативная гипотеза - нестационарность, крит. знач = 0,146)
#можно сделать нулевую гипотезу - стационарность, крит. знач = 0,463
test_kpss



#ARIMA подбор модели
?Arima
m1 <- Arima(Y5, order = c(2,0,0))
summary(m1)
#график коррелограммы остатков для модели m1
acf(m1$residuals)

#тест Льюинга-Бокса
#fitdf = p+q
resid_m1 <- resid(m1)
Box.test(resid_m1, lag = 10, type = "Ljung-Box", fitdf = 2)
#на 5% уровне получаем, что график остатков ведёт себя как белый шум, то есть 
#мы принимаем нулевую гипотезу теста Льюинга-Бокса

#Альтерантивным тестом проверки характера поведения остатков модели является 
#тест Бокса-Пирса, однако, на практике выборочные значения критерия часто отклоняются от 
#распределения Хи-квадрат, из-за чего его не рекомендуется применять

m2 <- Arima(Y5, order = c(1,0,0))
summary(m2)

#Вывод - модель AR(2) - лучшая



#Прогноз для Ar(2)
?forecast
prognoz <- forecast(m1,h=1)
prognoz
plot(prognoz)

#Автоматический подбор ARIMA модели auto.arima
best_model <- auto.arima(Y5)
summary(best_model)
#согласуется с нами

Y18 <- ARIMA_301$Y18
Y51 <- ARIMA_301$Y51

best_model <- auto.arima(Y18)
summary(best_model)
best_model <- auto.arima(Y51)
summary(best_model)


############################################
#Задание 2. Настоящие данные. Финансовые данные
############################################
library(quantmod)
Sys.setlocale("LC_TIME","C")
#загружаем данные
getSymbols(Symbols = "AAPL", from = "2019-03-04", to = "2020-06-04")
head(AAPL)
z <- AAPL$AAPL.Close
tsdisplay(z)
df_test <- ur.df(z, type="drift",selectlags = "BIC")
summary(df_test)
dz <- diff(z)
tsdisplay(dz)
auto.arima(z)
auto.arima(dz)
#ряд нестационарный
best_model <- auto.arima(dz)
summary(best_model)

prognoz <- forecast(auto.arima(dz),h=3)
prognoz

getSymbols(Symbols = "AAPL", from = "2019-06-04", to = "2020-09-04")
head(AAPL)
tail(AAPL)
z <- AAPL$AAPL.Close
dz <- diff(z)
tail(dz, 4)
