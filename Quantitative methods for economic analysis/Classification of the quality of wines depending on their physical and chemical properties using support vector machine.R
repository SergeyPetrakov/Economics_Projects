#Импорт необходимых библиотек
library(readr)
library(corrplot)
library(Hmisc)
library("PerformanceAnalytics")
library(dplyr)

#install.packages('caret') # реализовано и деление выборки на тестовую и обучающую части и подбор параметров с помощью кросс-валидации 
library(caret)
library(ggplot2)
library(tictoc)
#install.packages('kernlab')
library(kernlab) 


#Установка правильной директории
setwd("~/Семестр - 6/Количественные методы/дз5")

############################
#Пункт 1
############################

#Импорт данных
winequality_red <- read_csv("~/Семестр - 6/Количественные методы/дз5/winequality-red.csv")
View(winequality_red)
summary(winequality_red)

#проверка отсутствия пропусков в данных
sum(is.na(winequality_red))

#Описательная статистика
#Средние значения
mean_variables <- data.frame(Characteristic=colnames(winequality_red), sapply(winequality_red, mean, na.rm=TRUE))
st_dev <- data.frame(Characteristic=colnames(winequality_red), sapply(winequality_red, sd, na.rm=TRUE))

names(st_dev)[2] <- "Standard deviation"
names(mean_variables)[2] <- "Mean"

statistics <- merge(x = mean_variables, y = st_dev, by = 'Characteristic')
format(statistics, digits = 2)

#Посмотрим на взаимосвязи характеристик
res <- cor(winequality_red)
round(res, 2)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
chart.Correlation(winequality_red, histogram=TRUE, pch=19)         


#Добавление бинарного показателя качества
plot(winequality_red$quality)
summary(winequality_red$quality)
plot(density(winequality_red$quality))
#определим высокое качество как всё, что выше или равно шести
winequality_red <- mutate(winequality_red, new_quality = floor(quality/6))
#оставим только новый критерий качества
df <- winequality_red %>% select(-quality)
df <- data.frame(df)
names(df)[12] <- "quality"




res2 <- cor(df)
round(res2, 2)

corrplot(res2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
chart.Correlation(df, histogram=TRUE, pch=19)     
#нет значительных изменений в корреляциях после преобразования переменной quality


#Теперь преобразуем нашу целевую переменную в факторную, чтобы модели работали 
#строго на классификацию (если этого не сделать, что SVM будет решать задачу регрессии)  
df$quality <- as.factor(df$quality)
levels(df$quality) <- c("0", "1")
##################################
#Пункт 2
##################################

#Разделение выборки на обучающую и тестовую

# Разделим выборку на две части — 70% для обучения и 30% для оценки качества обучения. 
# Умный R соблюдает бланс классов и в обучающей выборке 70% нулей и 70% единиц нашей целевой переменной quality
set.seed(13)
train.index <- createDataPartition(y = df$quality, p = 0.7, list = FALSE)
train.df <- df[train.index, ]
test.df <- df[-train.index, ]

# обращаем внимание на выбор ядра и его гиперпараметры (https://scikit-learn.org/stable/auto_examples/svm/plot_rbf_parameters.html) 
# а также на С 
# 
tic()
m1 <- ksvm(quality ~ ., data = train.df, kernel = "rbfdot", kpar = list(sigma = 2), C = 5)
toc()
m1
#
?ksvm
# теперь будем строить прогнозы для тестовой части выборки

test.df$quality.pred <- predict(m1, test.df)
table(test.df$quality.pred, test.df$quality)
View(test.df)
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
calc_class_err(test.df$quality.pred, test.df$quality)

#Проделаем для самопроверки те же операции в другом пакете - e1071, также 
#жто нужно чтобы ответить на вопрос о количесвте векторов в каждом классе
#так как пакет kernlab с его функцией ksvm умеет отвечать только на вопрос 
#о суммарном количестве опорных векторов
#install.packages('e1071')
library(e1071)
#в моём варианте рассматриваются экзогенные параметры cost = 5, gamma = 2 и гаусовское ядро по умолчанию
svm_model <- svm(quality ~ ., data=train.df, cost = 5, gamma = 2) 
# автоматически ошибка будет считаться, если выбирать параметр кросс-валидации
summary(svm_model)
# svm_model1 <- svm(x,y) -- или можно вот так, используя x, y
# summary(svm_model1)

# можно предсказывать классы и сравнивать, насколько хорошо мы это делаем
pred <- predict(svm_model,test.df)
table(pred,test.df$quality)

############################
#Пункт 3.
############################
#В данном пункте требуется построить ошибку предсказания на обучающей выборке с
#помощью кросс-валидации (параметр k𝑘)


# при фиксированных параметрах C и sigma будем менять количесвто фолдов для кросс валидации (k)
k_to_try = 2:40
err_k = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
  pred = ksvm(quality ~ alcohol+residual.sugar, data = train.df,
              kernel = "rbfdot", kpar = list(sigma = 2), C = 5, cross = k_to_try[i])
  err_k[i] = pred@cross
}
err_k
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k parameter", ylab = "cross validation error on train set",
     main = "cross validation error on train set vs k")

pred_for_my_var = ksvm(quality ~ alcohol+residual.sugar, data = train.df,
            kernel = "rbfdot", kpar = list(sigma = 2), C = 5, cross = 10)

cross_val_error = pred_for_my_var@cross
cross_val_error

################################
#Пункт 4.
################################
?tune
ctrl <- tune.control(sampling = "cross", cross = 10) # будем подбирать на основе кросс-валидации 
svm_tune <- tune(svm, quality~., data = train.df, kernel="radial", 
                 ranges=list(cost=seq(from = 1, to = 30, by = 1), gamma=seq(from = 0.1, to = 3, by = 0.1)),
                 tunecontrol = ctrl)
print(svm_tune)
tunedModel <- svm_tune$best.model
summary(tunedModel)



#sampling method: 10-fold cross validation
#cost gamma
#2   0.2
#best performance: 0.2267857
#Number of Support Vectors:  737
#( 353 384 )

###############################
#Пункт 5.продолжение оптимизации параметров
###############################

ctrl_1 <- tune.control(sampling = "cross", cross = 10) # будем подбирать на основе кросс-валидации 
svm_tune_1 <- tune(svm, quality~., data = train.df, kernel="radial", 
                 ranges=list(cost=2, gamma=0.2,epsilon = seq(from = 0.01, to = 0.05, by = 0.02),
                             tolarance = seq(from = 0.00001, to = 0.00005, by = 0.00002), 
                             cachesize = seq(from = 6, to = 15, by = 3)), tunecontrol = ctrl_1)
print(svm_tune_1)
tunedModel_1 <- svm_tune_1$best.model
summary(tunedModel_1)

#Разные результаты оптимизации моделей (просто менял параметры тут)
#best performance: 0.2321429 20-fold cross validation 
# cost gamma epsilon tolarance cachesize
#2   0.2    0.05     5e-04        20
#best performance: 0.2321429 10-fold cross validation 
#cost gamma epsilon tolarance cachesize
#2   0.2    0.05     5e-04        20
#
#best performance: 0.2358182 25-fold cross validation 
#cost gamma epsilon tolarance cachesize
#2   0.2    0.01     1e-04        10
#  
#
#best performance: 0.2348214 10-fold cross validation 
#cost gamma epsilon tolarance cachesize
#2   0.2    0.01     1e-05         6
#



ctrl_2 <- tune.control(sampling = "cross", cross = 10) # будем подбирать на основе кросс-валидации 
svm_tune_2 <- tune(svm, quality~., data = train.df, kernel="radial", 
                   ranges=list(cost=seq(from = 1.5, to = 2.5, by = 0.1), gamma=seq(from = 0.14, to = 0.26, by = 0.02)),
                   tunecontrol = ctrl_2)
print(svm_tune_2)
tunedModel_2 <- svm_tune_2$best.model
summary(tunedModel_2)

#- sampling method: 10-fold cross validation 
#
#- best parameters:
#  cost gamma
#   1.6  0.26
#
#- best performance: 0.2223214 




#Другие ядра (линейное)


ctrl_3 <- tune.control(sampling = "cross", cross = 10) # будем подбирать на основе кросс-валидации 
svm_tune_3 <- tune(svm, quality~., data = train.df, kernel="linear", 
                   ranges=list(cost=seq(from = 1.5, to = 2, by = 0.1), gamma=seq(from = 0.2, to = 0.3, by = 0.02)),
                   tunecontrol = ctrl_2)
print(svm_tune_3)
tunedModel_3 <- svm_tune_3$best.model
summary(tunedModel_3)

#- sampling method: 10-fold cross validation 
#
#- best parameters:
#  cost gamma
#   1.9   0.2
#
#- best performance: 0.2553571 
# Это хуже, чем гауссовское ядро

#Самый избирательный подход
ctrl_113 <- tune.control(sampling = "cross", cross = 13) # будем подбирать на основе кросс-валидации 
svm_tune_113 <- tune(svm, quality~., data = train.df, kernel="radial", 
                   ranges=list(cost=seq(from = 1.8, to = 2.4, by = 0.2),
                               gamma=seq(from = 0.18, to = 0.22, by = 0.02),
                               epsilon = seq(from = 0.01, to = 0.05, by = 0.02),
                               tolarance = seq(from = 0.00001, to = 0.00005, by = 0.00002), 
                               cachesize = seq(from = 2, to = 8, by = 2)), 
                   tunecontrol = ctrl_113)
print(svm_tune_113)
tunedModel_113 <- svm_tune_113$best.model
summary(tunedModel_113)
#Эта модель оказалась не так хороша, как tunedModel_2
  
#  - sampling method: 13-fold cross validation 
#
#- best parameters:
#  cost gamma epsilon tolarance cachesize
#2  0.22    0.01     1e-05         2
#
#- best performance: 0.2240968 
# Мы выбирали пока оптимальный С для фиксированного сигма, а надо бы
# выбрать из сетки значений по обоим параметрам -- пакет caret
ctrl <- trainControl(method = "repeatedcv")
fit <- train(quality ~ ., data = train.df, method = "svmRadial", trControl = ctrl)
fit


ctrl_3 <- trainControl(method = "optimism_boot")
fit_3 <- train(quality ~ ., data = train.df, method = "svmRadial", trControl = ctrl_3)
fit_3

Dh <- train.df[1:40,]
m3 <- ksvm(quality ~ alcohol+pH, data = Dh, kernel = "rbfdot", C = 1.9, kpar = list(sigma = 0.2))
m3_1 <- ksvm(quality ~ alcohol+chlorides, data = Dh, kernel = "rbfdot", C = 1.9, kpar = list(sigma = 0.2))
m3_2 <- ksvm(quality ~ alcohol+volatile.acidity, data = Dh, kernel = "rbfdot", C = 5, kpar = list(sigma = 2))


plot(m3, data = Dh)
plot(m3_1, data = Dh)
plot(m3_2, data = Dh)

