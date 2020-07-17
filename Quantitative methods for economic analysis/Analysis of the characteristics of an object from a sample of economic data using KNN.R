# Домашнее задание №1
setwd("~/Семестр - 6/Количественные методы")
#install.packages("FNN")
#install.packages("Hmisc")
#install.packages("pastecs")
#install.packages("psych")
#install.packages("corrplot")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("tictoc")
library(tictoc)
library(GGally)
library(ggplot2)
library(dplyr)
library(corrplot)
library(psych)
library(pastecs)
library(Hmisc)
library(FNN)
?get.knnx

# загрузка файла с данными
T1 <- read.csv("WHI2019.csv", sep=";", dec=".", header=TRUE)


# описательные статистики
summary(T1)
Hmisc::describe(T1)
stat.desc(T1)
psych::describe(T1)

res <- select(T1, -c(Country.or.region))
corrplot(cor(res))
ggcorr(res)
ggpairs(res)

#Задание 2
T1 <- data.frame(T1)

Poland <- T1[40,]
South_Sudan <- T1[156, ]
T1[40, ] <- South_Sudan
T1[156, ] <- Poland
T1[c(40,156),]
tail(T1)

t1 <- get.knnx(T1[1:155,4:9], query = T1[156,4:9], k = 5, algorithm = "kd_tree")
T1[t1$nn.index,]$Country.or.region
neighbors <- T1[t1$nn.index,]
developed_neighbors <- neighbors[(neighbors['Advanced'] == 1), ]
developed_neighbors$Country.or.region

#Задание 3 - время работы
# считаем время вычисления операции

tic("task 12")
t12 <- get.knnx(T1[1:155,4:9], query = T1[156,4:9], k = 5, algorithm = "kd_tree")
toc()

tic("task 13")
t12 <- get.knnx(T1[1:155,4:9], query = T1[156,4:9], k = 5, algorithm = "brute")
toc()

#Задание 4
# по ближайшим пяти соседям понять "класс" объекта (развитая/развивающаяся)

?knn
tic("task 22")
t22 <- knn(train = T1[1:155,4:9], test = T1[156,4:9], cl = as.factor(T1[1:155,10]), 
           k = 5, prob = TRUE, algorithm="kd_tree")
toc()

tic("task 222")
t222 <- knn(train = T1[1:155,4:9], test = T1[156,4:9], cl = as.factor(T1[1:155,10]), 
            k = 5, prob = TRUE, algorithm="brute")
toc()

t22 # в саммари есть prob -- возвращает вероятность
t222


# Задание 5. Посчитаем, насколько точно мы предсказали класс вообще
?knn.cv
tic("task 23")
t23 <- knn.cv(train = T1[1:155,4:9], cl = as.factor(T1[1:155,10]), 
              k = 5, prob = TRUE, algorithm="kd_tree")
toc()

tic("task 232")
t232 <- knn.cv(train = T1[1:155,4:9], cl = as.factor(T1[1:155,10]), 
              k = 5, prob = TRUE, algorithm="brute")
toc()

summary(t23)
attributes(t23)

predicted <- as.numeric(t23) - 1
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
calc_class_err( T1[1:155,10], predicted)

# как выбрать количество соседей для прогноза класса?
k_to_try = 1:25
err_k = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
  pred = knn.cv(train = T1[1:155,4:9], 
  cl = as.factor(T1[1:155,10]), 
  k = k_to_try[i], algorithm="kd_tree")
  predicted = as.numeric(pred) - 1
  err_k[i] = calc_class_err(T1[1:155,10], pred)
}
err_k
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors")

# Задание 6. кластеризовать данные методом k-средних -- это обучение без учителя

# Within Cluster Sum of Squares (WCSS), which measures the squared average distance 
# of all the points within a cluster to the cluster centroid -- это минимизируем
# Between Clusters Sum of Squares (BCSS), which measures the squared average distance 
# between all centroids -- это максимизируем
# totss -- total distance of all observations from the global center

# используем только "социальная поддержка" и "Ожидание здоровой жизни" , чтобы можно было нарисовать
library(FNN)
library(stats)
?kmeans
kmean <- kmeans(T1[1:156,5:6], centers = 4, nstart = 25, iter.max = 40, algorithm = "Lloyd")
kmean
kmean$cluster[156]

plot(T1[1:156,5:6], col =(kmean$cluster +1) , main="k-means result with 4 clusters", pch=1, cex=1, las=1)
points(T1[156,5:6], col = "black", pch = 3, cex = 2)
plot(T1[1:156,5:6], col =(kmean$cluster +1) , main="k-means result with 4 clusters", pch=1, cex=1, las=1)
points(kmean$centers, col = "black", pch = 17, cex = 2)
T1[156,5:6]
kmean$centers
kmean$cluster

t4 <- get.knnx(T1[1:156,5:6], query = kmean$centers, k = 1, algorithm = "kd_tree")
print((T1[t4$nn.index,1]))
T1[t4$nn.index,2]

# Задание 7. Кластеризуем страны на 4 кластера по укрупненным компонентам свободы
# на сколько кластеров делить?
kmeanC <- kmeans( T1[1:156,4:9], 4, iter.max = 40, algorithm = "Lloyd")
kmeanC
#определим новый кластер для Польши по всем компонентам
kmeanC$cluster[156]

#визуализируем кластеры 
plot(T1[1:156,4:9], col =(kmean$cluster +1) , main="k-means result with 4 clusters", pch=1, cex=1, las=1)
with(T1[1:156,4:9], pairs(T1[1:156,4:9], col=c(1:4)[kmeanC$cluster]))

#точность
kmean_acc <- kmeanC$betweenss/kmeanC$totss
kmean_acc

k_to_try = 2:15
acc_k = rep(x = 0, times = length(k_to_try))

# Задание 8. Минимизация суммарного внутригруппового расстояния
for (i in seq_along(k_to_try)) {
  pred = kmeans( T1[1:156,5:6], centers = k_to_try[i], iter.max = 40, algorithm = "Lloyd")
  acc_k[i] = pred$betweenss
}
acc_k
plot(acc_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number clusters", ylab = "betweeness distance",
     main = "(Test) betweeness distance vs Clusters")



