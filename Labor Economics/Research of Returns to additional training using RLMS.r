install.packages("tidyverse")
install.packages("Hmisc")
install.packages("readstata13")
install.packages("sampleSelection")
install.packages("textclean")
library(tidyverse)
library(readstata13)
library(sampleSelection)
library(textclean)
library(Hmisc)

data <- read.dta13("data_Kuleshov_Petrakov_Semeneev.dta")   #Берём данные, первоначальный отбор которых произведён в стате, убраны наблюдения, для которых не определена заработная плата

data$govwork <- data$ixentgov       # создание дополнительного стобца для работы с переменной, отвечающий за факт работы на государство
data$govwork[!(data$govwork==1)|is.na(data$govwork)|(data$work==0)] <- 0 # Условие, из которого следует, что значение 1 сохраняется только для тех, у кого есть работа и она "государственная"
data$kids <- data$ixkids    #параметр, который в итоге не был использован в итоговых регрессиях
data$kids[(!(data$kids==1))|is.na(data$kids)] <- 0

data_prepared <- data %>%
  select(work, age, age2, EDUC1, EDUC2, EDUC3, fem, disab, rel_inc, rus, lnwage, skills,
         real_train_days, avg_train_hours_per_day,non_sal_inc,govwork,kids)  #отбор переменных, для более быстрой работы программы
data_prepared$wage <- exp(data_prepared$lnwage)
data_prepared$wage[is.na(data_prepared$wage)] <- 0  #восстановления данных по зарплате из логарифма и заполнение Na
data_prepared$real_train_hours <- data_prepared$avg_train_hours_per_day*data_prepared$real_train_days  #создание переменной, означающее общее число часов посещения тренингов
data_prepared$trained[data_prepared$real_train_hours>0] <- 1  # создание переменной, отвечающей за факт посещения тренингов
data_prepared$trained[is.na(data_prepared$trained)] <- 0


data_osn_1 <- data_prepared[((data_prepared$wage)<=70000)&(data_prepared$age>=16)&(data_prepared$age<=65),] #применение ограничений
b1 <- selection(selection = work ~ age + age2 + EDUC1 + EDUC2 + EDUC3 + fem +
                  non_sal_inc + disab,
                outcome = lnwage ~ age + age2 + EDUC1 + EDUC2
                + EDUC3 +  trained + disab +fem +govwork,
                data = data_osn_1, method='2step')  #построение первой регрессии, для проверки регрессии в стате
summary(b1)

b2 <- selection(selection = work ~ age + age2 + EDUC1 + EDUC2 + EDUC3 + fem +
                  non_sal_inc + disab,
                outcome = lnwage ~ age + age2 + EDUC1 + EDUC2
                + EDUC3 +  trained + disab +fem +govwork+real_train_hours,
                data = data_osn_1, method='2step')
summary(b2)    #построение остальных регрессий, предстваленных в презентации

b3 <- selection(selection = work ~ age + age2 + EDUC1 + EDUC2 + EDUC3 + fem +
                  non_sal_inc + disab,
                outcome = lnwage ~ age + age2 + EDUC1 + EDUC2
                + EDUC3 +  trained + disab +fem +govwork+ real_train_days ,
                data = data_osn_1, method='2step')
summary(b3)


data_osn_2 <- data_osn_1[!is.na(data_osn_1$skills),]               #дополнительное ограничение по ответом на вопрос о своих способностях
b4 <- selection(selection = work ~ age + age2 + EDUC1 + EDUC2 + EDUC3 + fem +
                  non_sal_inc + disab,
                outcome = lnwage ~ age + age2 + EDUC1 + EDUC2
                + EDUC3 +  trained + disab +fem +govwork+skills,
                data = data_osn_2, method='2step')
summary(b4)

 