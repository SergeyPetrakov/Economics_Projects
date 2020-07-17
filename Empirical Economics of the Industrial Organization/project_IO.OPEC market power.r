library(readxl)
library(tidyverse)
library("stargazer") 
library("ggplot2")
library("lmtest")
library("sandwich")



#############################################################################
#importing from sheets
###############################################################################
the_way <- '~/Семестр - 6/Экономика отраслевых рынков/проект'
#the_way <- 'C:/Users/ds_nl/Uni/Otraslevyje_rynki/Projekt' # doughnut

#the_way="~/Uni/Otraslevyje_rynki"
#the_way <- "C:/Users/Александр/Desktop/Проект по отраслям/Данные"

setwd(the_way)


#View(data_for_project)

data_for_project_sheet_1_monthly<-read_xlsx("data.xlsx",
                                            sheet = 1, range = "A1:H569", col_names = TRUE,col_types = NULL,
                                            na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_1_monthly)


data_for_project_sheet_2_daily <- read_xlsx("data.xlsx",
                                            sheet = 2, range = "A1:B3397", col_names = TRUE,col_types = NULL, 
                                            na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_2_daily)


data_for_project_sheet_3_anually <- read_xlsx("data.xlsx",
                                              sheet = 3, range = "A1:B5579", col_names = TRUE,col_types = NULL, 
                                              na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_3_anually)

data_for_project_sheet_4_quarterly <- read_xlsx("data.xlsx",
                                              sheet = 4, range = "A1:C198", col_names = TRUE,col_types = NULL, 
                                              na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_4_quarterly)

data_for_project_sheet_5_monthly <- read_xlsx("data.xlsx",
                                             sheet = 5, range = "A1:G570", col_names = TRUE,col_types = NULL, 
                                             na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_5_monthly)

data_for_project_sheet_6_anually <- read_xlsx("data.xlsx",
                                              sheet = 6, range = "A1:O50", col_names = TRUE,col_types = NULL, 
                                              na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_6_anually)

data_for_project_sheet_7_monthly <- read_xlsx("data.xlsx",
                                              sheet = 7, range = "A1:AC569", col_names = TRUE,col_types = NULL, 
                                              na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_7_monthly)

data_for_project_sheet_8_anually <- read_xlsx("data.xlsx",
                                              sheet = 8, range = "A1:J50", col_names = TRUE,col_types = NULL, 
                                              na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_8_anually)

data_for_project_sheet_9_monthly <- read_xlsx("data.xlsx",
                                              sheet = 9, range = "A1:B629", col_names = TRUE,col_types = NULL, 
                                              na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_9_monthly)

data_for_project_sheet_10_monthly <- read_xlsx("data.xlsx",
                                              sheet = 10, range = "A1:B397", col_names = TRUE,col_types = NULL, 
                                              na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")

#View(data_for_project_sheet_10_monthly)

data_for_project_sheet_11_anually <- read_xlsx("data.xlsx",
                                               sheet = 11, range = "A1:I47", col_names = TRUE,col_types = NULL, 
                                               na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_11_anually)

data_for_project_sheet_12_monthly <- read_xlsx("data.xlsx",
                                               sheet = 12, range = "A1:B566", col_names = TRUE,col_types = NULL, 
                                               na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_12_monthly)

data_for_project_sheet_13_monthly <- read_xlsx("data.xlsx",
                                               sheet = 13, range = "A1:G569", col_names = TRUE,col_types = NULL, 
                                               na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_13_monthly)

data_for_project_sheet_14_monthly <- read_xlsx("data.xlsx",
                                               sheet = 14, range = "A1:B566", col_names = TRUE,col_types = NULL, 
                                               na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(data_for_project_sheet_14_monthly)

UOCI <- read_xlsx("data.xlsx",
                   sheet = "Cost", range = "A1:B82", col_names = TRUE,col_types = NULL, 
                   na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(UOCI)
Dummy <- read_xlsx("data.xlsx",
                   sheet = "Dummy", range = "A1:I566", col_names = TRUE,col_types = NULL, 
                   na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")

OPEC_plus <- read_xlsx("data.xlsx",
                        sheet = "OPEC+", range = "A1:M569", col_names = TRUE,col_types = NULL, 
                        na = "", trim_ws = TRUE, skip = 0, progress = readxl_progress(), .name_repair = "unique")
#View(OPEC_plus)
########################################################################################
#making data frames and joining
#############################################################################
#data frames
RATES_AGGRS_SECURITIES_monthly <- data.frame(data_for_project_sheet_1_monthly)
Vol_Index <- data.frame(data_for_project_sheet_2_daily)
EMBI <- data.frame(data_for_project_sheet_3_anually)
Deflator <- data.frame(data_for_project_sheet_4_quarterly)
PRICES_AND_IPG_months <- data.frame(data_for_project_sheet_5_monthly)
OPEC_prod_stocks_cons_ys <- data.frame(data_for_project_sheet_6_anually)
PRODUCTION_months <- data.frame(data_for_project_sheet_7_monthly)
OPEC_prod_years <- data.frame(data_for_project_sheet_8_anually)
killian_index <- data.frame(data_for_project_sheet_9_monthly)
Brent_Price_Global <- data.frame(data_for_project_sheet_10_monthly)
World_GDP <- data.frame(data_for_project_sheet_11_anually)
OECD_stocks <- data.frame(data_for_project_sheet_12_monthly)
Demand <- data.frame(data_for_project_sheet_13_monthly)
Bonds <- data.frame(data_for_project_sheet_14_monthly)


colnames(PRODUCTION_months)[1] <- "observation_date"
colnames(killian_index)[1] <- "observation_date"
colnames(Brent_Price_Global)[1] <- "observation_date"
#View(PRODUCTION_months)
#joining
#first join (outer join = annual)
#df_1 <- merge(x=RATES_AGGRS_SECURITIES_monthly,y=Vol_Index,by="observation_date",all=TRUE)
#df_2 <- merge(x=df_1,y=EMBI,by="observation_date",all=TRUE)
#df_3 <- merge(x=df_2,y=Deflator,by="observation_date",all=TRUE)
#df_4 <- merge(x=df_3,y=PRICES_AND_IPG_months,by="observation_date",all=TRUE)
#df_5 <- merge(x=df_4,y=OPEC_prod_stocks_cons_ys,by="observation_date",all=TRUE)
#df_6 <- merge(x=df_5,y=PRODUCTION_months,by="observation_date",all=TRUE)
#df_7 <- merge(x=df_6,y=OPEC_prod_years,by="observation_date",all=TRUE)
#df_8 <- merge(x=df_7,y=killian_index,by="observation_date",all=TRUE)
#df_all <- merge(x=df_8,y=Brent_Price_Global,by="observation_date",all=TRUE)
#df_all$China_GDP <- as.numeric(as.character(df_all$China_GDP))
#df_all$Exogenous_oil_supply_shocks <- as.numeric(as.character(df_all$Exogenous_oil_supply_shocks))






#second join (all monthly)
df_11 <- merge(x=RATES_AGGRS_SECURITIES_monthly,y=Vol_Index,by="observation_date",all.x=TRUE)
df_21 <- merge(x=df_11,y=EMBI,by="observation_date",all.x=TRUE)
df_31 <- merge(x=df_21,y=Deflator,by="observation_date",all.x=TRUE)
df_41 <- merge(x=df_31,y=PRICES_AND_IPG_months,by="observation_date",all.x=TRUE)
df_51 <- merge(x=df_41,y=OPEC_prod_stocks_cons_ys,by="observation_date",all.x=TRUE)
df_61 <- merge(x=df_51,y=PRODUCTION_months,by="observation_date",all.x=TRUE)
df_71 <- merge(x=df_61,y=OPEC_prod_years,by="observation_date",all.x=TRUE)
df_81 <- merge(x=df_71,y=killian_index,by="observation_date",all.x=TRUE)
df_91 <- merge(x=df_81,y=Brent_Price_Global,by="observation_date",all.x=TRUE)
df_101 <- merge(x=df_91,y=World_GDP,by="observation_date",all.x=TRUE)
df_111 <- merge(x=df_101,y=OECD_stocks,by="observation_date",all.x=TRUE)
df_121 <- merge(x=df_111,y=Demand,by="observation_date",all.x=TRUE)
df_131 <- merge(x=df_121,y=UOCI,by="observation_date",all.x=TRUE)
df_141 <- merge(x=df_131,y=Dummy,by="observation_date",all.x=TRUE)
df_151 <- merge(x=df_141,y=OPEC_plus,by="observation_date",all.x=TRUE)
df_monthly <- merge(x=df_151,y=Bonds,by="observation_date",all.x=TRUE)

#борьба с character
df_monthly$BCI_OECD <- as.numeric(df_monthly$BCI_OECD)
df_monthly$Ind_prod_US <- as.numeric(df_monthly$Ind_prod_US)
df_monthly$BCI_US <- as.numeric(df_monthly$BCI_US)
df_monthly$BCI_CHN <- as.numeric(df_monthly$BCI_CHN)
df_monthly$BCI_G.7 <- as.numeric(df_monthly$BCI_G.7)
df_monthly$Ind_prod_EU27 <- as.numeric(df_monthly$Ind_prod_EU27)


#Working with NA's - В этой части происходит замена NA на значения с помощью линейной интерполяции

#Для этого применяется метод internNA (его можно применить только к временным рядам, поэтому надо
#перевести было столбец в временной ряд)
#Достоинства такой замены в том, что заполняются пропуски именно в тех местах, где есть соседние 
#значения а не просто замена всех NA, то есть где нет данных вообще значений не появится, а замена
#возникнет в случае, если с двух сторон присутствуют значения, также это не простая замена пропусков
#средним по выборке, это именно интерполированные соседние значения, то есть временная структура не
#нарушается

library(timeSeries)
df_monthly$Vol_index <- interpNA(ts(df_monthly$Vol_index), method = "linear")
df_monthly$embi <- interpNA(ts(df_monthly$embi), method = "linear")
df_monthly$Deflator <- interpNA(ts(df_monthly$Deflator), method = "linear")
df_monthly$Exogenous_oil_supply_shocks <- interpNA(ts(df_monthly$Exogenous_oil_supply_shocks), method = "linear")
df_monthly$UOCI <- interpNA(ts(df_monthly$UOCI), method = "linear")


#Можно положить, что производство сырой нефти по годам равномерное, в таком случае
#просто сделаем для всех месячных значений соответствующего года, равные 1/12 годовых
#так для каждой страны из наших данных


df_monthly$Fred_Saudi <- df_monthly$Fred_Saudi/12
df_monthly$Fred_Qatar<- df_monthly$Fred_Qatar/12

df_monthly$Fred_Iran<- df_monthly$Fred_Iran/12
df_monthly$Fred_UAE <- df_monthly$Fred_UAE/12
df_monthly$Fred_Libya <- df_monthly$Fred_Libya/12
df_monthly$Fred_Iraq <- df_monthly$Fred_Iraq/12

df_monthly$Fred_Saudi <- interpNA(ts(df_monthly$Fred_Saudi), method = "before")
df_monthly$Fred_Qatar <- interpNA(ts(df_monthly$Fred_Qatar), method = "before")
df_monthly$Fred_Iran <- interpNA(ts(df_monthly$Fred_Iran), method = "before")
df_monthly$Fred_UAE <- interpNA(ts(df_monthly$Fred_UAE), method = "before")
df_monthly$Fred_Libya <- interpNA(ts(df_monthly$Fred_Libya), method = "before")
df_monthly$Fred_Iraq <- interpNA(ts(df_monthly$Fred_Iraq), method = "before")


df_monthly$Fred_Oman <- df_monthly$Fred_Oman/12
df_monthly$Fred_Azer <- df_monthly$Fred_Azer/12
df_monthly$Fred_Syria <- df_monthly$Fred_Syria/12
df_monthly$Fred_Kuwait <- df_monthly$Fred_Kuwait/12
df_monthly$Fred_Kaz <- df_monthly$Fred_Kaz/12
df_monthly$Fred_Alg <- df_monthly$Fred_Alg/12

df_monthly$Fred_Oman <- interpNA(ts(df_monthly$Fred_Oman), method = "before")
df_monthly$Fred_Azer <- interpNA(ts(df_monthly$Fred_Azer), method = "before")
df_monthly$Fred_Syria <- interpNA(ts(df_monthly$Fred_Syria), method = "before")
df_monthly$Fred_Kuwait <- interpNA(ts(df_monthly$Fred_Kuwait), method = "before")
df_monthly$Fred_Kaz <- interpNA(ts(df_monthly$Fred_Kaz), method = "before")
df_monthly$Fred_Alg <- interpNA(ts(df_monthly$Fred_Alg), method = "before")


df_monthly$Pers_Consumption <- df_monthly$Pers_Consumption/12
df_monthly$China_GDP <- df_monthly$China_GDP/12
df_monthly$USA_GDP_monthly_average_2012.billions_dollars. <- df_monthly$USA_GDP_annual_average_2012.billions_dollars./12
df_monthly$USA_GDP_monthly_average_2012.billions_dollars. <- interpNA(ts(df_monthly$USA_GDP_annual_average_2012.billions_dollars.), method = "before")
df_monthly$China_GDP <- interpNA(ts(df_monthly$China_GDP), method = "before")
df_monthly$Pers_Consumption <- interpNA(ts(df_monthly$Pers_Consumption), method = "before")



df_monthly$World_supply <- df_monthly$World_supply/12
df_monthly$Refinery_Utilisation <- df_monthly$Refinery_Utilisation/12
df_monthly$Potential_capacities <- df_monthly$Potential_capacities/12
df_monthly$Operating_capacities <- df_monthly$Operating_capacities/12
df_monthly$Energy_consumption <- df_monthly$Energy_consumption/12
df_monthly$World_consumption <- df_monthly$World_consumption/12
df_monthly$World_consumption  <- interpNA(ts(df_monthly$World_consumption), method = "before")
df_monthly$Energy_consumption <- interpNA(ts(df_monthly$Energy_consumption), method = "before")
df_monthly$Operating_capacities <- interpNA(ts(df_monthly$Operating_capacities), method = "before")
df_monthly$Potential_capacities <- interpNA(ts(df_monthly$Potential_capacities), method = "before")
df_monthly$Refinery_Utilisation <- interpNA(ts(df_monthly$Refinery_Utilisation), method = "before")
df_monthly$World_supply <- interpNA(ts(df_monthly$World_supply), method = "before")

df_monthly$GDP_World <- df_monthly$GDP_World
df_monthly$GDP_World_Real_t <- df_monthly$GDP_World_Real_t
df_monthly$GDP_World_Real_t.1 <- df_monthly$GDP_World_Real_t.1/12/1000
df_monthly$GDP_World_Real_t.2 <- df_monthly$GDP_World_Real_t.2/12/1000
df_monthly$GDP_World_Real_t.3 <- df_monthly$GDP_World_Real_t.3/12/1000
df_monthly$GDP_World_Real_t.4 <- df_monthly$GDP_World_Real_t.4/12/1000
df_monthly$GDP_World_Real_t.5 <- df_monthly$GDP_World_Real_t.5/12/1000
df_monthly$GDP_World_Real_t.6 <- df_monthly$GDP_World_Real_t.6/12/1000
#   WE DIVIDE GDP SERIES SINCE WE WANT TRILLIONS NOT BILLIONS OF US$
df_monthly$GDP_World <- interpNA(ts(df_monthly$GDP_World), method = "before")
df_monthly$GDP_World_Real_t <- interpNA(ts(df_monthly$GDP_World_Real_t), method = "before")
df_monthly$GDP_World_Real_t.1 <- interpNA(ts(df_monthly$GDP_World_Real_t.1), method = "before")
df_monthly$GDP_World_Real_t.2 <- interpNA(ts(df_monthly$GDP_World_Real_t.2), method = "before")
df_monthly$GDP_World_Real_t.3 <- interpNA(ts(df_monthly$GDP_World_Real_t.3), method = "before")
df_monthly$GDP_World_Real_t.4 <- interpNA(ts(df_monthly$GDP_World_Real_t.4), method = "before")
df_monthly$GDP_World_Real_t.5 <- interpNA(ts(df_monthly$GDP_World_Real_t.5), method = "before")
df_monthly$GDP_World_Real_t.6 <- interpNA(ts(df_monthly$GDP_World_Real_t.6), method = "before")

df_monthly$Oil_reserves <- interpNA(ts(df_monthly$Oil_reserves), method = "before")
df_monthly$Oil_rigs_monthly <- interpNA(ts(df_monthly$Oil_rigs_annual), method = "before")


df_monthly <- df_monthly %>% select(-USA_GDP_annual_average_2012.billions_dollars., -Oil_rigs_annual)
df_monthly <- data.frame(df_monthly)
View(df_monthly)

colnames(df_monthly)
#################################################################################################################3
#Regressions
##############################################################################################################
#бьём выборку на наши временные интервалы

df_oil_crysis_1973_1979 <- df_monthly[1:84,]
df_iran_war_1980_1984 <- df_monthly[85:144,]
df_weak_opec_1985_1999 <- df_monthly[145:324,]
df_growth_2000_2007 <- df_monthly[325:420,]
df_high_prices_2008_2013 <- df_monthly[421:492,]
df_low_prices_2014_2016 <- df_monthly[493:528,]
df_upward_trend_and_stop_2016_2020 <- df_monthly[529:568,]


############################################################
#2SLS&OLS
#############################################################
library("AER") #Сборник полезных эмпирических примеров и необходимых для них пакетов
library("sandwich")
library("lmtest")
library("car")
library("stargazer") #Красивые таблички
library("ggplot2")
library("openintro")
library("OIdata")
library("gdata")
library("doBy")
library("plm") #Панельные данные -- основной пакет для сегодняшнего занятия
library("ivpack") 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

ivse = function(reg) {
  rob = robust.se(reg)[,2]
  return(rob)
}

#1973-1979
demand_1_2SLS <- ivreg(data = df_oil_crysis_1973_1979, log(World_consumption) ~ log(WTI_Price) + GDP_World_Real_t | .-log(WTI_Price) + log(lag(WTI_Price, n=1L)) + log(lag(WTI_Price, n=2L))+ GDP_World_Real_t)
summary(demand_1_2SLS, diagnostics = TRUE)

model_1_OLS <- lm(data = df_oil_crysis_1973_1979, log(World_consumption) ~ log(WTI_Price) + GDP_World_Real_t)
summary(model_1_OLS)


stargazer(demand_1_2SLS, model_1_OLS, title="мировой спрос 1973-1979", type="text", se=list(ivse(demand_1_2SLS), cse(model_1_OLS)),
          column.labels=c("2SLS", "OLS"), 
          df=FALSE, digits=3)
ivse(demand_1_2SLS)

mean(df_oil_crysis_1973_1979$OPEC_market_share)

s <- mean(df_oil_crysis_1973_1979$OPEC_market_share)
st <- df_oil_crysis_1973_1979$OPEC_market_share
a <- -0.02452
b <- 0.0433
lambda <- a/(a-b*(1-st))
lambda
plot(lambda, type="line", xlab="date")


#1980-1984
demand_2_2SLS <- ivreg(data = df_iran_war_1980_1984, log(World_consumption) ~ log(WTI_Price)+log(Ind_Index)+Exogenous_oil_supply_shocks+log(GDP_World_Real_t) | .-log(WTI_Price) + log(lag(lag(WTI_Price)))+log(GDP_World_Real_t.1))
summary(demand_2_2SLS, diagnostics = TRUE)

model_2_OLS <- lm(data = df_oil_crysis_1973_1979, log(World_consumption) ~ log(WTI_Price)+log(Ind_Index)+Exogenous_oil_supply_shocks+log(GDP_World_Real_t))
summary(model_2_OLS)

stargazer(demand_2_2SLS, model_2_OLS, title="мировой спрос 1980-1984", type="text", se=list(cse(demand_2_2SLS), cse(model_2_OLS)),
          column.labels=c("2SLS", "OLS"), 
          df=FALSE, digits=3)

s <- mean(df_iran_war_1980_1984$OPEC_market_share)
st <- df_iran_war_1980_1984$OPEC_market_share
a <- -0.00988
b <- 0.034
lambda <- a/(a-b*(1-s))
lambda
plot(lambda, type="line", xlab="date")

#1985_1999
demand_3_2SLS <- ivreg(data = df_weak_opec_1985_1999, log(World_consumption) ~ log(WTI_Price)+log(Ind_Index)+Exogenous_oil_supply_shocks+log(GDP_World_Real_t) | .-log(WTI_Price) + log(lag(WTI_Price))+log(GDP_World_Real_t.1))
summary(demand_3_2SLS, diagnostics = TRUE)

model_3_OLS <- lm(data = df_weak_opec_1985_1999, log(World_consumption) ~ log(WTI_Price)+log(Ind_Index)+Exogenous_oil_supply_shocks+log(GDP_World_Real_t))
summary(model_3_OLS, diagnostics = TRUE)

stargazer(demand_3_2SLS, model_3_OLS, title="мировой спрос 1985-1999", type="text", se=list(cse(demand_3_2SLS), cse(model_3_OLS)),
          column.labels=c("2SLS", "OLS"), 
          df=FALSE, digits=3)


s <- mean(df_weak_opec_1985_1999$OPEC_market_share)
s
st <- df_weak_opec_1985_1999$OPEC_market_share
a <- -0.00506
b <- 0.0833
lambda <- a/(a-b*(1-st))
lambda
plot(lambda, type="line", xlab="date")
#2000-2007
demand_4_2SLS <- ivreg(data = df_growth_2000_2007, log(World_consumption) ~ log(WTI_Price)+log(Ind_Index)+Exogenous_oil_supply_shocks+log(GDP_World_Real_t) | .-log(WTI_Price) + log(lag(WTI_Price)))
summary(demand_4, diagnostics = TRUE)

model_4_OLS <- lm(data = df_growth_2000_2007, log(World_consumption) ~ log(WTI_Price)+log(Ind_Index)+Exogenous_oil_supply_shocks+log(GDP_World_Real_t))
summary(model_4_OLS, diagnostics = TRUE)

stargazer(demand_4_2SLS, model_4_OLS, title="мировой спрос 1985-1999", type="text", se=list(cse(demand_3_2SLS), cse(model_3_OLS)),
          column.labels=c("2SLS", "OLS"), 
          df=FALSE, digits=3)

model_4_OLS <- lm(data = df_growth_2000_2007, log(World_consumption) ~ log(WTI_Price)+log(GDP_World_Real_t)+log(GDP_World_Real_t.1)+log(GDP_World_Real_t.2)+log(GDP_World_Real_t.3)+log(GDP_World_Real_t.4)+log(GDP_World_Real_t.5)+
                    log(GDP_World_Real_t.6)+log(SP_500)+log(Freight_Index)+log(embi)+log(Energy_consumption)+log(Pers_Consumption)+log(China_GDP)+log(OECD_Stocks)+log(Exch_rate_real)+log(M2_real)+
                    AAA10Y_Monthly+Fed_rate+Deflator)
summary(model_4_OLS)


s <- mean(df_growth_2000_2007$OPEC_market_share)
st <- df_growth_2000_2007$OPEC_market_share
a <- -0.03016
b <- 0.048619 
lambda <- a/(a-b*(1-st))
lambda
plot(lambda, type="line", xlab="date")
#2008-2013

s <- mean(df_high_prices_2008_2013$OPEC_market_share)
st <- df_high_prices_2008_2013$OPEC_market_share
a <- -0.0244
b <- 0.01973
lambda <- a/(a-b*(1-st))
lambda
plot(lambda, type="line", xlab="date")
#2014-2016


s <- mean(df_low_prices_2014_2016$OPEC_market_share)
st <- df_low_prices_2014_2016$OPEC_market_share
a <- -0.0002763
b <- 0.030425
lambda <- a/(a-b*(1-st))
lambda
plot(lambda, type="line", xlab="date")

View(df_monthly)





df_monthly_wo_ussr_shocks_and_volindex <- df_monthly %>% select(-USSR_IEA, -Exogenous_oil_supply_shocks, -observation_date, -Vol_index, -South_Sudan_IEA)
df_monthly_wo_ussr_shocks_and_volindex <- na.omit(data.frame(df_monthly_wo_ussr_shocks_and_volindex))
View(df_monthly_wo_ussr_shocks_and_volindex)

## we first make the Least Squares Ordinary

attach(df_monthly_wo_ussr_shocks_and_volindex)
model_01 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption) ~ log(WTI_Price)+log(GDP_World_Real_t)+log(GDP_World_Real_t.1))

alpha <- data.frame(model_01$coefficients[2])[1,1]
summary(model_01)
# a = -0.003679
## We later make the IV-method since the GDP is probably influenced by World Oil Consumption. We take only former values of GDP as instrunments for the current

model_iv_01 <- AER::ivreg(log(World_consumption) ~ log(GDP_World_Real_t) + log(WTI_Price) |log(GDP_World_Real_t.2) + log(GDP_World_Real_t.1) + log(lag(WTI_Price)) , data = df_monthly_wo_ussr_shocks_and_volindex)
summary(model_iv_01, diagnostics = TRUE)
## insignificant зато все тесты годные резы показывают

alpha_1 = -0.0103

library(AER)

iv2 = ivreg(log(World_consumption) ~ log(WTI_Price) + log(GDP_World_Real_t) | log(GDP_World_Real_t) + log(lag(WTI_Price)),
            data = df_monthly_wo_ussr_shocks_and_volindex)
summary(iv2, diagnostics = TRUE)


## OUR GDP is in billions, while the GDP in the Ma work is in $trln
model_iv_01_1 <- AER::ivreg(log(World_consumption) ~ log(WTI_Price) + log(GDP_World_Real_t)|log(GDP_World_Real_t) + log(lag(WTI_Price)) + log(lag(lag(WTI_Price))) , data = df_monthly_wo_ussr_shocks_and_volindex)
summary(model_iv_01_1, diagnostics = TRUE)
#

model_iv_02 <- AER::ivreg(log(World_consumption) ~ log(GDP_World_Real_t) + log(WTI_price)|.-GDP_World_Real_t + log(GDP_World_Real_t.1), data = df_monthly_wo_ussr_shocks_and_volindex)
## one can also simply regress  by lagged vals of GDP_World we dont understand why GDP_World is always used
model_01_1 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption) ~ log(WTI_Price)+log(GDP_World_Real_t.1))
summary(model_01_1)
# alpha = 0.006- not great again

## try regression with lag on price
attach(df_monthly_wo_ussr_shocks_and_volindex)
model_01_2 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption) ~ log(lag(WTI_Price))+log(GDP_World_Real_t.1) + log(GDP_World_Real_t))
summary(model_01_2)
## -0.004 if GDP_World_Real - both

model_01_3 <-  lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption) ~ log(lag(lag(WTI_Price))) + log(lag(WTI_Price))+log(GDP_World_Real_t.1) + log(GDP_World_Real_t))
summary(model_01_3)
# insignificant results  - if only GDP[-1]
# -0.01 if with GDP_World_Real

model_01_3 <-  lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption) ~ log(lag(lag(WTI_Price))) +log(GDP_World_Real_t.1))
summary(model_01_3)
# alpha =

### alpha = 0.005 (*)



#alpha_1 = -0.008255
alpha_1 = -0.008255

attach(df_monthly)
model_02 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex,
               log(NOPEC_IEA) ~ log(WTI_Price)+log(Oil_rigs_monthly)+log(UOCI)+log(WTI_Inflation)+log(Freight_Index))
model_02
summary(model_02)


model_02_1 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex,
               log(NOPEC_IEA) ~ lag(log(WTI_Price))+log(Oil_rigs_monthly)+log(UOCI)+log(WTI_Inflation)+log(Freight_Index) )
summary(model_02_1)

model_02_2 <- AER::ivreg(data = df_monthly_wo_ussr_shocks_and_volindex, log(NOPEC_IEA) ~ log(WTI_Price)+log(Oil_rigs_monthly)+log(UOCI)+log(Freight_Index) | .-WTI_Price + log(lag(WTI_Price)))
summary(model_02_2, diagnostics = TRUE)

# stil negative, damn!
model_02_3 <- ivreg(data = df_monthly_wo_ussr_shocks_and_volindex, log(NOPEC_IEA) ~ log(WTI_Price)+log(Oil_rigs_monthly)+log(UOCI)+log(Freight_Index) | .-log(WTI_Price) + lag(log(WTI_Price)))
summary(model_02_3, diagnostics = TRUE)
# ebenso

#beta_1 = 0.19054
beta_1 = 0.01764
s <- df_monthly_wo_ussr_shocks_and_volindex$OPEC_market_share

lambda <- alpha_1/(alpha_1-(1-s))
plot(lambda, type="line")
lambda
s
#пока не похоже




stargazer(model_01, model_02,   
          se=list(cse(model_01),cse(model_02)), 
          title="МНК WTI на регрессоры", type="text", 
          column.labels=c("лог-Линейная модель мирового спроса", "лог-линейная модель предложения неОПЕК"), 
          df=FALSE, digits=3)


##############################################################################################################
#Regressions on df_monthly
##############################################################################################################
df_monthly_wo_ussr_shocks_and_volindex <- df_monthly %>% select(-USSR_IEA, -Exogenous_oil_supply_shocks, -observation_date, -Vol_index, -South_Sudan_IEA)
df_monthly_wo_ussr_shocks_and_volindex <- na.omit(data.frame(df_monthly_wo_ussr_shocks_and_volindex))
View(df_monthly_wo_ussr_shocks_and_volindex)
attach(df_monthly_wo_ussr_shocks_and_volindex)

model_03 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption*30000000) ~ log(WTI_Price)+log(GDP_World_Real_t)+log(GDP_World_Real_t.1)+log(GDP_World_Real_t.2)+log(GDP_World_Real_t.3)+log(GDP_World_Real_t.4)+log(GDP_World_Real_t.5)+
                 log(GDP_World_Real_t.6)+log(SP_500)+log(Freight_Index)+log(embi)+percent_dev_econ_act_from_trend+log(Energy_consumption)+log(Pers_Consumption)+log(China_GDP))#+
                 #log(USA_GDP_monthly_average_2012.billions_dollars.)+log(OECD_stocks))
summary(model_03)
## negligible (10-3)
model_04 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption*30000000) ~ log(WTI_Price)+log(GDP_World_Real_t)+log(GDP_World_Real_t.1)+log(GDP_World_Real_t.2)+log(GDP_World_Real_t.3)+log(GDP_World_Real_t.4)+log(GDP_World_Real_t.5)+
                 log(GDP_World_Real_t.6)+log(SP_500)+log(Freight_Index)+log(embi)+percent_dev_econ_act_from_trend+log(Energy_consumption)+log(Pers_Consumption)+log(China_GDP)+log(OECD_Stocks))
summary(model_04)
## (10-3)
model_05 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption*30000000) ~ log(WTI_Price)+log(GDP_World_Real_t)+log(GDP_World_Real_t.1)+log(GDP_World_Real_t.2)+log(GDP_World_Real_t.3)+log(GDP_World_Real_t.4)+log(GDP_World_Real_t.5)+
                 log(GDP_World_Real_t.6)+log(SP_500)+log(Freight_Index)+log(embi)+log(Energy_consumption)+log(Pers_Consumption)+log(China_GDP)+log(OECD_Stocks))
summary(model_05)
## -0.004
model_051 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption*30000000) ~ log(Brent_Price_Global)+log(GDP_World_Real_t)+log(GDP_World_Real_t.1)+log(GDP_World_Real_t.2)+log(GDP_World_Real_t.3)+log(GDP_World_Real_t.4)+log(GDP_World_Real_t.5)+
                 log(GDP_World_Real_t.6)+log(SP_500)+log(Freight_Index)+log(embi)+log(Energy_consumption)+log(Pers_Consumption)+log(China_GDP)+log(OECD_Stocks))
summary(model_051)


model_06 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption) ~ log(WTI_Price)+log(GDP_World_Real_t)+log(GDP_World_Real_t.1)+log(GDP_World_Real_t.2)+log(GDP_World_Real_t.3)+log(GDP_World_Real_t.4)+log(GDP_World_Real_t.5))#+
                 #log(GDP_World_Real_t.6)+log(SP_500))#+log(Freight_Index)+log(embi)+percent_dev_econ_act_from_trend+log(Energy_consumption)+log(Pers_Consumption)+log(China_GDP)+log(OECD_Stocks))
summary(model_06)
# -0.003
model_07 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption) ~ log(WTI_Price)+log(GDP_World_Real_t)+log(GDP_World_Real_t.1)+log(GDP_World_Real_t.2)+log(GDP_World_Real_t.3)+log(GDP_World_Real_t.4))#+log(GDP_World_Real_t.5)+
                 #log(GDP_World_Real_t.6)+log(SP_500)+log(Freight_Index))#+log(embi)+log(Energy_consumption)+log(Pers_Consumption)+log(China_GDP)+log(OECD_Stocks))
summary(model_07)
# -0.003
model_08 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption*30000000) ~ log(WTI_Price)+log(GDP_World_Real_t)+log(GDP_World_Real_t.1)+log(GDP_World_Real_t.2)+log(GDP_World_Real_t.3)+log(GDP_World_Real_t.4)+log(GDP_World_Real_t.5)+
                log(GDP_World_Real_t.6)+log(SP_500)+log(Freight_Index)+log(embi)+log(Energy_consumption)+log(Pers_Consumption)+log(China_GDP)+log(OECD_Stocks)+log(Exch_rate_real)+log(M2_real)+log(WTI_Inflation)+
                 AAA10Y_Monthly+log(Net_pos)+Fed_rate+Deflator)
summary(model_08)
# -0.002
model_09 <- lm(data = df_monthly_wo_ussr_shocks_and_volindex, log(World_consumption*30000000) ~ log(WTI_Price)+log(Brent_Price_Eu)+log(GDP_World_Real_t)+log(GDP_World_Real_t.1)+log(GDP_World_Real_t.2)+log(GDP_World_Real_t.3)+log(GDP_World_Real_t.4)+log(GDP_World_Real_t.5)+
                 log(GDP_World_Real_t.6)+log(SP_500)+log(Freight_Index)+log(embi)+log(Energy_consumption)+log(Pers_Consumption)+log(China_GDP)+log(OECD_Stocks)+log(Exch_rate_real)+log(M2_real)+log(WTI_Inflation)+
                 AAA10Y_Monthly+log(Net_pos)+Fed_rate+Deflator)
summary(model_09)
# 0.005

stargazer(model_06, model_07,  
          se=list(cse(model_06), cse(model_07)), 
          title="МНК WTI на регрессоры", type="text", 
          column.labels=c("лог-Линейная модель мирового спроса_1", "лог-Линейная модель мирового спроса_2"),
          df=FALSE, digits=3)


stargazer(model_03, model_04, model_05, model_06, model_07,title="мировой спрос", type="text", se=list(cse(model_03), cse(model_04), cse(model_05), cse(model_06), cse(model_07)),
          column.labels=c("Модель 3", "Модель 4", "Модель 5", "Модель 6", "Модель 7"), 
          df=FALSE, digits=3)

stargazer(model_03, model_04, model_05, model_06, model_07, model_08, title="мировой спрос", type="text", se=list(cse(model_03), cse(model_04), cse(model_05), cse(model_06), cse(model_07),cse(model_08)),
          column.labels=c("Модель 3", "Модель 4", "Модель 5", "Модель 6", "Модель 7", "Модель 8"), 
          df=FALSE, digits=3)

stargazer(model_03, model_04, model_05, model_06, model_07, model_08, model_09, title="мировой спрос", type="text", se=list(cse(model_03), cse(model_04), cse(model_05), cse(model_06), cse(model_07), cse(model_08), cse(model_09)),
          column.labels=c("Модель 3", "Модель 4", "Модель 5", "Модель 6", "Модель 7", "Модель 8","model_09"), 
          df=FALSE, digits=3)

alpha = -0.005


library(writexl)
# Write the first data set in a new workbook

write_xlsx(df_monthly,"./df_monthly.xlsx")

### 3SLS - ESTIMATION:

# TODO: complete the ssytemfit estimation for Non-OPEC countries
#systemfit::systemfit(method ='3SLS', formula = , inst =  )



