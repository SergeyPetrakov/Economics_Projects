#install.packages("QuantTools")
#install.packages("data.table")
#install.packages("ghyp")
#install.packages("copula")
#install.packages("fGarch")
#install.packages("evd")
#install.packages("rugarch")

library( QuantTools ) # Market data loading
library( data.table ) # Library for data management
library( ghyp )       # GHP
library( copula )     # Copula
library( fGarch )     # GARCH   
library( evd )        # Extreme Value
library(rugarch)


#Data Loading
# download data
stock_1 <- get_yahoo_data( "XOM", from = "2016-01-01", to = "2020-01-01" )
stock_2 <- get_yahoo_data( "VOW3.DE" , from = "2016-01-01", to = "2020-01-01" )
stock_3 <- get_yahoo_data( "TCEHY" , from = "2016-01-01", to = "2020-01-01" )

# calс returns
stock_1 <- stock_1[, .( date, return_1 = close / shift( close, fill = close[1] ) - 1 )]
stock_2 <- stock_2[, .( date, return_2 = close / shift( close, fill = close[1] ) - 1 )]
stock_3 <- stock_3[, .( date, return_3 = close / shift( close, fill = close[1] ) - 1 )]

# merge to portfolio
portfolio <- merge( stock_1, stock_2, by = "date" )
portfolio <- merge(portfolio, stock_3, by = "date")
data = as.vector( portfolio[, .( return_1, return_2, return_3 ) ] )

# pnl as cumulative products
plot_dts( portfolio[, .(date, XOM = cumprod(1+return_1), VOW3.DE = cumprod(1+return_2), TCEHY =cumprod(1+return_3))])
                 

#Portfolio optimization

calc_perfomance = function( returns ){
  
  # Cumulative Products
  pnl = cumprod( returns + 1 )
  
  summary = data.table(
    pnl    = prod( returns + 1 ) - 1,
    max_dd = min( pnl / cummax( pnl ) - 1 ) * 100,
    sharpe = mean( returns ) / sd( returns ) * sqrt(252)
  )
  
  round( summary, 2 )
  
}

calc_perfomance( portfolio$return_1 )
calc_perfomance( portfolio$return_2 )
calc_perfomance( portfolio$return_3 )

# weights
weights = CJ(
  w1 = seq( 0.0, 1, 0.05),
  w2 = seq( 0.0, 1, 0.05),
  w3 = seq( 0.0, 1, 0.05)
)

weights = weights[ w1+w2+w3 == 1 ]

result = weights[, calc_perfomance( portfolio$return_1*w1 + portfolio$return_2*w2 + portfolio$return_3*w3 )
                 , by = .( w1, w2, w3 ) ]

result[order(-pnl)][1:5]
result[order(-max_dd)][1:5]
result[order(-sharpe)][1:5]

#Выберем параметры для портфеля с равными весами, с макс доходностью, максимальном Sharpe

# sum returns
portf_eq         = portfolio[, .(date, pnl_eq     = return_1/3 + return_2/3 + return_3/3)   ]
portf_max_pnl    = portfolio[, .(date, pnl_pnl    = return_1*0 + return_2*0 + return_3*1) ]
portf_max_sharpe = portfolio[, .(date, pnl_sharpe = return_1*0 + return_2*0.2 + return_3*0.8) ]


# calc pnl
portf_eq        [, pnl_eq     := cumprod( 1 + pnl_eq)     ]
portf_max_pnl   [, pnl_pnl    := cumprod( 1 + pnl_pnl)    ]
portf_max_sharpe[, pnl_sharpe := cumprod( 1 + pnl_sharpe) ]

plot_dts(
  portf_eq,
  portf_max_pnl,
  portf_max_sharpe
)

#Динамическая оптимизация

calc_opt_weights = function( return_1, return_2, return_3 ){
  
  weights = CJ(
    w1 = seq( 0.05, 1, 0.05),
    w2 = seq( 0.05, 1, 0.05), 
    w3 = seq( 0.05, 1, 0.05)
  )
  weights = weights[ w1+w2+w3 == 1 ]
  
  weights = weights[, calc_perfomance( return_1*w1 + return_2*w2 + return_3*w3 )
                    , by = .( w1, w2, w3 ) ]
  
  weights[order(-sharpe)][1, .(w1, w2, w3)]
  
}


portfolio[, year := year(date) ]
weights = portfolio[, calc_opt_weights( return_1, return_2, return_3 ), by = year ]
weights[, year := year + 1] # use this w for new year
weights

#Посчитаем доходность для каждого года с учетом весов
#Определим баланс на конец каждого года, чтобы потом учесть в итоговом портфеле
# calc pnl yearly
portfolio_dynamic = merge( portfolio, weights, by = "year", all.x = T )
portfolio_dynamic = portfolio_dynamic[ !is.na( w1 ) ]
portfolio_dynamic[, pnl_yearly := cumprod( return_1*w1 + return_2*w2 + return_3*w3 +1), by = year]

# calc balance
result_yearly = portfolio_dynamic[, .(balance_yeraly = pnl_yearly[.N]), by = year]
result_yearly[, balance_yeraly := shift( balance_yeraly, fill = 1 ) ]
result_yearly[, balance := cumprod( balance_yeraly )]

#Добавляем баланс в итоговый портфель и считаем доходность
# include balance for portf
portfolio_dynamic = merge( portfolio_dynamic, result_yearly, by = "year", all.x = T )
portfolio_dynamic[, pnl_dynamic := pnl_yearly * balance]


plot_dts( 
  portfolio_dynamic[, .(date, pnl_dynamic)],
  portf_max_sharpe[year(date) >= 2014]
)


#Оценка риска
#В этом разделе будут рассмотрены варианты моделирования доходности портфеля и оценки риска

#ghyp
#Выбираем лучшую модель

aic.mv = stepAIC.ghyp( data, dist = c( "gauss", "t", "ghyp" ), symmetric=NULL, silent=T )
aic.mv$best.model@model[1]

#Подгоняем модель и считаем VaR и ES
prt.fit = fit.tmv( data, symmetric=T, silent=T )

w     = c(1/3,1/3,1/3)
N     = 10^6
alpha = 0.05

sim = rghyp( n = N, object = prt.fit )
prt.sim = w[1] * sim[,1] + w[2] * sim[,2] + w[3] * sim[,3]
prt.sim = sort(prt.sim)

VaR = prt.sim[ alpha*N ]
ES  = mean( prt.sim[ 1:( alpha * N-1 ) ] )

data.table( VaR, ES )

#Пример оптимизации портфеля
opt = portfolio.optimize(prt.fit, risk.measure= "value.at.risk", 
                         type="minimum.risk", silent=T)
opt$opt.weights

w = opt$opt.weights
prt.sim = w[1]*sim[,1]+w[2]*sim[,2]+w[3]*sim[,3]
prt.sim = sort(prt.sim)

VaR = prt.sim[ alpha*N ]
ES  = mean( prt.sim[ 1:( alpha * N-1 ) ] )

data.table( VaR, ES )

#copula
#Выбираем лучшую модель для каждой бумаги

stock1.fit = stepAIC.ghyp( data$return_1, dist = c( "gauss", "t", "ghyp" ),
                           symmetric=NULL, silent=T)$best.model
stock2.fit = stepAIC.ghyp( data$return_2, dist = c( "gauss", "t", "ghyp" ),
                           symmetric=NULL, silent=T)$best.model
stock3.fit = stepAIC.ghyp( data$return_3, dist = c( "gauss", "t", "ghyp" ),
                           symmetric=NULL, silent=T)$best.model

stock1.fit@model[1]
stock2.fit@model[1]
stock3.fit@model[1]

#Получаем эмперические распределения доходностей Строим совместное распределение

stock1.cdf = pghyp( data$return_1, object = stock1.fit )
stock2.cdf = pghyp( data$return_2, object = stock2.fit )
stock3.cdf = pghyp( data$return_3, object = stock3.fit )

cdf = cbind( stock1.cdf, stock2.cdf, stock3.cdf )
plot( cdf )

#Подгоняем копулы и определяем лучшую модель
# define copulas
norm.cop = normalCopula( dim=3, dispstr = "un")
stud.cop = tCopula     ( dim=3, param=0.5 )
gumb.cop = gumbelCopula( dim=3, param=2 )



# Copula fiting
norm.fit = fitCopula(cdf,copula=norm.cop)
stud.fit = fitCopula(cdf,copula=stud.cop)
gumb.fit = fitCopula(cdf,copula=gumb.cop)

# best model
data.table(
  norm = norm.fit@loglik,
  stud = stud.fit@loglik,
  gumb = gumb.fit@loglik
)


#Считаем Var и ES

N = 10^3
stud.sim = rCopula( n=N, copula = stud.fit@copula )

stock1.sim = qghyp( stud.sim[,1], object=stock1.fit )
stock2.sim = qghyp( stud.sim[,2], object=stock2.fit )
stock3.sim = qghyp( stud.sim[,3], object=stock3.fit )

w = c(1/3,1/3,1/3)
prt.sim = w[1]*stock1.sim + w[2]*stock2.sim+w[3]*stock3.sim 

alpha = 0.05
prt.sim = sort(prt.sim)
VaR = prt.sim[alpha*N]
ES = mean(prt.sim[1:(alpha*N-1)])

data.table( VaR, ES )

#GARCH
#Подгоняем GARCH модели, стандартизируем остатки и получаем распределение

stock1.gfit = garchFit( data = data$return_1, formula =~garch(1,1),
                        cond.dist="ged", trace=F )

stock2.gfit = garchFit( data = data$return_2, formula =~garch(1,1),
                        cond.dist="ged", trace=F )

stock3.gfit = garchFit( data = data$return_3, formula =~garch(1,1),
                        cond.dist="ged", trace=F )


# stand residuals
z = matrix( nrow=nrow(data), ncol=3 )
z[,1] = stock1.gfit@residuals / stock1.gfit@sigma.t
z[,2] = stock2.gfit@residuals / stock2.gfit@sigma.t
z[,3] = stock3.gfit@residuals / stock3.gfit@sigma.t

# CDF for residuals
mean = c(0,0,0); sd = c(1,1,1); nu = c(1.3,1.3,1.3)
xi = c(1, 1, 1)
cdf = matrix( nrow=nrow(data), ncol=3 )

cdf[,1] = psged( z[,1], mean=mean[1], sd=sd[1], nu=nu[1], xi=xi[1] )
cdf[,2] = psged (z[,2], mean=mean[2], sd=sd[2], nu=nu[2], xi=xi[2] )
cdf[,3] = psged (z[,3], mean=mean[3], sd=sd[3], nu=nu[3], xi=xi[3] )

#Подгонка копул

# Copula fiting
norm.fit = fitCopula( cdf, copula=norm.cop )
stud.fit = fitCopula( cdf, copula=stud.cop )
gumb.fit = fitCopula( cdf, copula=gumb.cop )

# best model
data.table(
  norm = norm.fit@loglik,
  stud = stud.fit@loglik,
  gumb = gumb.fit@loglik
)

#Оцениваем риск

cdf.sim = rCopula( n=N, copula=norm.fit@copula )
z.sim = matrix( nrow=N, ncol=3 )
for (i in 1:3) 
  z.sim[,i] = qsged(cdf.sim[,i], mean=mean[i], sd=sd[i], nu=nu[i], xi=xi[i] )

frc1 = predict( stock1.gfit, n.ahead=1 )
frc2 = predict( stock2.gfit, n.ahead=1 )
frc3 = predict( stock3.gfit, n.ahead=1 )
mu = c(frc1[,1],frc2[,1],frc3[,1])
sigma = c(frc1[,3],frc2[,3],frc3[,3])


prt.sim = w[1]*(mu[1]+sigma[1]*z.sim[,1]) +
  w[2]*(mu[2]+sigma[2]*z.sim[,2])+w[3]*(mu[3]+sigma[3]*z.sim[,3])

prt.sim = sort(prt.sim)
VaR = prt.sim[alpha*N]
ES = mean(prt.sim[1:(alpha*N-1)])

data.table( VaR, ES )




#Метод экстремальных значений
#Рассмотрим случай для выборки значений, превышающих многомерный порог
#Метод оценивает абсолютные значения, поэтому меняем знак у векторов 
#доходности Выбираем значения, превышающих многомерный порог

ESM = -as.matrix(data)

alpha = 1 - 0.10

T = nrow( data )
u = c(sort(ESM[,1])[alpha*T],sort(ESM[,2])[alpha*T],sort(ESM[,3])[alpha*T])
t.ESM = ESM[ ( ESM[,1] > u[1] ) & ( ESM[,2] > u[2] ) & ( ESM[,3] > u[3] ), ]

#Подгоняем модели GPD и получаем значения частных функций

fit1 = fpot( t.ESM[,1],threshold=u[1], model="gpd" )
fit2 = fpot( t.ESM[,2],threshold=u[2], model="gpd", std.err = FALSE)
fit3 = fpot( t.ESM[,3],threshold=u[3], model="gpd", std.err = FALSE)


# CDF
cdf1 = pgpd( t.ESM[,1],loc=u[1],scale=fit1$par[1], shape=fit1$par[2] )
cdf2 = pgpd( t.ESM[,2],loc=u[2],scale=fit2$par[1], shape=fit2$par[2] )
cdf3 = pgpd( t.ESM[,3],loc=u[3],scale=fit3$par[1], shape=fit3$par[2] )

cdf = cbind( cdf1, cdf2, cdf3 )


#Подгонка копулы

gumb.cop = gumbelCopula( dim=3, param = 3 )
gal.cop = galambosCopula( 3 )

gumb.fit = fitCopula( cdf, copula = gumb.cop )
#gal.fit  = fitCopula( cdf, copula = gal.cop  )

data.table(
  gumb = gumb.fit@loglik#,
  #gal  = gal.fit@loglik
)

#Моделируем значения и расчитываем меры риска

cdf.sim = rCopula(n=N,copula=gumb.fit@copula)
sim1 = qgpd(cdf.sim[,1],loc=u[1],scale=fit1$par[1],
            shape=fit1$par[2])
sim2 = qgpd(cdf.sim[,2],loc=u[2],scale=fit2$par[1],
            shape=fit2$par[2])
sim3 = qgpd(cdf.sim[,3],loc=u[3],scale=fit3$par[1],
            shape=fit2$par[2])

loss = sort(w[1]*sim1+w[2]*sim2+w[3]*sim3)

Fu = nrow(t.ESM)/T
alpha = 1-1/(260*Fu)
VaR = loss[alpha*N]
ES = mean(loss[(alpha*N+1):N])

data.table( -VaR, -ES )

##########################################################

пример работы с пакетом rmgarch для двух временных рядов (часть с бэктестом пропущена)

# libraries
library(Rfast)
library(xts)
library(rmgarch)
library(crypto)
no_cores = detectCores() - 1

# choose coins and dates
list_of_coins = c('BTC', 'ETH')
start_date = as.Date("2016-06-01")
end_date = as.Date("2018-05-25")

# download data and combine into one df
all_coin_price = getCoins(coin=list_of_coins, start_date=format(start_date, "%Y%m%d"), end_date=format(end_date, "%Y%m%d"))

needed_columns = c('date', 'close')
all_data = NULL
for (ticket in list_of_coins){
  coin_price = all_coin_price[all_coin_price$symbol==ticket,]
  
  coin_price = coin_price[,needed_columns]
  colnames(coin_price) = c('date', paste('close', ticket, sep='_'))
  
  if (is.null(all_data)){
    all_data = coin_price
  } else{
    all_data = merge(x=all_data, y=coin_price, by='date')
  }
  
  
  if (ncol(all_data) == 0){
    print('No intersection in dates')
    break()
  }
}


# preprocess data
all_data$date = as.Date(all_data$date)
#all_data[,-1] = data.frame(lapply(all_data[,-1], function(x) (x - mean(x))/sd(x) )) # mean(x)
all_data_ts = xts(all_data[,-1], order.by=all_data$date)
all_data_ts_diff = diff(log(all_data_ts))[-1,]


# SOME PARAMS FOR CALCULATION
p = 1
n_sim = 2*10^4
alpha = 0.05

n_series = ncol(all_data_ts_diff)
T_series = nrow(all_data_ts_diff)

# specify univariate model for volatility -- sGARCH
garch.spec = ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                        distribution.model='norm')

# specify multivariate model for mean and cov matrix -- VAR(1) + DCC or Copula
spec.DCC_mvnorm = dccspec(uspec = multispec( replicate(n_series, garch.spec) ),
                          VAR = TRUE, robust = TRUE, lag = p,
                          dccOrder = c(1,1),
                          model = 'DCC',
                          distribution = 'mvnorm')

spec.Copula_mvt = cgarchspec(uspec=multispec( replicate(n_series, garch.spec) ),
                             VAR = TRUE, robust = TRUE, lag = p,
                             dccOrder = c(1, 1), asymmetric = FALSE,
                             distribution.model = list(copula = "mvt",
                                                       method = "Kendall", 
                                                       time.varying = FALSE, 
                                                       transformation = "empirical"))

# fit DCC model and make a prediction
cl = makeCluster(no_cores)
dcc.fit = dccfit(spec.DCC_mvnorm, data = all_data_ts_diff, cluster=cl)
stopCluster(cl)

pred_x = dccforecast(dcc.fit, n.ahead=1)
fitted(pred_x)


# fit Copula model and make simulations and then a prediction
cl = makeCluster(no_cores)
cgarch.fit = cgarchfit(spec.Copula_mvt, data = all_data_ts_diff, cluster=cl)
stopCluster(cl)

cl = makeCluster(no_cores)
sim = cgarchsim(cgarch.fit, n.sim=1, m.sim=n_sim, startMethod = "sample", cluster = cl, rseed=42)
stopCluster(cl)
sim_x = do.call(rbind,  sim@msim$simX)

ms = colMeans(sim_x)
sds = apply(sim_x, 2, sd)


# obtain skew and shape
k = 0
skew = coef(cgarch.fit)[paste(c('[', colnames(all_data_ts)[k], '].skew'), sep='', collapse = '')]
shape = coef(cgarch.fit)[paste(c('[', colnames(all_data_ts)[k], '].shape'), sep='', collapse = '')]


# use this fucntions for conducting VaR and ES tests after backtesting
VaRTest(alpha=alpha, conf.level=0.95, actual=temp_real, VaR=temp_VaR)
ESTest(alpha=0.05, conf.level = 0.95, actual=temp_real, ES=temp_ES, VaR=temp_VaR, boot=TRUE)