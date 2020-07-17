# libraries
library(Rfast)
library(xts)
library(rmgarch)
library(crypto)
no_cores = detectCores() - 1
 
# choose coins and dates
list_of_coins = c('BTC', 'ETH')
start_date = asDate("2016-06-01")
end_date = asDate("2018-05-25")
 
# download data and combine into one df
all_coin_price = getCoins(coin=list_of_coins, start_date=format(start_date, "%Y%m%d"), end_date=format(end_date, "%Y%m%d"))

needed_columns = c('date', 'close')
all_data = NULL
for (ticket in list_of_coins){
  coin_price = all_coin_price[all_coin_price$symbol==ticket,]
  
  coin_price = coin_price[,needed_columns]
  colnames(coin_price) = c('date', paste('close', ticket, sep='_'))
  
  if (isnull(all_data)){
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
all_data$date = asDate(all_data$date)
#all_data[,-1] = dataframe(lapply(all_data[,-1], function(x) (x - mean(x))/sd(x) )) # mean(x?
all_data_ts = xts(all_data[,-1], orderby=all_data$date)
all_data_ts_diff = diff(log(all_data_ts))[-1,]


# SOME PARAMS FOR CALCULATION
p = 1
n_sim = 2*10^4
alpha = 005

n_series = ncol(all_data_ts_diff)
T_series = nrow(all_data_ts_diff)

# specify univariate model for volatility -- sGARCH
garchspec = ugarchspec(variancemodel = list(model = 'sGARCH', garchOrder = c(1, 1)),
                        meanmodel = list(armaOrder = c(0, 0), includemean = FALSE),
                        distributionmodel=norm)

# specify multivariate model for mean and cov matrix -- VAR(1) + DCC or Copula
specDCC_mvnorm = dccspec(uspec = multispec( replicate(n_series, garchspec) ),
                          VAR = TRUE, robust = TRUE, lag = p,
                          dccOrder = c(1,1),
                          model = 'DCC',
                          distribution = 'mvnorm')

specCopula_mvt = cgarchspec(uspec=multispec( replicate(n_series, garchspec) ),
                             VAR = TRUE, robust = TRUE, lag = p?
                             dccOrder = c(1, 1), asymmetric = FALSE,
                             distributionmodel = list(copula = "mvt",
                                                       method = "Kendall", 
                                                       timevarying = FALSE, 
                                                       transformation = "empirical"))

# fit DCC model and make a prediction
cl = makeCluster(no_cores)
dccfit = dccfit(specDCC_mvnorm, data = all_data_ts_diff, cluster=cl)
stopCluster(cl)

pred_x = dccforecast(dccfit, nahead=1)
fitted(pred_x)


# fit Copula model and make simulations and then a prediction
cl = makeCluster(no_cores)
cgarchfit = cgarchfit(specCopula_mvt, data = all_data_ts_diff, cluster=cl)
stopCluster(cl)

cl = makeCluster(no_cores)
sim = cgarchsim(cgarchfit, nsim=1, msim=n_sim, startMethod = "sample", cluster = cl, rseed=42)
stopCluster(cl)
sim_x = docall(rbind,  sim@msim$simX)

ms = colMeans(sim_x)
sds = apply(sim_x, 2, sd)


# obtain skew?and shape
k = 0
skew = coef(cgarchfit)[paste(c('[', colnames(all_data_ts)[k], ']skew'), sep='', collapse = '')]
shape = coef(cgarchfit)[paste(c('[', colnames(all_data_ts)[k], ']shape'), sep='', collapse = '')]


# use this fucntions for conducting Va? and ES tests after backtesting
VaRTest(alpha=alpha, conflevel=095, actual=temp_real, VaR=temp_VaR)
ESTest(alpha=005, conflevel = 095, actual=temp_real, ES=temp_ES, VaR=temp_VaR, boot=TRUE)