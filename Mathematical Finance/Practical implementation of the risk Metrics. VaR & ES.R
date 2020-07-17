library( QuantTools ) # Market data loading
library( data.table ) # Library for data management
library( ghyp )       # GHP
library( copula )     # Copula
library( fGarch )     # GARCH   
library( evd )        # Extreme Value

# download data
stock_1 = get_yahoo_data( "AAPL", from = "2013-01-02", to = "2020-01-01" )
stock_2 = get_yahoo_data( "MCD" , from = "2013-01-02", to = "2020-01-01" )

# cal? returns
stock_1 = stock_1[, .( date, return_1 = close / shift( close, fill = close[1] ) - 1 )]
stock_2 = stock_2[, .( date, return_2 = close / shift( close, fill = close[1] ) - 1 )]

# merge to portfolio
portfolio = merge( stock_1, stock_2, by = "date" )
data = as.vector( portfolio[, .( return_1, return_2 ) ] )

# pnl as cumulative products
plot_dts( portfolio[ , .(date, AAPL = cumprod(1+return_1), MCD = cumprod(1+return_2)) ] )

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

# weights
weights = CJ(
  w1 = seq( 0.0, 1, 0.05),
  w2 = seq( 0.0, 1, 0.05)
)
weights = weights[ w1+w2 == 1 ]

result = weights[, calc_perfomance( portfolio$return_1*w1 + portfolio$return_2*w2  )
                 , by = .( w1, w2 ) ]

result[order(-pnl)][1:5]

result[order(-max_dd)][1:5]

result[order(-sharpe)][1:5]

# sum returns
portf_eq         = portfolio[, .(date, pnl_eq     = return_1*0.5 + return_2*0.5)   ]
portf_max_pnl    = portfolio[, .(date, pnl_pnl    = return_1*1 + return_2*0) ]
portf_max_sharpe = portfolio[, .(date, pnl_sharpe = return_1*0.3 + return_2*0.7 ) ]
portf_min_max_dd = portfolio[, .(date, pnl_min_max_dd = return_1*0.2 + return_2*0.8 ) ]
# calc pnl
portf_eq        [, pnl_eq     := cumprod( 1 + pnl_eq)     ]
portf_max_pnl   [, pnl_pnl    := cumprod( 1 + pnl_pnl)    ]
portf_max_sharpe[, pnl_sharpe := cumprod( 1 + pnl_sharpe) ]
portf_min_max_dd[, pnl_min_max_dd := cumprod( 1 + pnl_min_max_dd) ]

plot_dts(
  portf_eq,
  portf_max_pnl,
  portf_max_sharpe,
  portf_min_max_dd
)

#ghyp
#???????? ?????? ?????? (?? ???????? ??????)

aic.mv = stepAIC.ghyp( data, dist = c( "gauss", "t", "ghyp" ), symmetric=NULL, silent=T )

aic.mv$best.model@model[1]

#????????? ?????? ? ??????? VaR ? ES

prt.fit = fit.tmv( data, symmetric=T, silent=T )

w     = c(0.5,0.5)
N     = 10^6
alpha = 0.05

sim = rghyp( n = N, object = prt.fit )
prt.sim = w[1] * sim[,1] + w[2] * sim[,2]
prt.sim = sort(prt.sim)

VaR = prt.sim[ alpha*N ]
ES  = mean( prt.sim[ 1:( alpha * N-1 ) ] )

data.table( VaR, ES )

#?????? ??????????? ????????

opt = portfolio.optimize(prt.fit, risk.measure= "value.at.risk", 
                         type="minimum.risk", silent=T,)
opt$opt.weights

w = opt$opt.weights
prt.sim = w[1]*sim[,1]+w[2]*sim[,2]
prt.sim = sort(prt.sim)

VaR = prt.sim[ alpha*N ]
ES  = mean( prt.sim[ 1:( alpha * N-1 ) ] )

data.table( VaR, ES )

#copula
#???????? ?????? ?????? ??? ?????? ??????

stock1.fit = stepAIC.ghyp( data$return_1, dist = c( "gauss", "t", "ghyp" ),
                           symmetric=NULL, silent=T)$best.model

stock2.fit = stepAIC.ghyp( data$return_2, dist = c( "gauss", "t", "ghyp" ),
                           symmetric=NULL, silent=T)$best.model

stock1.fit@model[1]

stock2.fit@model[1]

stock1.cdf = pghyp( data$return_1, object = stock1.fit )
stock2.cdf = pghyp( data$return_2, object = stock2.fit )

cdf = cbind( stock1.cdf, stock2.cdf )
plot( cdf )

# define copulas
norm.cop = normalCopula( dim=2, param=0.5, dispstr="un" )
stud.cop = tCopula     ( dim=2, param=0.5 )
gumb.cop = gumbelCopula( dim=2, param=2 )

# Copula plot
persp( norm.cop, dCopula )

persp( stud.cop, dCopula )

persp( gumb.cop, dCopula )

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

#??????? Var ? ES

N = 10^3
stud.sim = rCopula( n=N, copula = stud.fit@copula )

stock1.sim = qghyp( stud.sim[,1], object=stock1.fit )
stock2.sim = qghyp( stud.sim[,2], object=stock2.fit )

w = c(0.5,0.5)
prt.sim = w[1]*stock1.sim + w[2]*stock2.sim

alpha = 0.05
prt.sim = sort(prt.sim)
VaR = prt.sim[alpha*N]
ES = mean(prt.sim[1:(alpha*N-1)])

data.table( VaR, ES )

#GARCH
#????????? GARCH ??????, ??????????????? ??????? ? ???????? ?????????????

stock1.gfit = garchFit( data = data$return_1, formula =~garch(1,1),
                        cond.dist="ged", trace=F )

stock2.gfit = garchFit( data = data$return_2, formula =~garch(1,1),
                        cond.dist="ged", trace=F )

# stand residuals
z = matrix( nrow=nrow(data), ncol=2 )
z[,1] = stock1.gfit@residuals / stock1.gfit@sigma.t
z[,2] = stock2.gfit@residuals / stock2.gfit@sigma.t

# CDF for residuals
mean = c(0,0); sd = c(1,1); nu = c(1.3,1.3)
xi = c(1, 1)
cdf = matrix(nrow=nrow(data), ncol=2)

cdf[,1] = psged(z[,1], mean=mean[1], sd=sd[1], nu=nu[1], xi=xi[1])
cdf[,2] = psged(z[,2], mean=mean[2], sd=sd[2], nu=nu[2], xi=xi[2])
psged()
#???????? ?????

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

#????????? ????

cdf.sim = rCopula( n=N, copula=norm.fit@copula )
z.sim = matrix( nrow=N, ncol=2 )
for (i in 1:2) 
  z.sim[,i] = qsged(cdf.sim[,i], mean=mean[i], sd=sd[i], nu=nu[i], xi=xi[i] )

frc1 = predict( stock1.gfit, n.ahead=1 )
frc2 = predict( stock2.gfit, n.ahead=1 )
mu = c(frc1[,1],frc2[,1])
sigma = c(frc1[,3],frc2[,3])


prt.sim = w[1]*(mu[1]+sigma[1]*z.sim[,1]) +
  w[2]*(mu[2]+sigma[2]*z.sim[,2])

prt.sim = sort(prt.sim)
VaR = prt.sim[alpha*N]
ES = mean(prt.sim[1:(alpha*N-1)])

data.table( VaR, ES )

#????? ????????????? ????????
#?????????? ?????? ??? ??????? ????????, 
#??????????? ??????????? ?????

#????? ????????? ?????????? ????????, ??????? ?????? ???? 
#? ???????? ?????????? ???????? ????????, ??????????? ??????????? ?????

ESM = -as.matrix(data)

alpha = 1 - 0.10

T = nrow( data )
u = c(sort(ESM[,1])[alpha*T],sort(ESM[,2])[alpha*T])
t.ESM = ESM[ ( ESM[,1] > u[1] ) & ( ESM[,2] > u[2] ), ]

#????????? ?????? GPD ? ???????? ???????? ??????? ???????

fit1 = fpot( t.ESM[,1],threshold=u[1], model="gpd" )
fit2 = fpot( t.ESM[,2],threshold=u[2], model="gpd" )

# CDF
cdf1 = pgpd( t.ESM[,1],loc=u[1],scale=fit1$par[1], shape=fit1$par[2] )
cdf2 = pgpd( t.ESM[,2],loc=u[2],scale=fit2$par[1], shape=fit2$par[2] )
cdf = cbind( cdf1, cdf2 )

#???????? ??????

gumb.cop = gumbelCopula( dim=2, param = 2 )
gal.cop = galambosCopula( 2 )

gumb.fit = fitCopula( cdf, copula = gumb.cop )
#gal.fit  = fitCopula( cdf, copula = gal.cop  )

data.table(
  gumb = gumb.fit@loglik#,
  #gal  = gal.fit@loglik
)

#?????????? ???????? ? ??????????? ???? ?????
cdf.sim = rCopula(n=N,copula=gumb.fit@copula)
sim1 = qgpd(cdf.sim[,1],loc=u[1],scale=fit1$par[1],
            shape=fit1$par[2])
sim2 = qgpd(cdf.sim[,2],loc=u[2],scale=fit2$par[1],
            shape=fit2$par[2])

loss = sort(w[1]*sim1+w[2]*sim2)

Fu = nrow(t.ESM)/T
alpha = 1-1/(260*Fu)
VaR = loss[alpha*N]
ES = mean(loss[(alpha*N+1):N])

data.table( -VaR, -ES )











