##### import the necessary modules and set chart style####
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib as mpl
mpl.style.use('bmh')
import pandas_datareader.data as web
import matplotlib.pylab as plt
from datetime import datetime
import statsmodels.api as sm
from pykalman import KalmanFilter
from math import sqrt

# scrape html from website and store 3rd DataFrame as our stock tickers - this is dictated to us by the structure of the html
stock_list = pd.read_html("https://www.marketwatch.com/tools/industry/stocklist.asp?bcind_ind=9535&amp;bcind_period=3mo")[3]

# convert the DataFrame of stocks into a list so we can easily iterate over it
stocks = stock_list[1].dropna()[1:].tolist()

# set empty list o hold the stock price DataFrames that we can later concatenate into a master frame
df_list = []

# not all stocks will return data so set up an empty list to store the stock tickers that actually successfully returns data
used_stocks = []

# iterate over stock tickers in list and download relevant data, storing said data and successfully downloaded tickers along the way
for stock in stocks:
    try:
        data = pd.DataFrame(web.DataReader(stock, data_source='iex', start='01/01/2016')['close'])
        data.columns = [stock]
        df_list.append(data)
        used_stocks.append(stock)
    except:
        pass

# concatenate list of individual tciker price DataFrames into one master DataFrame
df = pd.concat(df_list, axis=1)


df.plot(figsize=(20,10))
plt.show()


#NOTE CRITICAL LEVEL HAS BEEN SET TO 5% FOR COINTEGRATION TEST
def find_cointegrated_pairs(dataframe, critial_level = 0.05):
    n = dataframe.shape[1] # the length of dateframe
    pvalue_matrix = np.ones((n, n)) # initialize the matrix of p
    keys = dataframe.columns # get the column names
    pairs = [] # initilize the list for cointegration
    for i in range(n):
        for j in range(i+1, n): # for j bigger than i
            stock1 = dataframe[keys[i]] # obtain the price of "stock1"
            stock2 = dataframe[keys[j]]# obtain the price of "stock2"
            result = sm.tsa.stattools.coint(stock1, stock2) # get conintegration
            pvalue = result[1] # get the pvalue
            pvalue_matrix[i, j] = pvalue
            if pvalue < critial_level: # if p-value less than the critical level
                pairs.append((keys[i], keys[j], pvalue)) # record the contract with that p-value
    return pvalue_matrix, pairs


# set up the split point for our "training data" on which to perform the co-integration test (the remaining dat awill be fed to our backtest function)
split = int(len(df))
print('split=%f' % (split))

# run our dataframe (up to the split point) of ticker price data through our co-integration function and store results
pvalue_matrix, pairs = find_cointegrated_pairs(df[:23])

# convert our matrix of stored results into a DataFrame
pvalue_matrix_df = pd.DataFrame(pvalue_matrix)

# use Seaborn to plot a heatmap of our results matrix
fig, ax = plt.subplots(figsize=(15, 10))
sns.heatmap(pvalue_matrix_df, xticklabels=used_stocks, yticklabels=used_stocks, ax=ax)
plt.show()


for pair in pairs:
    print("Stock {} and stock {} has a co-integration score of {}".format(pair[0],pair[1],round(pair[2],4)))


def KalmanFilterAverage(x):
    # Construct a Kalman filter
    kf = KalmanFilter(transition_matrices=[1],
                      observation_matrices=[1],
                      initial_state_mean=0,
                      initial_state_covariance=1,
                      observation_covariance=1,
                      transition_covariance=0.01)

    # Use the observed values of the price to get a rolling mean
    state_means, state_covs = kf.filter(x.values)
    state_means = pd.Series(state_means.flatten(), index=x.index)
    return state_means


# Kalman filter regression
def KalmanFilterRegression(x, y):
    delta = 0.001
    trans_cov = delta / (1 - delta) * np.eye(2)  # How much random walk wiggles
    obs_mat = np.expand_dims(np.vstack([[x], [np.ones(len(x))]]).T, axis=1)

    kf = KalmanFilter(n_dim_obs=1, n_dim_state=2,  # y is 1-dimensional, (alpha, beta) is 2-dimensional
                      initial_state_mean=[0, 0],
                      initial_state_covariance=np.ones((2, 2)),
                      transition_matrices=np.eye(2),
                      observation_matrices=obs_mat,
                      observation_covariance=2,
                      transition_covariance=trans_cov)

    # Use the observations y to get running estimates and errors for the state parameters
    state_means, state_covs = kf.filter(y.values)
    return state_means


def half_life(spread):
    spread_lag = spread.shift(1)
    spread_lag.iloc[0] = spread_lag.iloc[1]
    spread_ret = spread - spread_lag
    spread_ret.iloc[0] = spread_ret.iloc[1]
    spread_lag2 = sm.add_constant(spread_lag)
    model = sm.OLS(spread_ret, spread_lag2)
    res = model.fit()
    halflife = int(round(-np.log(2) / res.params[1], 0))

    if halflife <= 0:
        halflife = 1
    return halflife


def backtest(df, s1, s2):
    #############################################################
    # INPUT:
    # DataFrame of prices
    # s1: the symbol of contract one
    # s2: the symbol of contract two
    # x: the price series of contract one
    # y: the price series of contract two
    # OUTPUT:
    # df1['cum rets']: cumulative returns in pandas data frame
    # sharpe: Sharpe ratio
    # CAGR: Compound Annual Growth Rate

    x = df[s1]
    y = df[s2]

    # run regression (including Kalman Filter) to find hedge ratio and then create spread series
    df1 = pd.DataFrame({'y': y, 'x': x})
    df1.index = pd.to_datetime(df1.index)
    state_means = KalmanFilterRegression(KalmanFilterAverage(x), KalmanFilterAverage(y))

    df1['hr'] = - state_means[:, 0]
    df1['spread'] = df1.y + (df1.x * df1.hr)

    # calculate half life
    halflife = half_life(df1['spread'])

    # calculate z-score with window = half life period
    meanSpread = df1.spread.rolling(window=halflife).mean()
    stdSpread = df1.spread.rolling(window=halflife).std()
    df1['zScore'] = (df1.spread - meanSpread) / stdSpread

    ##############################################################
    # trading logic
    entryZscore = 2
    exitZscore = 0

    # set up num units long
    df1['long entry'] = ((df1.zScore < - entryZscore) & ( df1.zScore.shift(1) >
    - entryZscore))
    df1['long exit'] = ((df1.zScore > - exitZscore) & (df1.zScore.shift(1) <
    - exitZscore))
    df1['num units long'] = np.nan
    df1.loc[df1['long entry'], 'num units long'] = 1
    df1.loc[df1['long exit'], 'num units long'] = 0
    df1['num units long'][0] = 0
    df1['num units long'] = df1['num units long'].fillna(
        method='pad')  # set up num units short df1['short entry'] = ((df1.zScore &gt; entryZscore) &amp; ( df1.zScore.shift(1) &lt; entryZscore))
    df1['short exit'] = ((df1.zScore < exitZscore) & (df1.zScore.shift(1) > exitZscore))
    df1.loc[df1['short entry'], 'num units short'] = -1
    df1.loc[df1['short exit'], 'num units short'] = 0
    df1['num units short'][0] = 0
    df1['num units short'] = df1['num units short'].fillna(method='pad')

    df1['numUnits'] = df1['num units long'] + df1['num units short']
    df1['spread pct ch'] = (df1['spread'] - df1['spread'].shift(1)) / ((df1['x'] * abs(df1['hr'])) + df1['y'])
    df1['port rets'] = df1['spread pct ch'] * df1['numUnits'].shift(1)

    df1['cum rets'] = df1['port rets'].cumsum()
    df1['cum rets'] = df1['cum rets'] + 1

    ##############################################################

    try:
        sharpe = ((df1['port rets'].mean() / df1['port rets'].std()) * sqrt(252))
    except ZeroDivisionError:
        sharpe = 0.0

    ##############################################################
    start_val = 1
    end_val = df1['cum rets'].iat[-1]

    start_date = df1.iloc[0].name
    end_date = df1.iloc[-1].name

    days = (end_date - start_date).days

    CAGR = round(((float(end_val) / float(start_val)) ** (252.0 / days)) - 1, 4)

    df1[s1 + " " + s2] = df1['cum rets']

    return df1[s1 + " " + s2], sharpe, CAGR


results = []

for pair in pairs:
    rets, sharpe, CAGR = backtest(df[split:], pair[0], pair[1])
    results.append(rets)
    print("The pair {} and {} produced a Sharpe Ratio of {} and a CAGR of {}".format(pair[0], pair[1], round(sharpe, 2),
                                                                                     round(CAGR, 4)))
    rets.plot(figsize=(20, 15), legend=True)

# concatenatge together the individual equity curves into a single DataFrame
results_df = pd.concat(results, axis=1).dropna()

# equally weight each equity curve by dividing each by the number of pairs held in the DataFrame
results_df /= len(results_df.columns)

# sum up the equally weighted equity curves to get our final equity curve
final_res = results_df.sum(axis=1)

# plot the chart of our final equity curve
final_res.plot(figsize=(20, 15))
plt.show()

# calculate and print our some final stats for our combined equity curve
sharpe = (final_res.pct_change().mean() / final_res.pct_change().std()) * (sqrt(252))
start_val = 1
end_val = final_res.iloc[-1]

start_date = final_res.index[0]
end_date = final_res.index[-1]

days = (end_date - start_date).days

CAGR = round(((float(end_val) / float(start_val)) ** (252.0 / days)) - 1, 4)
print("Sharpe Ratio is {} and CAGR is {}".format(round(sharpe, 2), round(CAGR, 4)))

