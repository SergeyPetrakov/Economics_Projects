
# import needed modules
# import needed modules
from datetime import datetime
from pandas_datareader import data
import pandas as pd
import numpy as np
from numpy import log, polyfit, sqrt, std, subtract
import statsmodels.tsa.stattools as ts
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
import pprint
import sqlite3 as db

# set the database file path we wish to connect to
# this will obviously be unique to wherever you created
# the SQLite database on your local system
database = 'C:\Users\2\sqlite\first_data_base.db'

# this is the SQL statement containing the information
# regarding which tickers we want to pull from the database
# As an example I have chosen to pull down all tickers which
# have their "Focus" listed as being "Silver"
sql = 'SELECT Ticker FROM etftable WHERE Niche = "Value";'

# create a connection to the database specified above
cnx = db.connect(database)
cur = cnx.cursor()

# execute the SQL statement and place the results into a
# variable called "tickers"
tickers = pd.read_sql(sql, con=cnx)

# create an empty list
symbList = []

# iterate over the DataFrame and append each item into the empty list
for i in xrange(len(tickers)):
    symbList.append(tickers.ix[i][0])


def get_symb_pairs(symbList):
    '''symbList is a list of ETF symbols
       This function takes in a list of symbols and
       returns a list of unique pairs of symbols'''

    symbPairs = []

    i = 0

    # iterate through the list and create all possible combinations of
    # ticker pairs - append the pairs to the "symbPairs" list
    while i < len(symbList) - 1:
        j = i + 1
        while j < len(symbList):
            symbPairs.append([symbList[i], symbList[j]])
            j += 1
            i += 1

            # iterate through the newly created list of pairs and remove any pairs #made up of two identical tickers
    for i in symbPairs:
        if i[0] == i[1]:
            symbPairs.remove(i)
            # create a new empty list to store only unique pairs
    symbPairs2 = []
    # iterate through the original list and append only unique pairs to the
    # new list
    for i in symbPairs:
        if i not in symbPairs2:
            symbPairs2.append(i)

    return symbPairs2


symbPairs = get_symb_pairs(symbList)


def backtest(symbList):
    start_date = '2012/01/01'
    end_date = datetime.now()
    # download data from Yahoo Finance
    y = data.DataReader(symbList[0], "yahoo", start=start_date, end=end_date)
    x = data.DataReader(symbList[1], "yahoo", start=start_date, end=end_date)

    # rename column to make it easier to work with later
    y.rename(columns={'Adj Close': 'price'}, inplace=True)
    x.rename(columns={'Adj Close': 'price'}, inplace=True)

    # make sure DataFrames are the same length
    min_date = max(df.dropna().index[0] for df in [y, x])
    max_date = min(df.dropna().index[-1] for df in [y, x])
    y = y[(y.index >= min_date) & amp;
    (y.index <= max_date)]
    x = x[(x.index >= min_date) & amp;
    (x.index <= max_date)]

    ############################################################

    plt.plot(y.price, label=symbList[0])
    plt.plot(x.price, label=symbList[1])
    plt.ylabel('Price')
    plt.xlabel('Time')
    plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
    plt.show()

    #############################################################

    sns.jointplot(y.price, x.price, color='b')
    plt.show()

    #############################################################

    # run Odinary Least Squares regression to find hedge ratio
    # and then create spread series
    df1 = pd.DataFrame({'y': y['price'], 'x': x['price']})
    est = sm.OLS(df1.y, df1.x)
    est = est.fit()
    df1['hr'] = -est.params[0]
    df1['spread'] = df1.y + (df1.x * df1.hr)

    ##############################################################

    plt.plot(df1.spread)
    plt.show()

    ##############################################################

    cadf = ts.adfuller(df1.spread)
    print
    'Augmented Dickey Fuller test statistic =', cadf[0]
    print
    'Augmented Dickey Fuller p-value =', cadf[1]
    print
    'Augmented Dickey Fuller 1%, 5% and 10% test statistics =', cadf[4]

    ##############################################################

    def hurst(ts):
        """Returns the Hurst Exponent of the time series vector ts"""
        # Create the range of lag values
        lags = range(2, 100)

        # Calculate the array of the variances of the lagged differences
        tau = [sqrt(std(subtract(ts[lag:], ts[:-lag]))) for lag in lags]

        # Use a linear fit to estimate the Hurst Exponent
        poly = polyfit(log(lags), log(tau), 1)

        # Return the Hurst exponent from the polyfit output
        return poly[0] * 2.0

    ##############################################################

    print
    "Hurst Exponent =", round(hurst(df1.spread), 2)

    ##############################################################

    # Run OLS regression on spread series and lagged version of itself
    spread_lag = df1.spread.shift(1)
    spread_lag.ix[0] = spread_lag.ix[1]
    spread_ret = df1.spread - spread_lag
    spread_ret.ix[0] = spread_ret.ix[1]
    spread_lag2 = sm.add_constant(spread_lag)

    model = sm.OLS(spread_ret, spread_lag2)
    res = model.fit()

    halflife = round(-np.log(2) / res.params[1], 0)

    if halflife <= 0:
        halflife = 1

    print
    'Halflife = ', halflife

    ##############################################################

    meanSpread = df1.spread.rolling(window=halflife).mean()
    stdSpread = df1.spread.rolling(window=halflife).std()

    df1['zScore'] = (df1.spread - meanSpread) / stdSpread

    df1['zScore'].plot()
    plt.show()

    ##############################################################

    entryZscore = 2
    exitZscore = 0

    # set up num units long
    df1['long entry'] = ((df1.zScore < - entryZscore) & (df1.zScore.shift(1) > - entryZscore))
    df1['long exit'] = ((df1.zScore > - exitZscore) & (df1.zScore.shift(1) < - exitZscore))
    df1['num units long'] = np.nan
    df1.loc[df1['long entry'], 'num units long'] = 1
    df1.loc[df1['long exit'], 'num units long'] = 0
    df1['num units long'][0] = 0
    df1['num units long'] = df1['num units long'].fillna(
        method='pad')  # set up num units short df1['short entry'] = ((df1.zScore >  entryZscore) & ( df1.zScore.shift(1) < entryZscore))
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

    plt.plot(df1['cum rets'])
    plt.xlabel(i[1])
    plt.ylabel(i[0])
    plt.show()

    ##############################################################

    start_val = 1
    end_val = df1['cum rets'].iat[-1]

    start_date = df1.iloc[0].name
    end_date = df1.iloc[-1].name
    days = (end_date - start_date).days

    CAGR = round(((float(end_val) / float(start_val)) ** (252.0 / days)) - 1, 4)

    print
    "CAGR = {}%".format(CAGR * 100)
    print
    "Sharpe Ratio = {}".format(round(sharpe, 2))
    print
    100 * "----"


for i in symbPairs:
    backtest(i)