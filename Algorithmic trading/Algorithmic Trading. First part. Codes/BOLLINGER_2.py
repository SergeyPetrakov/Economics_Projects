# make the necessary imports
import pandas as pd
from pandas_datareader import data, wb
import numpy as np
import matplotlib.pyplot as plt
import quandl


# download Dax data from the start of 2015 and store in a Pandas DataFrame
df = quandl.get("CHRIS/EUREX_FDAX1", authtoken="5GGEggAyyGa6_mVsKrxZ", start_date="2015-01-01")

df.head()

# Set number of days and standard deviations to use for rolling lookback period for Bollinger band calculation
window = 21
no_of_std = 2

# Calculate rolling mean and standard deviation using number of days set above
rolling_mean = df['Settle'].rolling(window).mean()
rolling_std = df['Settle'].rolling(window).std()

# create two new DataFrame columns to hold values of upper and lower Bollinger bands
df['Rolling Mean'] = rolling_mean
df['Bollinger High'] = rolling_mean + (rolling_std * no_of_std)
df['Bollinger Low'] = rolling_mean - (rolling_std * no_of_std)



df[['Settle','Bollinger High','Bollinger Low']].plot()
plt.show()

# Create an "empty" column as placeholder for our /position signals
df['Position'] = None

# Fill our newly created position column - set to sell (-1) when the price hits the upper band, and set to buy (1) when it hits the lower band
for row in range(len(df)):

    if (df['Settle'].iloc[row] > df['Bollinger High'].iloc[row]) and (
            df['Settle'].iloc[row - 1] < df['Bollinger High'].iloc[row - 1]):
        df['Position'].iloc[row] = -1

    if (df['Settle'].iloc[row] < df['Bollinger Low'].iloc[row]) and (
            df['Settle'].iloc[row - 1] > df['Bollinger Low'].iloc[row - 1]):
        df['Position'].iloc[row] = 1

    # Forward fill our position column to replace the "None" values with the correct long/short positions to represent the "holding" of our position
# forward through time
df['Position'].fillna(method='ffill', inplace=True)

# Calculate the daily market return and multiply that by the position to determine strategy returns
df['Market Return'] = np.log(df['Settle'] / df['Settle'].shift(1))
df['Strategy Return'] = df['Market Return'] * df['Position']

# Plot the strategy returns
df['Strategy Return'].cumsum().plot()
plt.show()


def bollinger_strat(df, window, std):
    rolling_mean = df['Settle'].rolling(window).mean()
    rolling_std = df['Settle'].rolling(window).std()

    df['Bollinger High'] = rolling_mean + (rolling_std * no_of_std)
    df['Bollinger Low'] = rolling_mean - (rolling_std * no_of_std)

    df['Short'] = None
    df['Long'] = None
    df['Position'] = None

    for row in range(len(df)):

        if (df['Settle'].iloc[row] > df['Bollinger High'].iloc[row]) and (
                df['Settle'].iloc[row - 1] < df['Bollinger High'].iloc[row - 1]):
            df['Position'].iloc[row] = -1

        if (df['Settle'].iloc[row] < df['Bollinger Low'].iloc[row]) and (
                df['Settle'].iloc[row - 1] > df['Bollinger Low'].iloc[row - 1]):
            df['Position'].iloc[row] = 1

    df['Position'].fillna(method='ffill', inplace=True)

    df['Market Return'] = np.log(df['Settle'] / df['Settle'].shift(1))
    df['Strategy Return'] = df['Market Return'] * df['Position']

    df['Strategy Return'].cumsum().plot()




bollinger_strat(df,50,2)
plt.show()

# Set up "daily look back period" and "number of standard deviation" vectors
# For example the first one creates a vector of 20 evenly spaced integer values ranging from 10 to 100
# The second creates a vector of 10 evenly spaced floating point numbers from 1 to 3
windows = np.linspace(10, 100, 20, dtype=int)
stds = np.linspace(1, 3, 10)

# And iterate through them both, running the strategy function each time
for window in windows:
    for std in stds:
        bollinger_strat(df, window, std)
plt.show()
