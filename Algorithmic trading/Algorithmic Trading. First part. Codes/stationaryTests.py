# Example 2: Using ADF Test for Mean Reversion
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.tsa.stattools import adfuller
from genhurst import genhurst

df=pd.read_csv("inputData_USDCAD.csv", encoding='utf-8')
y=df.loc[df['Time']==1659, 'Close']
plt.plot(y)
plt.xlabel('July 22, 2007, to March 28, 2012')
plt.ylabel('USD.CAD')
plt.title('USD.CAD Price Series')
plt.show()

results=adfuller(y, maxlag=1, regression='c', autolag=None)
print(results)

# Find Hurst exponent
H, pVal=genhurst(np.log(y))
print("H=%f pValue=%f" % (H, pVal))