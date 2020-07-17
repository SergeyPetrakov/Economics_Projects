import pandas as pd
import sqlite3 as db
import requests

# create empty list to store our data scraped from web
frames = []

# create a string of webpages which we will feed into the html parser
s = '''

http://www.etf.com/channels/global-ex-us-etfs

'''

# split urls in string and feed into pandas html parser to create dataframes of scraped info
for i in s.split():
    print ("Scraping data from {}.".format(i))
    df = pd.read_html(requests.get(i ,headers={'User-agent': 'Mozilla/5.0'}).text)
    # df holds multiple DataFrames - index [5] is the DataFrame we
    # are interested in the scraped data has slightly different
    # headings across different webpages, so we reset the
    # column headings to be identical for each DataFrame
    # these names also match the columns we
    # will set up in out SQLite database
    df[5].columns = ['TICKER' ,'FUND NAME' ,'SPREAD %']
    frames.append(df[5])

# create a "master" dataframe which concatenates all the relevant
# (index 5) DataFrames together
masterFrame = pd.concat(frames)

# create connection to our previously created SQLite database
# use the path and name which corresponds to your local database
cnx = db.connect(r'C:\Users\2\sqlite\first_data_base.db')
cur = cnx.cursor()

# remove table if already exists and any data it contains
cur.execute('DROP TABLE IF EXISTS etftable;')

# create the table within the database
sql = '''CREATE TABLE etftable ('TICKER' TEXT,'FUND NAME' TEXT,'SPREAD %' TEXT)'''

cur.execute(sql)

# append the data
masterFrame.to_sql(name='etftable', con=cnx, if_exists = 'append', index=False)

cnx.close()





