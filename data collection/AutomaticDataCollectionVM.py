# -*- coding: utf-8 -*-
"""
Created on Sat Feb 17 20:20:17 2018

@author: Master
"""

from TwitterAPI import TwitterAPI
import numpy as np
import pandas as pd
from datetime import datetime

#Functions
def get_twitter():
    """ 
    Generates twitter connection
    """
    
    #Tokens
    consumer_key='nDx2bQS2DVdXEZd0b8MwaEG2L'
    consumer_secret='BeA1VuBq7hyi01fo4LFD13YAcrDgeYxIJTQgzx4gg8H5fSc3FR'
    access_token='900568210424164352-pKLEmPfNcqyGi3q67pyK3bJYtc5G01S'
    access_token_secret='uGbegX9whUY0FjZS6tgIrDITVOM8Q6tKMODNVPG2sT9Aq'
    
    
    twitter = TwitterAPI(
                   consumer_key,
                   consumer_secret,
                   access_token,
                   access_token_secret)
    return twitter

def get_NumberofFollowers(screen_name):       #does not crash when Exception, but does not correct exceptions either.
    '''
    returns the number of friends the user has
    '''
    try:
        request=twitter.request('users/lookup',{'screen_name':screen_name})
        return request.json()[0]['followers_count']
    except Exception as e:
        print (screen_name +str(request.json()['errors']))
        return np.NaN
  
if __name__ == "__main__":
    #import accounts
    accounts_df = pd.read_csv('DataVM1/accounts.csv',sep=";")
    #collect data
    twitter = get_twitter()
    accounts = accounts_df['Account'] #pd series
    todaysFollowers = pd.DataFrame(accounts, columns=['Account'])
    date=str(datetime.now().date())
    todaysFollowers[date] = np.NaN
    todaysFollowers[date] = todaysFollowers['Account'].apply(get_NumberofFollowers)
    #save Data
    todaysFollowers.to_csv('DataVM1/todaysFollowers_'+date+'.csv',sep=';',index=False)
    ##add to historic data
    historicData = pd.read_csv('DataVM1/todaysFollowers_all.csv',sep = ';') #load historic
    historicData = pd.concat([historicData,todaysFollowers[date]],axis=1) #add todays column
    historicData.to_csv('DataVM1/todaysFollowers_all.csv',sep=';',index=False) #save
