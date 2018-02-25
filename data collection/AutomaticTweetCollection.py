# -*- coding: utf-8 -*-
"""
Created on Sat Feb 24 19:46:21 2018

@author: Raquel & Edu
"""

from TwitterAPI import TwitterAPI
import numpy as np
import pandas as pd
from datetime import datetime,timedelta
from pytz import timezone
import json

#==============================================================================
# Functions
#==============================================================================
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

def get_TodaysTweets(screen_name,count=200,exclude_replies = True, max_id = None): #should modify this to be more robust
    '''
    retuns a list with the last (count-deleted) tweets from the user
    '''
    try:
        request = twitter.request('statuses/user_timeline',{'screen_name': screen_name,'count': count,
                                                            'exclude_replies':exclude_replies,'max_id':max_id})
        tweet_list = request.json()
        yesterday_DT = datetime.now(timezone('US/Central'))-timedelta(days=1)
        todaysTweets = []
        if tweet_list == []:
            return todaysTweets
        elif not (strToChicago_DT(tweet_list[-1]['created_at']) > yesterday_DT): #last of the 200 was in the last 24h
            todaysTweets =  [t for t in tweet_list if strToChicago_DT(t['created_at']) > yesterday_DT]
            return todaysTweets
        else:
            todaysTweets = tweet_list.copy()
            max_id = todaysTweets[-1]['id']
            todaysTweets += get_TodaysTweets(screen_name,count=200,exclude_replies = True, max_id = max_id)[1:]
            return todaysTweets
    except Exception as e:
        print (screen_name +str(request.json()['errors']))
        return np.NaN
    
def strToChicago_DT(string):
    UTC_DT = datetime.strptime(string,'%a %b %d %H:%M:%S +0000 %Y').replace(tzinfo = timezone('UTC'))
    Chicago_DT = UTC_DT.astimezone(timezone('US/Central'))
    return Chicago_DT

#==============================================================================
# Main
#==============================================================================
if __name__ == '__main__':
    #Import Accounts
    accounts_df = pd.read_csv('Data/accounts.csv',sep=";")
    #Collect Data
    twitter = get_twitter()
    accounts = accounts_df['Account'] #pd series
    todaysTweets = pd.DataFrame(accounts, columns=['Account'])
    date=str(datetime.now().date())
    todaysTweets[date] = np.NaN
    todaysTweets[date] = todaysTweets['Account'].apply(get_TodaysTweets)
    #Save Data
    todaysTweets_json = todaysTweets.to_json(orient='records')
    with open('Data/todaysTweets_'+date+'.json','w') as fp:
        json.dump(todaysTweets_json,fp)
    #Add to historic
        #TODO