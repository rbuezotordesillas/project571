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
import matplotlib.pyplot as plt
import time
import sys

#==============================================================================
# Twitter Functions
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
        assert (type(request.json())==list) 
    except AssertionError:
        print(request.json())
        return np.NaN
    else:
        tweet_list = request.json()
        yesterday_DT = datetime.now(timezone('US/Central'))-timedelta(days=1)
        todaysTweets = []
        if tweet_list == []:
            pb.increase()
            return todaysTweets
        elif not (strToChicago_DT(tweet_list[-1]['created_at']) > yesterday_DT): #last of the 200 was in the last 24h
            todaysTweets =  [t for t in tweet_list if strToChicago_DT(t['created_at']) > yesterday_DT]
            pb.increase()
            return todaysTweets
        else:
            todaysTweets = tweet_list.copy()
            max_id = todaysTweets[-1]['id']
            todaysTweets += get_TodaysTweets(screen_name,count=200,exclude_replies = True, max_id = max_id)[1:]
            return todaysTweets

#==============================================================================
# Helper Functions
#==============================================================================
    
def strToChicago_DT(string):
    UTC_DT = datetime.strptime(string,'%a %b %d %H:%M:%S +0000 %Y').replace(tzinfo = timezone('UTC'))
    Chicago_DT = UTC_DT.astimezone(timezone('US/Central'))
    return Chicago_DT

class progress_bar(object):
    def __init__(self,length,startingValue = 0):
        self.length = length
        self.progress = startingValue
    def increase(self,by = 1):
        self.progress += 1/self.length
        self.update_progress(self.progress)
    def update_progress(self,progress):
        barLength = 10 # Modify this to change the length of the progress bar
        status = ""
        if isinstance(progress, int):
            progress = float(progress)
        if not isinstance(progress, float):
            progress = 0
            status = "error: progress var must be float\r\n"
        if progress < 0:
            progress = 0
            status = "Halt...\r\n"
        if progress >= 1:
            progress = 1
            status = "Done...\r\n"
        block = int(round(barLength*progress))
        text = "\rPercent: [{0}] {1}% {2}".format( "#"*block + "-"*(barLength-block), round(progress*100,2), status)
        sys.stdout.write(text)
        sys.stdout.flush()

#==============================================================================
# Main
#==============================================================================
if __name__ == '__main__':
	print('Starting...')
	#Import Accounts
	accounts_df = pd.read_csv('Data/accounts.csv',sep=";")
	#Collect Data
	twitter = get_twitter()
	accounts = accounts_df['Account'] #pd series
	todaysTweets = pd.DataFrame(accounts, columns=['Account'])
	date=str(datetime.now().date())
	todaysTweets[date] = np.NaN
	pb = progress_bar(len(accounts_df))
	print('Collecting Todays Tweets')
	todaysTweets[date] = todaysTweets['Account'].apply(get_TodaysTweets)
	print('Saving...')
	#Save Data
	todaysTweets_json = todaysTweets.to_json(orient='records')
	with open('Data/todaysTweets_'+date+'.json','w') as fp:
		json.dump(todaysTweets_json,fp)
	# #Add to historic
	# with open('Data/todaysTweets_all.json','r') as fp: #load historic tweets
		# historicTweets_json = json.load(fp)
	# historicTweets = pd.read_json(historicTweets_json) #turn into pd.dataframe
	# historicTweets = pd.concat([historicTweets,todaysTweets[date]],axis=1) #concatenate
	# historicTweets_json = historicTweets.to_json(orient='records') #convert back to json
	# with open('Data/todaysTweets_all.json','w') as fp: #save
		# json.dump(historicTweets_json,fp)