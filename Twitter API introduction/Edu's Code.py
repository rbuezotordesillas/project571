# -*- coding: utf-8 -*-
"""
Created on Wed Aug 23 23:15:03 2017

@author: Eduardo
"""

from TwitterAPI import TwitterAPI
import time

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

def get_followers(screen_name,count=200):       #should modify this to behave as get_friends()
    '''
    returns the followers of a user as a list.
    input: users screen_name as string
    count as default = max(200)
    '''
    request = twitter.request('followers/list', {'screen_name':screen_name, 'count':count})
    followers_list=request.json()['users']
    return followers_list

def get_NumberOfFriends(screen_name):
    '''
    returns the number of friends the user has 
    '''
    request=twitter.request('users/lookup',{'screen_name':screen_name})
    user=[u for u in request][0]
    return user['friends_count']

def get_friends(screen_name,count=200):
    '''
    returns the friends of a user as a list.
    input: users screen_name as string
    count as default = max(200)
    '''
    friends_list=[]
    time_errors=[88,429]
    friends=get_NumberOfFriends(screen_name)
    
    cursor=-1   #default value, first page
    for i in range(friends//count+1):   #friends/count rounded up
        try: 
            request = twitter.request('friends/list', {'screen_name':screen_name, 'count':count, 'cursor':cursor})
            cursor=request.json()['next_cursor']
            friends_list+=request.json()['users']
        except Exception as e:
            error_code=request.json()['errors'][0]['code']
            if error_code in time_errors:
                print('execution suspended for 15 mins')
                print('progress: '+str(len(friends_list))+' friends retrieved out of '+str(friends))
                time.sleep(15*60+1)
                print('execution reactivated')
                
            else:
                print(error_code)
                print(request.json()['errors'][0]['message'])
                break
            
    return friends_list

def get_timeline(screen_name,count=200): #should modify this to be more robust
    '''
    retuns a list with the last (count-deleted) tweets from the user
    '''
    request = twitter.request('statuses/user_timeline',{'screen_name': screen_name,'count': count})
    return request.json()

twitter = get_twitter()
print('Established Twitter connection.')


#==============================================================================
# my_id=413444245
# my_screen_name='Edballest'
# 
#
'''both of this work'''
# request = twitter.request('followers/list', {'screen_name':my_screen_name, 'count':200})
# request = twitter.request('followers/list', {'id':my_id, 'count':200})
#==============================================================================


#realDonaldTrump
#HillaryClinton




