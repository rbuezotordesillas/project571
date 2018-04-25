# PRESENTATION QUERIES

# Change config for node displays

:config initialNodeDisplay: 1000

# Create indexes

CREATE INDEX ON :Tweet(id);
CREATE INDEX ON :User(accountName);
CREATE INDEX ON :Mentioned(accountName);
CREATE INDEX ON :Hashtag(HT);
CREATE INDEX ON :Link(url);


# Load data 

WITH "file:///Users/Raquel/Desktop/tweetJSON/byCategory/News.json" AS url
CALL apoc.load.json(url) YIELD value as t
MERGE (tweet:Tweet {id:t.id})
MERGE (user:User {accountName:t.user.screen_name, category: ____})
MERGE (user)-[:POSTS]->(tweet)
FOREACH (h IN t.entities.hashtags |
	MERGE (tag:Hashtag {HT: LOWER(h.text)})
    MERGE (tag)-[:TAGS]->(tweet))
FOREACH (l IN t.entities.urls |
	MERGE (link:Link {url: LOWER(l.display_url)})
    MERGE (tweet)-[:CONTAINS]->(link))
FOREACH (m IN t.entities.user_mentions |
	MERGE (mentionedUser:Mentioned {accountName: (m.screen_name)})
    MERGE (tweet)-[:MENTIONS]->(mentionedUser))
	
	

# Graph ALL

MATCH (t:Tweet)
WITH t ORDER BY t.id DESC LIMIT 2000	 # Can be taken off, but I get different results?
MATCH r1= (user:User)-[:POSTS]->(t)
MATCH r2= (t)<-[:TAGS]-(tag:Hashtag)
MATCH r3= (t)-[:MENTIONS]->(user2:Mentioned)  
RETURN r1,r2,r3


# Graph by categories with POSTS, MENTIONS, and TAGS relationships

MATCH (t:Tweet)
MATCH r1= (user:User)-[:POSTS]->(t) WHERE user.category='__'
MATCH r2= (t)<-[:TAGS]-(tag:Hashtag)
MATCH r3= (t)-[:MENTIONS]->(user2:Mentioned)  
RETURN r1,r2,r3


MATCH (t:Tweet)
MATCH r1=(user:User)-[:POSTS]->(t)<-[:TAGS]-(tag:Hashtag) WHERE user.accountName IN ['Newsweek','CNN', 'BBCWorld', 'NBCNews','nytimes','ABC','FoxNews', 'washingtonpost','WSJ', 'Reuters', 'el_pais', 'abc_es', 'antena3com', 'noticias_cuatro', '24h_tve']
MATCH r2=(t)-[:MENTIONS]->(user2:Mentioned)  
RETURN r1,r2


## Hashtag exploration analysis

# Most used hashtags and their count

MATCH ()<-[t:TAGS]-(h:Hashtag)
RETURN h.HT as HASHTAG,count(t) as COUNT
ORDER BY COUNT DESC


# See who used this hashtag

MATCH (u:User)-[p:POSTS]->()<-[:TAGS]-(h:Hashtag{HT:'metoo'})
RETURN u.accountName AS Account, count(p) AS Count
ORDER BY Count DESC

# by category

MATCH (u:User{category:'Fashion'})-[:POSTS]->()<-[t:TAGS]-(h:Hashtag)
RETURN h.HT as HASHTAG,count(t) as COUNT
ORDER BY COUNT DESC

# graph some: do for metoo, westworld, earthday, diadelatierra or díadelatierra

MATCH p=()-[:POSTS]->()<-[:TAGS]-(h:Hashtag{HT:'metoo'})
RETURN p

MATCH p=()-[:POSTS]->()<-[:TAGS]-(h:Hashtag) WHERE h.HT IN ['diadelatierra','díadelatierra']
RETURN p


## Mentioned Users analysis

# Most mentioned
MATCH ()-[m:MENTIONS]->(mu:Mentioned)
RETURN mu.accountName as Mentioned_User,count(m) as COUNT
ORDER BY COUNT DESC

# graph most mentionedUser
MATCH p= ()-[:POSTS]->()-[m:MENTIONS]->(mu:Mentioned{accountName:'MasterChef_es'})
RETURN p


