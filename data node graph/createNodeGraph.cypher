# Delete a node and all its relationships
MATCH (n)
DETACH DELETE n


WITH "file:///Users/Raquel/Desktop/tweetJSON/byCategory/News.json" AS url
CALL apoc.load.json(url) YIELD value as t
MERGE (tweet:Tweet {id:t.id})
MERGE (user:User {accountName:t.user.screen_name})
MERGE (user)-[:POSTS]->(tweet)
FOREACH (h IN t.entities.hashtags |
	MERGE (tag:Hashtag {HT: LOWER(h.text)})
    MERGE (tag)-[:TAGS]->(tweet))
FOREACH (l IN t.entities.urls |
	MERGE (link:Link {url: LOWER(l.display_url)})
    MERGE (tweet)-[:CONTAINS]->(link))
FOREACH (m IN t.entities.user_mentions |
	MERGE (mentionedUser:User {accountName: (m.screen_name)})
    MERGE (tweet)-[:MENTIONS]->(mentionedUser))


CREATE INDEX ON :Tweet(id);
CREATE INDEX ON :User(accountName);
CREATE INDEX ON :Hashtag(HT);
CREATE INDEX ON :Link(url);



# 1st i had to do something with dbms. ....

WITH "file:///Users/Raquel/Desktop/todaysTweets_2018-04-20.json" AS url
CALL apoc.load.json(url) YIELD value as tweets
RETURN tweets
LIMIT 10


# Create Tweet nodes
USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Hashtags.csv" AS row
MERGE (t:Tweet {id: row.Tweet_id})

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Links.csv" AS row
MERGE (t:Tweet {id: row.Tweet_id})

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Mentions.csv" AS row
MERGE (t:Tweet {id: row.Tweet_id})

# Create User nodes
CREATE CONSTRAINT ON (u:User) ASSERT u.accountName IS UNIQUE

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///accounts.csv" AS row
CREATE (:User {accountName: row.Account, category: row.Category})

# Create Link nodes
USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Links.csv" AS row
MERGE (l:Link {url: row.Links})

# Create relationship Tweet CONTAINS Link
USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Links.csv" AS row
MATCH (t:Tweet {id: row.Tweet_id})
MATCH (l:Link {url: row.Links})
MERGE (t)-[:CONTAINS]->(l);

# Create Hashtag nodes
USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Hashtags.csv" AS row
MERGE (h:Hashtag {HT: row.Hashtags})

# Create relationship Hashtag TAGS Tweet
USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Hashtags.csv" AS row
MATCH (t:Tweet {id: row.Tweet_id})
MATCH (h:Hashtag {HT: row.Hashtags})
MERGE (h)-[:TAGS]->(t);


# graphs

MATCH (t:Tweet)
WITH t ORDER BY t.id DESC LIMIT 2000
MATCH r1=(user:User)-[:POSTS]->(t)<-[:TAGS]-(tag:Hashtag) WHERE user.accountName IN ['CNN', 'BBCWorld', 'NBCNews','nytimes','ABC','FoxNews']
MATCH r2=(t)-[:MENTIONS]->(user2:User)  
RETURN r1,r2

MATCH (t:Tweet)
WITH t ORDER BY t.id DESC LIMIT 2000
MATCH r1=(user:User)-[:POSTS]->(t)<-[:TAGS]-(tag:Hashtag) WHERE user.accountName IN ['el_pais', 'abc_es', 'antena3com','noticias_cuatro','24h_tve']
MATCH r2=(t)-[:MENTIONS]->(user2:User)  
RETURN r1,r2

MATCH (t:Tweet)
WITH t ORDER BY t.id DESC LIMIT 2000
MATCH r1=(user:User)-[:POSTS]->(t)-[:CONTAINS]->(link:Link) WHERE user.accountName IN ['CNN', 'BBCWorld', 'NBCNews','nytimes','ABC','FoxNews']
MATCH r2=(t)-[:MENTIONS]->(user2:User)  
RETURN r1,r2