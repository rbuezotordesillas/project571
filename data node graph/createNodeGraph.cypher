WITH "file:///Users/Raquel/Desktop/tweetJSON/BBC_Travel.json" AS url
CALL apoc.load.json(url) YIELD value as t
MERGE (tweet:Tweet {id:t.id})
MERGE (user:User {accountName:t.user.screen_name})
MERGE (user)-[:POSTS]->(tweet)
FOREACH (h IN t.entities.hashtags |
	MERGE (tag:Hashtag {ht: LOWER(h.text)})
    MERGE (tag)-[:TAGS]->(tweet))
FOREACH (l IN t.entities.urls |
	MERGE (link:Links {url: LOWER(l.display_url)})
    MERGE (tweet)-[:CONTAINS]->(link))
FOREACH (m IN t.entities.user_mentions |
	MERGE (mentionedUser:User {accountName: (m.screen_name)})
    MERGE (tweet)-[:MENTIONS]->(mentionedUser))




head = []
with open("result.json", "w") as outfile:
    for f in file_list:
        with open(f, 'rb') as infile:
            file_data = json.load(infile)
            head += file_data
    json.dump(head, outfile)


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


