
# Create Tweet nodes
USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Hashtags.csv" AS row
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


