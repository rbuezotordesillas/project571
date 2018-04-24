USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///accounts.csv" AS row
CREATE (:User {accountName: row.Account, category: row.Category});

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Hashtags.csv" AS row
CREATE (:Hashtags {HT: row.Hashtags});

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Links.csv" AS row
CREATE (:Links {url: row.Links});

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Mentions.csv" AS row
CREATE (:MentionedUser {name: row.Mentions});

CREATE INDEX ON :Tweet(id);
CREATE INDEX ON :User(accountName);
CREATE INDEX ON :Hashtag(HT);
CREATE INDEX ON :Link(url);

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Hashtags.csv" AS row
MATCH (u:User {accountName: row.User})
MATCH (h:Hashtag {HT: row.Hashtags})
MERGE (u)-[:TAGGED]->(h);

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Links.csv" AS row
MATCH (u:User {accountName: row.User})
MATCH (l:Link {url: row.Links})
MERGE (u)-[:LINKED]->(l);

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Mentions.csv" AS row
MATCH (u:User {accountName: row.User})
MATCH (uMentioned:User {accountName: row.Mentions})
MERGE (u)-[:MENTIONED]->(uMentioned);