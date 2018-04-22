USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///accounts.csv" AS row
CREATE (:User {accountName: row.Account, category: row.Category});

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Hashtags.csv" AS row
CREATE (:Hashtags {HT: row.Hashtags});

USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM "file:///Links.csv" AS row
CREATE (:Links {url: row.Links});



