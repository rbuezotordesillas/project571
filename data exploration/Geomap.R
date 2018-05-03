%Localization

library(gdata)
#accountsLOC <- read.xls('~/project571//data collection/Data/ModelData/accountsTODO.xlsx')
#import accountsTODO
accountsLOC <- data.frame(accountsTODO, stringsAsFactors = FALSE)
#accounts <- accounts[-3]
#accounts <- data.frame(accounts, accountsLOC)
#accounts <- accounts[-c(3,4,5)]
#colnames(accounts) <- c("Account", "Category", "City", "State", "Country")
unique(accountsLOC$city)
which(accountsLOC$city == "Brussels,")
accountsLOC[134,"city"] <- "Brussels"
table(accountsLOC$city)
table(accountsLOC$country)

colnames(accountsLOC) <- c("#", "Account", "Category", "City", "State", "Country")

cities <- data.frame(accountsLOC$City, stringsAsFactors = FALSE)

cities <- cities[complete.cases(cities),]

#cities <- cities[!apply(cities == "", 1, all),]
citiesN <- sort(table(cities))
namecitiesN <- names(citiesN)
numcitiesN <- as.vector(citiesN)
newcities <- data.frame(namecitiesN, numcitiesN, stringsAsFactors = FALSE)

#install.packages("ggmap")
#install.packages("maptools")
#install.packages("maps")
library("ggmap")
library("maptools")
library("maps")
library(ggmap)
library(rworldmap)
complete.cases(cities)
lonlat <- geocode(newcities$namecitiesN)
lonlat
lonlat[which(complete.cases(lonlat)==FALSE),] <- geocode(cities[which(complete.cases(lonlat)==FALSE)])
lonlat

mapcities <- data.frame(newcities, lonlat, stringsAsFactors = FALSE)
colnames(mapcities) <- c("city", "num", "lon", "lat")

write.csv(mapcities, "newmapcities.csv")

#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=mapcities$lon, y=mapcities$lat) ,color="red", size=0.12*mapcities$num) 
mp