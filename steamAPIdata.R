library(jsonlite)
library(ggplot2)

## get and package steamspy data
steamspy.data <- fromJSON("http://steamspy.com/api.php?request=all")
d <- data.frame()

for (i in steamspy.data) {
  tmp <- data.frame(appid=i$appid, name=i$name, owners=i$owners, owners_variance=i$owners_variance, players_forever=i$players_forever, players_forever_variance=i$players_forever_variance, players_2weeks=i$players_2weeks, players_2weeks_variance=i$players_2weeks_variance, average_forever=i$average_forever, average_2weeks=i$average_2weeks, median_forever=i$median_forever, median_2weeks=i$median_2weeks)
  d <- rbind(d, tmp)  
}

## sort data by players, descending
d <- d[with(d, order(-players_forever)), ]

## get current price data from steam 
# (based on current IP: final (after sale) prices in EU region 1, in Euro)
df.priced <- data.frame()
for (i in 1:nrow(d)) {
  Sys.sleep(2) # steam storefront API is throttled to ~200requests/5mins
  row <- d[i,]
  print(paste("http://store.steampowered.com/api/appdetails/?appids=",row["appid"], sep=""))
  tmp <- fromJSON(paste("http://store.steampowered.com/api/appdetails/?appids=",row["appid"], sep=""))
  
  price <- tmp[[paste(row[["appid"]])]]$data$price_overview$final
  print(price)
  
  if(!is.null(price)) {
    row$price <- price
  } else { # TODO: additionally check for is.free here!
    row$price <- 0
  }
  
  tmp <- data.frame(row)
  df.priced <- rbind(df.priced, tmp)
}

write.table(df.priced, file="steamdata.csv")
write.csv(df.priced, file="steamdata.csv", row.names=FALSE)
