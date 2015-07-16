df.priced <- read.table(file="steamdata.csv")


ggplot(d, aes(x=owners)) + stat_ecdf() + scale_x_log10()
ggplot(d, aes(x=average_forever)) + stat_ecdf() + scale_x_log10()
ggplot(d, aes(x=appid, y=owners)) + geom_point() + scale_y_log10() + xlim(0,400000)
ggplot(d, aes(x=appid, y=average_forever)) + geom_point() + scale_y_log10() + xlim(0,400000)
ggplot(d, aes(x=appid, y=average_2weeks)) + geom_point() + scale_y_log10() + xlim(0,400000)

ggplot(d, aes(x=players_forever, y=average_forever)) + geom_point() + scale_x_log10() + scale_y_log10()
ggplot(d, aes(x=players_2weeks, y=average_2weeks)) + geom_point() + scale_x_log10() + scale_y_log10()


### price data

ggplot(df.priced, aes(x=price, y=average_2weeks)) + geom_point() + xlim(0,10000) + scale_y_log10()
ggplot(df.priced, aes(x=price, y=average_forever)) + geom_point() + xlim(0,10000) + scale_y_log10()
ggplot(df.priced, aes(x=price, y=average_forever, size=players_forever, color=players_forever)) + geom_point() + scale_x_log10() + scale_y_log10() 

ggplot(df.priced, aes(x=price, y=players_forever)) + geom_point() + xlim(0,10000) + scale_y_log10()
ggplot(df.priced, aes(x=price, y=players_forever)) + geom_point() + xlim(0,10000) + scale_y_log10() + scale_x_log10()

prices <- as.data.frame(table(df.priced$price))
names(prices) <- c("price", "frequency")
ggplot(prices, aes(x=price, y=frequency)) + geom_point()
ggplot(df.priced, aes(x=price)) + stat_ecdf() + scale_x_log10()

ggplot(df.priced, aes(x=price, y=average_2weeks)) + geom_point() + xlim(0,10000) + scale_y_log10()

ggplot(df.priced, aes(x=price, y=players_forever)) + geom_point() + xlim(0,10000) + scale_y_log10()
ggplot(df.priced, aes(x=price, y=players_2weeks)) + geom_point() + xlim(0,10000) + scale_y_log10()



### heatmap
# be careful, the zero values are changed to 1 here, to allow for log scaling!
df.priced[df.priced["players_forever"]==0,"players_forever"] = 1
df.priced[df.priced["price"]==0,"price"] = 1
ggplot(df.priced, aes(x=price, y=players_forever)) + geom_bin2d() + scale_x_log10() + scale_y_log10()


### custom binning and setting levels
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




d1 <- subset(df.priced, price==0  | (price>1000))

graph <- ggplot(d1, aes(x=average_forever, color=as.factor(sign(price)))) 
graph + stat_ecdf() + scale_x_log10()

