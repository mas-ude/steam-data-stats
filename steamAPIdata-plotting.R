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
df.priced2 <- data.frame()
for (i in 1:nrow(df.priced)) {
  row <- df.priced[i,]
  
  if(row$price == 0) {
    row$price_category <- "free"
  } else if (row$price > 0 & row$price <= 500){
    row$price_category <- "<5"
  } else if (row$price > 500 & row$price <= 1000) {
    row$price_category <- "5 to 10"
  } else if (row$price > 1000 & row$price <= 2000) {
    row$price_category <- "10 to 20"
  } else if (row$price > 2000 & row$price <= 4000) {
    row$price_category <- "20 to 40"
  } else if (row$price > 4000) {
    row$price_category <- "above 40"
  }  
  tmp <- data.frame(row)
  df.priced2 <- rbind(df.priced2, tmp)
}
df.priced2$price_category <- as.factor(df.priced2$price_category)
df.priced2$price_category <- ordered(df.priced2$price_category, levels=c("free", "<5", "5 to 10", "10 to 20", "20 to 40", "above 40"))
ggplot(df.priced2, aes(x=price_category, y=players_forever)) + geom_violin() + scale_y_log10()
ggsave("dampfviolinen-players.pdf")
ggplot(df.priced2, aes(x=price_category, y=average_forever)) + geom_violin() + scale_y_log10()
ggsave("dampfviolinen-playtime.pdf")

ggplot(df.priced2, aes(x=price_category, y=average_forever)) + geom_boxplot() + scale_y_log10()



d1 <- subset(df.priced, price==0  | (price>1000))
graph <- ggplot(d1, aes(x=average_forever, color=as.factor(sign(price)))) 
graph + stat_ecdf() + scale_x_log10()


