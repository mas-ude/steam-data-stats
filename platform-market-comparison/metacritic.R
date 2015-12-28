library(ggplot2)
library(plyr)

### load and analyse metacritic data
# Note: file not provided here
# scrape the data with https://github.com/mas-ude/metacritic_scraper
# and place it in the data folder

# setwd("git/steam-data-stats/platform-market-comparison/")

df.metacritic <- read.csv("data/metacritic-20151227.csv", header=TRUE, sep=";", colClasses=c("numeric", "character", "character", "character", "numeric", "character", "character"))
df.metacritic$platform <- as.factor(df.metacritic$platform)
df.metacritic$release <- as.Date(df.metacritic$release, format = "%B %d, %Y")
ggplot(df.metacritic, aes(x=score, color=platform)) + stat_ecdf()
ggplot(df.metacritic, aes(x=score, color=platform)) + stat_density(position = "dodge", fill=NA, lwd=1)


## quick plot for releases per year
#df.metacritic$month = strftime(df.metacritic$release, "%b")
df.metacritic$year = strftime(df.metacritic$release, "%Y")
releases.peryear = count(df.metacritic, vars = c("year", "platform"))
ggplot(releases.peryear, aes(x=year, y=freq, fill=platform)) + geom_bar(stat="identity", position = "stack")
ggsave("releases-per-year.pdf")


##############
df.psnow <- read.csv("data/psnow-games.csv", sep = ";", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "logical"))
df.metacritic.ps3 <- subset(df.metacritic, platform == "ps3")
df.merged <- merge(df.psnow, df.metacritic.ps3, by.x = "Title", by.y = "title", all.x = TRUE)

## scores
mean(df.merged$score, na.rm = TRUE)

## age
df.merged$year <- as.numeric(strftime(df.merged$release, "%Y"))
df.merged$age <- 2015 - df.merged$year

hist(df.merged$age)
mean(df.merged$age, na.rm = TRUE)
median(df.merged$age, na.rm = TRUE)


df.lengths <- read.csv("data/gamelengths.csv", sep = ";", colClasses = c("character", "numeric", "numeric", "character"))

## average the lengths over each occurence on the individual platforms and count the reports
tmp1 <- aggregate(average_length ~ title, data = df.lengths,FUN="mean")
tmp2 <- aggregate(times_reported ~ title, data = df.lengths,FUN="sum")
df.lengths.cross <- merge(tmp1, tmp2, by = "title")


df.merged2 <- merge(df.merged, df.lengths.cross, by.x = "Title", by.y = "title", all.x = TRUE)

