library(ggplot2)
library(plyr)

### load and analyse metacritic data
# Note: file not provided here
# scrape the date with https://github.com/mas-ude/metacritic_scraper
# and place it in the data folder


df.metacritic <- read.csv("data/metacritic.csv", header=TRUE, sep=";", colClasses=c("numeric", "character", "character", "character", "numeric", "character", "character"))
df.metacritic$platform <- as.factor(df.metacritic$platform)
df.metacritic$release <- as.Date(df.metacritic$release, format = "%B %d, %Y")
ggplot(df.metacritic, aes(x=score, color=platform)) + stat_ecdf()
ggplot(df.metacritic, aes(x=score, color=platform)) + stat_density(position = "dodge", fill=NA, lwd=1)


## quick plot for releases per year
#df.metacritic$month = strftime(df.metacritic$release, "%b")
df.metacritic$year = strftime(df.metacritic$release, "%Y")
releases.per-year = count(df.metacritic, vars = c("year", "platform"))
ggplot(releases.per-year, aes(x=year, y=freq, fill=platform)) + geom_bar(stat="identity", position = "stack")
ggsave("releases-per-year.pdf")
