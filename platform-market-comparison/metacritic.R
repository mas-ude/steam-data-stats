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

## merge with gamelengths.com dataset
# optional, as it contains only very entries 
df.lengths <- read.csv("data/gamelengths.csv", sep = ";", colClasses = c("character", "numeric", "numeric", "character"))

## average the lengths over each occurence on the individual platforms and count the reports
tmp1 <- aggregate(average_length ~ title, data = df.lengths,FUN="mean")
tmp2 <- aggregate(times_reported ~ title, data = df.lengths,FUN="sum")
df.lengths.cross <- merge(tmp1, tmp2, by = "title")


df.merged2 <- merge(df.merged, df.lengths.cross, by.x = "Title", by.y = "title", all.x = TRUE)


## merge with howlongtobeat.com dataset

## PRE-MERGE-TODO: unify category names across datasets and merge by title AND category
## PRE-MERGE-TODO: remove outliers, e.g. runescape @ 100000h

df.hltb <- read.csv("data/howlongtobeat.csv", sep = ";", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "character"))

df.merged2 <- merge(df.metacritic, df.hltb, by ="title", all.x = TRUE, all.y = TRUE)

summary(df.hltb)

tmp <- df.hltb[order(-df.hltb$combined_length),]

ggplot(df.hltb, aes(x = combined_length)) + stat_ecdf() + scale_x_log10()
ggplot(df.hltb, aes(x = combined_length)) + stat_density() + scale_x_log10() # + xlim(0, 100)


####  combine some stuff with steam datasets
## TODO: consider using adist() for fuzzy string matching the titles
## http://www.r-bloggers.com/fuzzy-string-matching-a-survival-skill-to-tackle-unstructured-information/
string.matches <- adist(subset(df.priced, date = "20151015")$name, subset(df.metacritic, platform == "pc")$title, ignore.case = TRUE)

  
df.steammetascore <- merge(df.priced, subset(df.metacritic, platform == "pc"), by.x = "name", by.y = "title", all.x = TRUE)
df.steammetascorehltb <- merge(df.steammetascore, subset(df.hltb, platform == "PC"), by.x = "name", by.y = "title", all.x = TRUE)

# hist of steam games metacritic score
ggplot(df.steammetascorehltb, aes(x = score)) + geom_histogram(binwidth = 1)
ggplot(df.steammetascorehltb, aes(x = score)) + stat_ecdf()

