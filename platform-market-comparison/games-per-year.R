library(ggplot2)




# all cost calculated for region 1 EU in Euros; cost converted from other regions (UK/US) with current exchange rates plus taxes

# basis a yearly budget 
budget <- seq(0, 1000, 50)

## update to current exchange rates here
exchangerate.GBPtoEUR <- 1.41160
exchangerate.USDtoEUR <- 1.09205

## factor in additional cost factors
# consider broadband Internet access costs (might not be available in rural areas)
broadband.enabled <- FALSE

#### ps now streaming
# US, CA, UK only; using US as data source, as info on UK is insufficient:
# https://en.wikipedia.org/wiki/List_of_PlayStation_Now_games
# http://www.ign.com/wikis/playstation-4/List_of_PlayStation_Now_Games

## monthly cost 12.99 pounds
psnow.monthly <- 12.99 * exchangerate.GBPtoEUR

## specific hardware requirements
# either a ps4, ps3, or specific Sony TV models (plus controller)
# going with the ps4 option, as its the most common and includes a controller
psnow.hw <- 329 # as of 2015/11/04

## size of included catalog
psnow.games <- read.csv("data/psnow-games.csv", header = TRUE, sep = ";", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "logical"))

psnow.subscription.numgames <- length(psnow.games$Included.In.Subscription[psnow.games$Included.In.Subscription == TRUE])
  
  
## cost of renting additional games
psnow.rental.price.48h <- 2.99 * exchangerate.GBPtoEUR
psnow.rental.price.30d <- 7.99 * exchangerate.GBPtoEUR
psnow.rental.numgames <- nrow(psnow.games) - psnow.subscription.numgames
  



# CAPEX: 150 (device+controler over 10 years) 150/10
# OPEX: subscription + rental cost (assumed 7 days rental for 1/day) + 20*12; 7 per title
# plus assumed 100 titles in subscription service included
psnow <- ((budget-15)-240)
psnow[psnow>0] <- psnow[psnow>0]/15+134
psnow[psnow <0] <- 0


## gamefly
# CAPEX: firetv + controller: 150 (over 10 years): 150/10
# OPEX: 6 packets, 7 games each, 7$ per packet
gamefly <- ifelse((budget)-15 > (7*6*12), 6*7,
            ifelse((budget)-15 > (7*5*12), 5*7,
                   ifelse((budget)-15 > (7*4*12), 4*7,
                          ifelse((budget)-15 > (7*3*12), 3*7,
                                 ifelse((budget)-15 > (7*2*12), 2*7,
                                        ifelse((budget)-15 > (7*1*12), 1*7, 0))))))

## console
# CAPEX: 400 over 5 years: 400/5
# OPEX: 50 per title
console <- pmax((budget-80),0)/50

## pc (steam)
# CAPEX: 500 over 3 years: 500/3 (midrange pc)
# OPEX: variant #1 buying AAA titles during steam sales: 15-20
# variant #2 mean price of all steam titles (outside of sales period): 10.10€ (EU region 1)
pc <- pmax((budget-167),0)/10.1

df <- data.frame(budget = budget, gamesperyear = psnow, platform = "ps now")

tmp <- data.frame(budget=budget, gamesperyear = gamefly, platform = "gamefly")
df <- rbind(df, tmp)
tmp <- data.frame(budget=budget, gamesperyear = console, platform = "consoles")
df <- rbind(df, tmp)
tmp <- data.frame(budget = budget, gamesperyear = pc, platform = "pc")
df <- rbind(df, tmp)

ggplot(df, aes(x=budget, y=gamesperyear, color=platform)) + geom_line() + geom_point(size=2)


## over a 10 year period at a fixed budget
money <- 500
year <- 1:10
budget.annual <- year*money

pc.annual <- ((year*money) - 167*year)/10.1
console.annual <- pmax((year*money-80*year),0)/50

psnow.annual <- (year*money)-(15+240)*year
psnow.annual[psnow.annual>0] <- psnow.annual[psnow.annual>0]/15+134
psnow.annual[psnow.annual <0] <- 0

gamefly.annual <- ifelse((year*money - 15*year) > (6*7*12*year), 6*7,
                  ifelse((year*money - 15*year) > (7*5*12*year), 5*7,
                         ifelse((year*money - 15*year) > (7*4*12*year), 4*7,
                                ifelse((year*money - 15*year) > (7*3*12*year), 3*7,
                                       ifelse((year*money - 15*year) > (7*2*12*year), 2*7,
                                              ifelse((year*money - 15*year) > (7*1*12*year), 1*7, 0))))))

df <- data.frame(year=year, games = pc.annual, platform = "pc")

tmp <- data.frame(year=year, games = console.annual, platform = "consoles")
df <- rbind(df, tmp)
tmp <- data.frame(year=year, games = psnow.annual, platform = "ps now")
df <- rbind(df, tmp)
tmp <- data.frame(year=year, games = gamefly.annual, platform = "gamefly")
df <- rbind(df, tmp)

ggplot(df, aes(x=year, y=games, color=platform)) + geom_line() + geom_point(size=2)
