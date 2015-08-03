library(ggplot2)

budget <- seq(0, 1000, 50)

## ps now streaming
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
