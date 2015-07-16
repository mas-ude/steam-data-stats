df.priced <- read.table(file="steamdata.csv")

### clustering
library(devtools)
install_github("sinhrks/ggfortify")
library(ggfortify)
df <- iris[c(1,2,3,4)]
autoplot(prcomp(df))
autoplot(kmeans(df.priced[c(9, 13)], 3), data=df.priced) + scale_x_log10() + scale_y_log10()

# price and average_forever playtime
cl <- kmeans(df.priced[c(13,9)], 3)
df.priced$cluster = factor(cl$cluster)
centers=as.data.frame(cl$centers)
ggplot(df.priced, aes(x=price, y=average_forever, color=cluster)) +geom_point() + scale_y_log10() + scale_x_log10()

# price and players_forever
cl <- kmeans(df.priced[c(13,5)], 3, nstart=100)
df.priced$cluster = factor(cl$cluster)
centers=as.data.frame(cl$centers)
ggplot(df.priced, aes(x=price, y=players_forever, color=cluster)) +geom_point() + scale_y_log10() + scale_x_log10()
last_plot() + geom_point(data=centers, aes(x=price,y=players_forever, color='Center')) + geom_point(data=centers, aes(x=price,y=players_forever, color='Center'), size=52, alpha=.3)


# clustering with pam
library(cluster)
library(fpc)
# find best number of clusters
cl <- pamk(df.priced[c(13,5)], krange=3:4, criterion="multiasw", ns=10, usepam=TRUE, diss=FALSE, critout = TRUE)
#cl <- pam(df.priced[c(13,5)], 3)
df.priced$cluster = factor(cl$pamobject$clustering)
ggplot(df.priced, aes(x=price, y=players_forever, color=cluster)) +geom_point() + scale_y_log10() + scale_x_log10()

df.priced$cluster <- NULL
cl <- pamk(df.priced[c(-1,-2)], krange=1:6, criterion="multiasw", ns=10, usepam=TRUE, diss=FALSE, critout = TRUE)
#cl <- pam(df.priced[c(13,5)], 3)
df.priced$cluster = factor(cl$pamobject$clustering)
ggplot(df.priced, aes(x=price, y=players_forever, color=cluster, size=average_forever)) +geom_point() + scale_y_log10() + scale_x_log10()
