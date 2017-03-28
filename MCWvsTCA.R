library(ggplot2)
library(plyr)

neighbor_limit = 5

stats = read.csv('LL72_Leaguewide_MD25.csv')
sum = aggregate(Season ~ TCA + MCW, stats, length)
sum = adply(sum, 1, function(x) { sum(sum[sum$MCW==x$MCW & sum$TCA > x$TCA-neighbor_limit & sum$TCA < x$TCA+neighbor_limit,]$Season) })
colnames(sum) <- c("TCA", "MCW", "Frequency", "Neighbors")

stats = merge(stats, sum)

png('MCWvTCA.png', w=2000, h=1000)
ggplot(stats, aes(x=TCA, y=MCW, size=Frequency)) +
    geom_point() +
    geom_abline(intercept=25*6,slope=-1) +
    geom_smooth() +
    geom_text(aes(label=Player), stats[stats$Frequency==1 & stats$Neighbors==1,], nudge_y=0.333, size=4)
dev.off()
