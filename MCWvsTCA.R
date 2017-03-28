library(ggplot2)
library(plyr)

neighbor_limit = 5

stats = read.csv('LL72_Leaguewide_MD25.csv')
sum = aggregate(Season ~ TCA + MCW, stats, length)
# This is pretty ugly, but does a decent job of highlighting interesting bits
# It just finds anyone who doesn't have close neighbors along the TCA axis (that is, people with a similar number of TCA who got the same number of MCWAs)
sum = adply(sum, 1, function(x) { sum(sum[sum$MCW==x$MCW & sum$TCA > x$TCA-neighbor_limit & sum$TCA < x$TCA+neighbor_limit,]$Season) })
colnames(sum) <- c("TCA", "MCW", "Frequency", "Neighbors")

stats = merge(stats, sum)

p = ggplot(stats, aes(x=TCA, y=MCW, size=Frequency)) +
    geom_point(shape=19) +
    geom_abline(intercept=25*6,slope=-1) +
    geom_smooth(show.legend=F) +
    geom_label(aes(label=Player), alpha=0.65, stats[stats$Frequency==1 & stats$Neighbors==1,], hjust=0, vjust=0.5, size=3) +
    scale_x_continuous(breaks=seq(0,150,10), minor_breaks = seq(0,150), limits=c(-2,152), expand=c(0,0)) +
    scale_y_continuous(breaks=seq(0,40,10), minor_breaks = seq(0,40), limits=c(-2,42), expand=c(0,0)) +
    coord_fixed() +
    labs(title="Number of MCWAs vs TCA in LL72") +
    theme(legend.position="bottom")

png('MCWvTCA.png', w=2560, h=900, type="cairo", res=96)
p
dev.off()
png('MCWvTCA_small.png', w=600, h=250, type="cairo",res=48)
p
dev.off()
