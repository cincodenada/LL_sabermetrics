library(ggplot2)

stats = read.csv('LL72_Leaguewide_MD25.csv')
sum = aggregate(Season ~ TCA + MCW, stats, length)
colnames(sum) <- c("TCA", "MCW", "Frequency")

stats = merge(stats, sum)

png('MCWvTCA.png', w=1000, h=500)
ggplot(stats, aes(x=TCA, y=MCW, size=Frequency)) +
    geom_point() +
    geom_abline(intercept=25*6,slope=-1)
dev.off()
