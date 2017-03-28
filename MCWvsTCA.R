library(ggplot2)

stats = read.csv('LL72_Leaguewide_MD25.csv')
sum = aggregate(Season ~ TCA + MCW, stats, length)
colnames(sum) <- c("TCA", "MCW", "Frequency")

png('MCWvTCA.png', w=1000, h=500)
ggplot(sum, aes(x=TCA, y=MCW, size=Frequency)) + geom_point()
dev.off()
