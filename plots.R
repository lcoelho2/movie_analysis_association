#arquivo com os gráficos

if (!file.exists("plots")) {
  dir.create("plots")

# Plontando a resposta da questão 1 (ggsave funciona pro ggplot, mas nao pro corrplot)
ggplot(tagg, aes(x = timestamp, y = rating, color=genres)) + geom_point() + facet_wrap(~genres) + scale_x_discrete(breaks=seq(1995, 2016, 5)) + theme(legend.position="none")+theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave(filename="./plots/q1.png") 

# Plontando a resposta da questão 3
png("./plots/q3.png")
corrplot(m, p.mat = res1[[1]], order="FPC", insig = "pch",sig.level=0.001,tl.cex=0.7)
dev.off()

## plotando as regras 
png("./plots/q3r.png")
plot(rulesr[quality(rulesr)$lift > 1.0], 
     method="grouped",
     measure="confidence", 
     shading="lift",
     interactive=FALSE)
dev.off()

##plot do dendrogram

ggdendrogram(hc, rotate = FALSE, size = 2)
ggsave(filename="./plots/q3d.png") 

}