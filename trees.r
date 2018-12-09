#using Rpart PAckage

install.packages("rpart")
install.packages("party")
library(party)
library(rpart)

raw.orig <- read.csv(file="E:/R Projects/Trees//rsei212_chemical.txt", header=T, sep="\t")
raw = subset(raw.orig, select=c("Metal","OTW","AirDecay","Koc"))
row.names(raw) = raw.orig$CASNumber
raw = na.omit(raw);
frmla = Metal ~ OTW + AirDecay + Koc

#Metal: Core Metal (CM); Metal (M); Non-Metal (NM); Core Non-Metal (CNM)
fit = rpart(frmla, method="class", data=raw)

printcp(fit)

plotcp(fit)
summary(fit)

plot(fit, uniform=TRUE, main="Classification Tree for Chemicals")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
table(subset(raw, Koc>=190.5)$Metal)


#Using Party Package



(ct = ctree(frmla, data = raw))
plot(ct, main="Conditional Inference Tree")
table(predict(ct), raw$Metal)