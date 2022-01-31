
### Quantile time-frequency price connectedness between green bond, green equity, sustainable investments and clean energy markets: Implications for eco-friendly investors
### GABAUER, D., CHATZIANTONIOU, I., Aikins Abakah, E.J. & TIWARI, A.K.
### replicated by David Gabauer

library("zoo")
library("openxlsx")
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())

data("gcat2022")
DATA = gcat2022
NAMES = c("Green Bond Index", 
          "MSCI Global Environment",
          "Sustainability Index World",
          "Global Clean Energy")
colnames(DATA) = NAMES
k = ncol(DATA)

par(mfrow = c(1,1), oma = c(0.5,1,0,0) + 0.1, mar = c(0.5,1,1,0) + .5, mgp = c(3, 0.5, 0))
plot(scale(DATA[,1],T,T), type="l",las=1,xaxs="i",col="steelblue4",main="",tck=-0.01,ylim=c(-5,5))
grid(NA, NULL)
for (i in k:1) {
  lines(scale(DATA[,i],T,T), col=i, lty=i)
}
legend("topleft", NAMES, fill=1:k, bty="n")

Y = DATA[-1,]
for (i in 1:k) {
  Y[,i] = 100*diff(log(DATA[,i]))
}

split = 2
par(mfrow = c(ceiling(k/split),split), oma = c(0.5,1,0,0) + 0.1, mar = c(0.5,1,1,0) + .5, mgp = c(3, 0.5, 0))
for (i in 1:k) {
  plot(Y[,i], type="l",las=1,xaxs="i",col="steelblue4",main=NAMES[i],tck=-0.02,ylim=c(-11,11),yaxs="i")
  grid(NA, NULL)
  lines(Y[,i], col="steelblue4")
  abline(h=0, lty=3)
  box()
}
SummaryStatistics(Y)

# DYNAMIC RETURN CONNECTEDNESS
partition = c(pi+0.00001, pi/5, 0)
dca = ConnectednessApproach(Y, 
                            model="QVAR",
                            connectedness="Frequency",
                            nlag=1,
                            nfore=100,
                            window.size=200,
                            VAR_config=list(QVAR=list(tau=0.5)),
                            Connectedness_config=list(
                              FrequencyConnectedness=list(partition=partition, generalized=TRUE)
                            ))
print(dca$TABLE)

par(mfrow=c(1,1))
plot(dca$TCI[,1,1]+dca$TCI[,2,1], type="l",ylim=c(0,80),xaxs="i",las=1)
lines(dca$TCI[,1,1], col=2)
lines(dca$TCI[,2,1], col=3)

par(mfrow=c(2,2))
for (i in 1:k) {
  plot(dca$NET[,i,1,1], type="l",ylim=c(-20,20),xaxs="i",main=NAMES[i],las=1)
  abline(h=0, lty=3)
  lines(dca$NET[,i,2,1], col=2)
  lines(dca$NET[,i,1,1]+dca$NET[,i,2,1], col=3)
}
