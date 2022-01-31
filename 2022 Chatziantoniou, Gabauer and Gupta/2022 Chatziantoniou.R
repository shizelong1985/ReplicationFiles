
### Integration and Risk Transmission in the Market for Crude Oil: A Time-Varying Parameter Frequency Connectedness Approach
### CHATZIANTONIOU, I., GABAUER, D. & GUPTA, R.
### replicated by David Gabauer

library("zoo")
library("openxlsx")
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("cgg2022")

# DATA TRANSFORMATION
Y = cgg2022
NAMES = c("Europe","United States","Dubai","Malaysia","Africa","OPEC")
colnames(Y) = NAMES
k = ncol(Y)

par(mfrow = c(ceiling(k/2), 2), oma = c(1,1,0,0) + 0.1, mar = c(1,0.5,0.5,0) + 1, mgp=c(0, .65, 0))
for (i in 1:k) {
  plot(Y[,i],type="l",las=1,xlab="",ylab="",main=NAMES[i],ylim=c(0,30),col="steelblue4",xaxs="i", yaxs="i")
  grid(NA,NULL)
  lines(Y[,i],col="steelblue4")
  box()
}
print(SummaryStatistics(Y))

# DYNAMIC RETURN CONNECTEDNESS
partition = c(pi+0.00001, pi/5, 0)
dca = ConnectednessApproach(Y, 
                            model="TVP-VAR",
                            connectedness="Frequency",
                            nlag=1,
                            nfore=100,
                            window.size=200,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior")),
                            Connectedness_config = list(
                              FrequencyConnectedness=list(partition=partition, generalized=TRUE)
                            ))

# CONNECTEDNESS PLOTS
date = index(Y)
par(mfrow=c(1,1))
plot(date, as.numeric(dca$TCI[,1,1]),type="l",las=1,ylim=c(0,80),col="blue",xlab="",ylab="")
lines(date, as.numeric(dca$TCI[,2,1]),type="l")

par(mfrow=c(3,2))
for (i in 1:k) {
  plot(date, as.numeric(dca$NET[,i,1,1]),type="l",las=1,col="blue",xlab="",ylab="",main=NAMES[i]); abline(h=0)
  lines(date, as.numeric(dca$NET[,i,2,1]),col="red")
}
