
### EMU risk-synchronisation and financial fragility through the prism of dynamic connectedness.
### CHATZIANTONIOU, I. & GABAUER, D.
### THE QUARTERLY REVIEW OF ECONOMICS AND FINANCE
### replicated by David Gabauer

library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())

# DATA PREPARATION
data("cg2021")
DATA = cg2021
k = ncol(DATA)
NAMES = colnames(DATA)

# DATA TRANSFORMATION
Y = DATA[-1,]
for (i in 1:k) {
  Y[,i] = diff(DATA[,i])
}

par(mfcol = c(ceiling(k/2), 2), oma = c(1,1,0,0) + 0.1, mar = c(1,0.5,0.5,0) + 1, mgp=c(0, .65, 0))
for (i in 1:k) {
  plot(Y[,i],type="l",las=1,xlab="",ylab="",main=NAMES[i],ylim=c(-max(abs(Y[,i])),max(abs(Y[,i]))),col="steelblue4",xaxs="i")
  grid(NA,NULL)
  lines(Y[,i],col="steelblue4")
  box()
}
print(SummaryStatistics(Y))

# DYNAMIC RETURN CONNECTEDNESS
dca = ConnectednessApproach(Y, 
                            model="TVP-VAR",
                            connectedness="Time",
                            nlag=1,
                            nfore=10,
                            window.size=200,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior")))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca, save=TRUE)
plot_net(dca, save=TRUE)
plot_to(dca, save=TRUE)
plot_from(dca, save=TRUE)
plot_npso(dca, save=TRUE)
plot_pci(dca, save=TRUE)
