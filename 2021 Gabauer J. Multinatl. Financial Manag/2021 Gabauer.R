
### Dynamic measures of asymmetric & pairwise connectedness within an optimal currency area: Evidence from the ERM I system
### Gabauer, D.
### Journal of Multinational Financial Management
### replicated by David Gabauer

rm(list=ls())
library("zoo")
library("openxlsx")
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())

# DATA PREPARATION
RAW = read.xlsx("./data.xlsx", detectDates=TRUE)
RAW = na.omit(RAW)
DATE = as.Date(RAW[,1])
DATA = RAW[,-1]
DATA = zoo(DATA, order.by=DATE)
k = ncol(DATA)
NAMES = colnames(DATA)

# DATA TRANSFORMATION
Y = DATA[-1,]
for (i in 1:k) {
  Y[,i] = 100*diff(log(DATA[,i]))
}

par(mfcol = c(ceiling(k/2), 2), oma = c(1,1,0,0) + 0.1, mar = c(1,0.5,0.5,0) + 1, mgp=c(0, .65, 0))
for (i in 1:k) {
  plot(Y[,i],type="l",las=1,xlab="",ylab="",main=NAMES[i],ylim=c(-max(abs(Y[,i])),max(abs(Y[,i]))),col="steelblue4",xaxs="i")
  grid(NA,NULL)
  lines(Y[,i],col="steelblue4")
  box()
}

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
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npso(dca)
plot_pci(dca)

# DYNAMIC VOLATILITY CONNECTEDNESS
dca = ConnectednessApproach(abs(Y), 
                            model="TVP-VAR",
                            connectedness="Time",
                            nlag=1,
                            nfore=10,
                            window.size=200,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior")))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npso(dca)
plot_pci(dca)
