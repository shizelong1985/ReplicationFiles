
### Dynamic spillovers across precious metals and energy realized volatilities: Evidence from quantile extended joint connectedness measures
### CUNADO, J., CHATZIANTONIOU, GABAUER, D., GARCIA DE PEREZ, F., & HARDIK, M.
### replicated by David Gabauer

rm(list=ls())
library("zoo")
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())

data(ccggh2022)
Y = ccggh2022[,c(1,2,5,6)]
k = ncol(Y)
NAMES = colnames(Y)

par(mfcol = c(ceiling(k/2), 2), oma = c(1,1,0,0) + 0.1, mar = c(1,0.5,0.5,0) + 1, mgp=c(0, .65, 0))
for (i in 1:k) {
  plot(Y[,i],type="l",las=1,xlab="",ylab="",main=NAMES[i],ylim=c(0,1.5),col="steelblue4",xaxs="i")
  grid(NA,NULL)
  lines(Y[,i],col="steelblue4")
  box()
}
print(SummaryStatistics(Y))

# DYNAMIC RETURN CONNECTEDNESS
tau = 0.50
dca = ConnectednessApproach(Y,
                            model="QVAR",
                            connectedness="Extended Joint",
                            nlag=1,
                            nfore=20,
                            window.size=250,
                            VAR_config=list(QVAR=list(tau=tau)))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npso(dca)
plot_pci(dca)
