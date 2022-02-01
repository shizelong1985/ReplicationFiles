
### Crude oil and Islamic sectoral stocks: Asymmetric connectedness and investment strategies.
### Adekoya, O. B., Akinseye, A., Antonakakis, N., Chatziantoniou, I., Gabauer, D., & Oliyide, J. A.
### replicated by David Gabauer

rm(list=ls())
library("zoo")
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())

data("aaacgj2022")
DATA = aaacgj2022
k = ncol(DATA)
NAMES = colnames(DATA)

par(mfrow=c(1,1), oma=c(1.0,1.5,0.5,0)+0.1, mar=c(1,1,1,0)+.5, mgp=c(3, 0.5, 0))
plot(scale(DATA[,1],TRUE,TRUE), xlab="",ylab="",type="l", las=1,tck=-0.02,yaxs="i", ylim=c(-4, 6), main="", col="black", xaxs="i", tck=-0.015)
grid(NA, NULL)
for (i in 1:k) {
  lines(scale(DATA[,i],TRUE,TRUE), xlab="",ylab="", col=i)
}
box()
legend("top", xpd=TRUE, NAMES, fill=c(1:k), bty='n', cex=1, ncol=5)

Y = Yp = Yn = DATA[-1,]
for (i in 1:k) {
  x = embed(as.numeric(DATA[,i]),2)
  Y[,i] = Yp[,i] = Yn[,i] = 100*(x[,1]-x[,2])/x[,2]
  Yp[which(Y[,i]<0),i] = 0
  Yn[which(Y[,i]>0),i] = 0
}

split = 2
par(mfrow=c(ceiling(k/split),split), oma=c(1.0,1.5,0.5,0)+0.1, mar=c(1,1,1,0)+.5, mgp=c(3, 0.5, 0))
for (i in 1:k) {
  plot(Y[,i], xlab="",ylab="",type="l", las=1,tck=-0.02, ylim=c(-10,10), main=NAMES[i], col="black", xaxs="i", tck=-0.015)
  grid(NA, NULL)
  lines(Y[,i], xlab="",ylab="", col=2)
  box()
}
SummaryStatistics(Y)
Y_list = list(Y, Yp, Yn)

# ASYMMETRIC TVP-VAR CONNECTEDNESS APPROACH
DCA_list = list()
spec = c("all", "positive", "negative")
for (i in 1:length(Y_list)) {
  dca = ConnectednessApproach(Y_list[[i]], 
                              model="TVP-VAR",
                              connectedness="Time",
                              nlag=1,
                              nfore=10,
                              window.size=200,
                              VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
  DCA_list[[i]] = dca
  
  # CONNECTEDNESS PLOTS
  path = paste0("./", spec[i])
  plot_tci(dca, path=path, save=TRUE)
  plot_net(dca, path=path, save=TRUE)
  plot_from(dca, path=path, save=TRUE)
  plot_to(dca, path=path, save=TRUE)
  print(dca$TABLE)
}

date = as.Date(rownames(DCA_list[[1]]$TCI))
par(mfrow=c(1,1))
plot(date, DCA_list[[1]]$TCI[,1,1],type="l",ylim=c(50,100), las=1, xaxs="i")
lines(date, DCA_list[[2]]$TCI[,1,1], col=2)
lines(date, DCA_list[[3]]$TCI[,1,1], col=3)

plot(date, DCA_list[[3]]$TCI[,1,1]-DCA_list[[2]]$TCI[,1,1],type="l", las=1, xaxs="i")
