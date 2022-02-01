
### Dynamic connectedness of uncertainty across developed economies: A time-varying approach
### Antonakakis, N., Gabauer, D., Gupta, R., & Plakandaras, V. (2018).
### Economics Letters
### replicated by David Gabauer

rm(list=c())
library("zoo")
library("openxlsx")
library("parallel")
library("waveslim")
library("ConnectednessApproach")
options(mc.cores=detectCores())

# DATA PREPARATION
RAW = read.xlsx("./data.xlsx", detectDates=TRUE)
RAW = na.omit(RAW)
DATE = as.Date(RAW[,1], "%Y-%m-%d")
DATA = RAW[,-1]
DATA = zoo(DATA, order.by=DATE)
k = ncol(DATA)
NAMES = colnames(DATA)

# DATA TRANSFORMATION
Y = DATA[-1,]
for (i in 1:k) {
  Y[,i] = diff(DATA[,i])
}

levels = 8
X_list = list()
for (i in 1:k) {
  fit = modwt(Y[,i], wf="la8", n.levels=levels)
  X_list[[i]] = matrix(unlist(fit), ncol=levels+1)
}

Y_list = list()
for (i in 1:levels) {
  data = NULL
  for (j in 1:k) {
    data = cbind(data, X_list[[j]][,i])
  }
  colnames(data) = NAMES
  Y_list[[i]] = zoo(data, order.by=index(Y))
}

split = 2
par(mfcol=c(ceiling(levels/split),split), oma=c(0.5,1.5,0,0), mar=c(1.5,1,1.5,2), mgp=c(0.5,0.5,0))
for (i in 1:length(Y_list)) {
  plot(Y_list[[i]][,1], type="l", col=j, las=1, xlab="", ylab="", main=paste("Level", i), xaxs="i", ylim=c(-max(abs(Y_list[[i]])), max(abs(Y_list[[i]]))))
  grid(NA, NULL, lty=3)
  for (j in 1:k) {
    lines(Y_list[[i]][,j], col=j)
  }
  box()
}

# DYNAMIC RETURN CONNECTEDNESS
DCA_list = list()
for (j in 1:levels) {
  dca = ConnectednessApproach(Y_list[[j]], 
                              model="TVP-VAR",
                              connectedness="Time",
                              nlag=1,
                              nfore=10,
                              window.size=250,
                              VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior")))
  print(dca$TABLE)
  path = paste0("./",j,"level")
  plot_tci(dca, save=TRUE, path=path)
  plot_net(dca, save=TRUE, path=path)
  plot_to(dca, save=TRUE, path=path)
  plot_from(dca, save=TRUE, path=path)
  plot_npso(dca, save=TRUE, path=path)
  DCA_list[[j]] = dca
}
