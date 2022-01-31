
library("zoo")
library("openxlsx")
library("parallel")
library("waveslim")
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
  Y[,i] = diff(DATA[,i])
}

waves = 8
Y_list = list()
for (i in 1:k) {
  fit = modwt(Y[,i], wf="la8", n.levels=waves)
  Y_list[[i]] = matrix(unlist(fit), ncol=waves+1)
}

Z_list = list()
for (i in 1:waves) {
  data = NULL
  for (j in 1:k) {
    data = cbind(data, Y_list[[j]][,i])
  }
  colnames(data) = NAMES
  Z_list[[i]] = zoo(data, order.by=index(Y))
}

split = 2
par(mfcol=c(ceiling(waves/split),split), oma=c(0.5,1.5,0,0), mar=c(1.5,1,1.5,2), mgp=c(0.5,0.5,0))
for (i in 1:length(Z_list)) {
  plot(Z_list[[i]][,1], type="l", col=j, las=1, xlab="", ylab="", main=paste("Level", i), xaxs="i", ylim=c(-max(abs(Z_list[[i]])), max(abs(Z_list[[i]]))))
  grid(NA, NULL, lty=3)
  for (j in 1:ncol(Z_list[[i]])) {
    lines(Z_list[[i]][,j], col=j)
  }
  box()
}

# DYNAMIC RETURN CONNECTEDNESS
DCA_list = list()
for (j in 1:length(Z_list)) {
  dca = ConnectednessApproach(Z_list[[j]], 
                              model="TVP-VAR",
                              connectedness="Time",
                              nlag=1,
                              nfore=10,
                              window.size=250,
                              VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior")))
  print(dca$TABLE)
  DCA_list[[j]] = dca
}


