
### RETURN CONNECTEDNESS ACROSS ASSET CLASSES AROUND THE COVID-19 OUTBREAK
### BOURI, E., CEPNI, O., GABAUER, D., & GUPTA, R.
### INTERNATIONAL REVIEW OF FINANCIAL ANALYIS
### replicated by David Gabauer

library("zoo")
library("openxlsx")
library("ConnectednessApproach")

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

par(mfcol = c(k, 1), oma = c(1,1,0,0) + 0.1, mar = c(1,0.5,0.5,0) + 1, mgp=c(0, .65, 0))
for (i in 1:k) {
   plot(date,Y[,i],type="l",las=1,xlab="",ylab="",main=NAMES[i],ylim=c(-max(abs(Y[,i])),max(abs(Y[,i]))),col="steelblue4",xaxs="i")
   grid(NA,NULL)
   lines(date,Y[,i],col="steelblue4")
   box()
}

# DYNAMIC CONNECTEDNESS
dca = ConnectednessApproach(Y, 
                            model="TVP-VAR",
                            connectedness="Time",
                            nlag=1,
                            nfore=20,
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1)))
ind = which(date=="2020-01-13")
ConnectednessTable(dca$FEVD[,,c(1:ind)])$TABLE
ConnectednessTable(dca$FEVD[,,-c(1:ind)])$TABLE

# CONNECTEDNESS PLOTS
plot_tci(dca, save=TRUE)
plot_net(dca, save=TRUE)
plot_to(dca, save=TRUE)
plot_from(dca, save=TRUE)
plot_npso(dca, save=TRUE)
