
### Interest Rate Swaps and the Transmission Mechanism of Monetary Policy:
### A Quantile Connectedness Approach
### Chatziantoniou, I., Gabauer, D., & Stenfors, A. (2021)
### Economics Letters
### by David Gabauer

rm(list=ls())
library("zoo")
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("cgs2021")

# SUMMARY STATISTICS
Y = diff(cgs2021)
print(SummaryStatistics(Y))

# INITIALISATION
nlag = 1
tau = 0.5
nfore = 20
window.size = 200

# DYNAMIC QUANTILE CONNECTEDNESS
dca = ConnectednessApproach(Y,
                            model="QVAR",
                            connectedness="Time",
                            nlag=nlag,
                            nfore=nfore,
                            window.size=window.size,
                            VAR_config=list(QVAR=list(tau=tau)))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npso(dca)


# DYNAMIC TOTAL & NET TOTAL DIRECTIONAL CONNECTEDNESS OVER QUANTILES - more advanced
# in the paper we have used n=100 - 100 quantile VARs are estimated - 
# however this estimation takes a lot of time which is why we downsampled it to 10
# if you want a single quantile VAR just exchange the seq() to the tau you want
k = ncol(Y)
t = nrow(dca$TCI)
date = index(Y)
NAMES = colnames(Y)

n = 10
quantiles = seq(0.05, 0.95, 1/n) 
print(quantiles)

path_name = "./Results/results.RData"
if (file.exists(path_name)) {
  load(path_name)
} else {
  DCA_list = list()
  TCI = array(NA, c(t, length(quantiles)), dimnames=list(as.character(tail(date,t)), quantiles))
  NET = array(NA, c(t, k, length(quantiles)), dimnames=list(as.character(tail(date,t)), NAMES, quantiles))
  for (j in 1:length(quantiles)) {
    dca = ConnectednessApproach(Y,
                                model="QVAR",
                                connectedness="Time",
                                nlag=nlag,
                                nfore=nfore,
                                window.size=window.size,
                                VAR_config=list(QVAR=list(tau=quantiles[j])))
    TCI[,j] = dca$TCI[,1,1]
    NET[,,j] = dca$NET
    DCA_list = c(DCA_list, list(dca$TABLE))
    print(quantiles[j])
  }
  names(DCA_list) = quantiles
  save.image(path_name)
}

# CONNECTEDNESS PLOTS
nlevels = 20
threshold = 0.01 # get rid of outliers
filled.contour(tail(date,t), quantiles, TCI, xlab="", ylab="", ylim=c(0,1))
for (i in 1:k) {
  net = NET[,i,,drop=FALSE]
  net[which(net<quantile(net, threshold), arr.ind=TRUE)] = as.numeric(quantile(net, threshold))
  net[which(net>quantile(net, 1-threshold), arr.ind=TRUE)] = as.numeric(quantile(net, 1-threshold))
  
  lvls = seq(-ceiling(max(abs(net))), ceiling(max(abs(net))), length.out=nlevels)
  color.palette = function(n) {hcl.colors(n, "RdBu", rev=TRUE)}
  quantiles = as.numeric(dimnames(net)[[3]])
  filled.contour(tail(date,t), quantiles, as.matrix(net[,1,]),
                 col=color.palette(nlevels-1), levels=lvls, main=colnames(net), ylim=c(0,1))
  
}
