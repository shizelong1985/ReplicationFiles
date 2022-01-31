
### CHATZIANTONIOU, I., GABAUER, D., & STENFORS, A. (2021)
### Interest Rate Swaps and the Transmission Mechanism of Monetary Policy:
### A Quantile Connectedness Approach
### ECONOMICS LETTERS
### by David Gabauer

options(mc.cores=detectCores())
library("zoo")
library("ConnectednessApproach")
data("cgs2021")

net.contour = function(date, net, nlevels=20, threshold=0.01) {
  # get rid of outliers
  net[which(net<quantile(net, threshold), arr.ind=TRUE)] = as.numeric(quantile(net, threshold))
  net[which(net>quantile(net, 1-threshold), arr.ind=TRUE)] = as.numeric(quantile(net, 1-threshold))
  
  lvls = seq(-ceiling(max(abs(net))), ceiling(max(abs(net))), length.out=nlevels)
  color.palette = function(n) {hcl.colors(n, "RdBu", rev=TRUE)}
  quantiles = as.numeric(dimnames(net)[[3]])
  filled.contour(date, quantiles, as.matrix(net[,1,]),
                 col=color.palette(nlevels-1), levels=lvls, main=colnames(net), ylim=c(0,1))
}


# SUMMARY STATISTICS
print(SummaryStatistics(cgs2021))

# QUANTILE VAR CONNECTEDNESS APPROACH
nlag = 1
nfore = 20
window.size = 200

# DYNAMIC QUANTILE CONNECTEDNESS
dca = ConnectednessApproach(cgs2021,
                            model="QVAR",
                            connectedness="Time",
                            nlag=nlag,
                            nfore=nfore,
                            window.size=window.size,
                            VAR_config=list(QVAR=list(tau=0.5)))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca, save=TRUE)
plot_net(dca, save=TRUE)
plot_to(dca, save=TRUE)
plot_from(dca, save=TRUE)
plot_npso(dca, save=TRUE)


# DYNAMIC CONNECTEDNESS OVER QUANTILES
# in the paper we have used n=100 - 100 quantile VARs are estimated - 
# however this estimation takes a lot of time which is why we downsampled it to 10
# if you want a single quantile VAR just exchange the seq() to the tau you want
k = ncol(cgs2021)
t = length(dca$TOTAL)
date = index(cgs2021)
NAMES = colnames(cgs2021)

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
    dca = ConnectednessApproach(cgs2021,
                                model="QVAR",
                                connectedness="Time",
                                nlag=nlag,
                                nfore=nfore,
                                window.size=window.size,
                                VAR_config=list(QVAR=list(tau=quantiles[j])))
    TCI[,j] = dca$cTOTAL
    NET[,,j] = dca$NET
    DCA_list = c(DCA_list, list(dca$TABLE))
    print(quantiles[j])
  }
  names(DCA_list) = quantiles
  save.image(path_name)
}

# FIGURE 2: 1-Year Dynamic Total Connectedness
dev.off()
filled.contour(tail(date,t), quantiles, TCI, xlab="", ylab="", ylim=c(0,1))

# FIGURE 3-6: 1-Year Net Total Directional Connectedness (.)
for (i in 1:k) {
  net.contour(tail(date,t), net=NET[,i,,drop=FALSE])
}
