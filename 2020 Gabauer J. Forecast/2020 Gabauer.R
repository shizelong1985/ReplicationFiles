
### GABAUER, D. (2020)
### Crude Oil Futures Contracts and Commodity Markets: New Evidence from a TVP-VAR Extended Joint Connectedness Approach
### JOURNAL OF FORECASTING
### replicated by David Gabauer

library("rmgarch")
library("ConnectednessApproach")
data("g2020")

# DCC-GARCH CONNECTEDNESS APPROACH
nfore = 100
k = ncol(g2020)
garch11.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                          variance.model=list(garchOrder=c(1,1), model="sGARCH"), 
                          distribution.model="norm")
dcc.garch11.spec = dccspec(uspec=multispec(replicate(k, garch11.spec)),
                           dccOrder=c(1,1), distribution="mvnorm")
fit = dccfit(dcc.garch11.spec, data=g2020)
dca = VFEVD(fit, nfore, standardize=FALSE) 
virf = dca$GVIRF
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_from(dca)
plot_to(dca)
plot_npso(dca)
