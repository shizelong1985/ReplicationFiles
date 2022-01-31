
### DIEBOLD, FX., AND YILMAZ, K. (2009)
### MEASURING FINANCIAL ASSET RETURN AND VOLATILITY SPILLOVERS, WITH APPLICATION TO GLOBAL EQUITY MARKETS
### Economic Journal
### by David Gabauer

options(mc.cores=detectCores())
library("ConnectednessApproach")
data("dy2009")

### STATIC CONNECTEDNESS
nlag = 2
nfore = 10
fit = VAR(dy2009, configuration=list(nlag=nlag))
DynamicConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=nfore, generalized=FALSE)$TABLE

# DYNAMIC CONNECTEDNESS
dca = ConnectednessApproach(dy2009, 
                            model="VAR",
                            connectedness="Time",
                            nlag=nlag,
                            nfore=nfore,
                            window.size=200,
                            Connectedness_config=list(TimeConnectedness=list(generalized=FALSE)))

# CONNECTEDNESS PLOTS
plot_tci(dca, save=TRUE)
plot_net(dca, save=TRUE)
plot_to(dca, save=TRUE)
plot_from(dca, save=TRUE)
plot_npso(dca, save=TRUE)
