
### Better to give than to receive: Predictive directional measurement of volatility spillovers
### Diebold, FX., & Yilmaz, K. (2012)
### International Journal of Forecasting
### replicated by David Gabauer

rm(list=c())
library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("dy2012")

### INITILISATION
nlag = 4
nfore = 10
window.size = 200
generalized = TRUE

# STATIC CONNECTEDNESS
fit = VAR(dy2012, configuration=list(nlag=nlag))
DynamicConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=nfore, generalized=generalized)$TABLE

# DYNAMIC CONNECTEDNESS
dca = ConnectednessApproach(dy2012, 
                            model="VAR",
                            connectedness="Time",
                            nlag=nlag,
                            nfore=nfore,
                            window.size=window.size,
                            Connectedness_config=list(TimeConnectedness=list(generalized=generalized)))
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
plot_npdc(dca)
plot_pci(dca)
plot_npso(dca)
plot_influence(dca)
