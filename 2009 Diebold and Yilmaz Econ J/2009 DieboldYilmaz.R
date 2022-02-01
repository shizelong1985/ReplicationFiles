
### Measuring financial asset return and volatility spillovers, with application to global equity markets
### Diebold, FX., & Yilmaz, K. (2009)
### Economic Journal
### by David Gabauer

library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())
data("dy2009")

### INITILISATION
nlag = 2
nfore = 10
window.size = 200
generalized = FALSE

### STATIC CONNECTEDNESS
fit = VAR(dy2009, configuration=list(nlag=nlag))
DynamicConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=nfore, generalized=generalized)$TABLE

# DYNAMIC CONNECTEDNESS
dca = ConnectednessApproach(dy2009, 
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
