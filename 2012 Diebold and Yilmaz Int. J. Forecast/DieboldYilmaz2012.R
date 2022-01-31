### DIEBOLD, FX., AND YILMAZ, K. (2012)
### BETTER TO GIVE THAN TO RECEIVE: PREDICTIVE DIRECTIONAL MEASUREMENT OF VOLATILITY SPILLOVERS
### International Journal of Forecasting
### replicated by David Gabauer

library("ConnectednessApproach")
data("dy2012")

# STATIC CONNECTEDNESS
nlag = 4
nfore = 10
fit = VAR(dy2012, configuration=list(nlag=nlag))
DynamicConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=nfore, generalized=TRUE)$TABLE

# DYNAMIC CONNECTEDNESS
dca = ConnectednessApproach(dy2012, 
                            model="VAR",
                            connectedness="Time",
                            nlag=nlag,
                            nfore=nfore,
                            window.size=200,
                            Connectedness_config=list(TimeConnectedness=list(generalized=TRUE)))

# CONNECTEDNESS PLOTS
plot_tci(dca, save=TRUE)
plot_net(dca, save=TRUE)
plot_to(dca, save=TRUE)
plot_from(dca, save=TRUE)
plot_npso(dca, save=TRUE)
