
### The joint spillover index
### Lastrapes, WD., & Wiesen, TF. (2021)
### Economic Modelling
### replicated by David Gabauer

library("parallel")
library("ConnectednessApproach")
options(mc.cores=detectCores())

data("lw2021")
fit = VAR(lw2021, configuration=list(nlag=2))
LW2021 = JointConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=30)
LW2021$TABLE

dca = ConnectednessApproach(lw2021, 
                            model="VAR",
                            connectedness="Joint",
                            nlag=2,
                            nfore=30,
                            window.size=200)
print(dca$TABLE)

# CONNECTEDNESS PLOTS
plot_tci(dca)
plot_net(dca)
plot_to(dca)
plot_from(dca)
